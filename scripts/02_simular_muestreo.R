# =============================================================================
# 02_simular_muestreo.R
# Simula la base de datos "tipo" del muestreo de visón (Neogale vison) en
# riberas de la cuenca del Toltén (Villarrica, Pucón, Curarrehue).
#
# Diseño simulado:
#   - 50 sitios ubicados al azar sobre la red hídrica, separados >= 4 km
#   - 1 transecto de 3 km de ribera por sitio (1,5 km aguas arriba y abajo)
#   - 1 cámara trampa por sitio, en la ribera, activa ~3 meses
#   - 4 visitas por sitio en una temporada (nov 2026 - feb 2027)
#   - 4 métodos por visita: huellas, fecas, cámara trampa, avistamiento
#     => 50 x 4 x 4 = 800 filas, UNA tabla (una fila = sitio x visita x método)
#
# La presencia del visón se simula con un modelo de ocupación con parámetros
# conocidos (ver data/simulacion/parametros_simulacion.csv): la ocupación
# aumenta con la cercanía (por el río) a centros productivos, con un efecto
# mayor para pisciculturas. La detección es imperfecta y depende del método.
#
# Al final se "ensucia" la tabla con errores típicos de digitación para
# producir el archivo crudo que limpia 03_limpiar_datos.R.
#
# Entradas : data/intermedio/capas_base.gpkg, data/raw/centros_productivos.csv,
#            data/raw/espacial/poblados_referencia.csv, docs/diccionario_datos.csv
# Salidas  : data/raw/vison_muestreo_raw.csv
#            data/intermedio/diseno_muestreo.gpkg (sitios, transectos)
#            data/simulacion/estado_real_sitios.csv
#            data/simulacion/parametros_simulacion.csv
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(igraph)
})

set.seed(20261020)
crs_utm <- 32719
source("scripts/funciones/geometria.R")

# --- Parámetros del diseño -----------------------------------------------------
N_SITIOS        <- 50
LARGO_TRANSECTO <- 3000      # m
SEP_MIN_SITIOS  <- 4000      # m, distancia mínima entre cámaras
N_VISITAS       <- 4
INICIO_TEMPORADA <- as.Date("2026-11-02")
DIAS_ENTRE_VISITAS <- 30
METODOS <- c("huellas", "fecas", "camara_trampa", "avistamiento")

# Cupos mínimos de sitios en los ríos principales (el resto se reparte al azar
# en proporción a la longitud de cada cauce con nombre)
CUPOS <- c(tolten = 6, pucon = 6, trancura = 6, liucura = 6)   # "pucon" = Río Pucón o Minetue

# --- Lectura -------------------------------------------------------------------
capas    <- "data/intermedio/capas_base.gpkg"
area     <- st_read(capas, layer = "area_estudio",  quiet = TRUE)
comunas  <- st_read(capas, layer = "comunas",       quiet = TRUE)
ssc      <- st_read(capas, layer = "subsubcuencas", quiet = TRUE)
rios     <- st_read(capas, layer = "rios",          quiet = TRUE)
lagos    <- st_read(capas, layer = "lagos",         quiet = TRUE)

centros <- read_csv("data/raw/centros_productivos.csv", show_col_types = FALSE) |>
  st_as_sf(coords = c("utm_este", "utm_norte"), crs = crs_utm, remove = FALSE)

poblados <- read_csv("data/raw/espacial/poblados_referencia.csv", show_col_types = FALSE) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_utm)

dicc <- read_csv("docs/diccionario_datos.csv", show_col_types = FALSE)

normalizar <- function(x) {
  tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
}

# Orden de Strahler: se usa el de la capa DGA (3 a 8 en el área); si faltara,
# se asigna uno aproximado según el nombre del cauce
asignar_strahler <- function(nombre, strahler) {
  n <- normalizar(nombre)
  case_when(
    !is.na(strahler)                 ~ as.integer(strahler),
    str_detect(n, "tolten|pucon")    ~ 8L,
    str_detect(n, "liucura")         ~ 7L,
    str_detect(n, "trancura|maichin") ~ 5L,
    str_detect(n, "^rio")            ~ 4L,
    TRUE                             ~ 3L
  )
}
rios <- rios |> mutate(strahler = asignar_strahler(nombre_rio, strahler))

# =============================================================================
# 1. Red hídrica como grafo (para distancias a lo largo del río)
# =============================================================================
red <- st_union(st_geometry(rios)) |> st_node() |> st_cast("LINESTRING")
red_xy <- lapply(red, function(l) unclass(l)[, 1:2, drop = FALSE])
red_largo <- vapply(red_xy, function(m) sum(sqrt(rowSums(diff(m)^2))), numeric(1))
clave <- function(p) sprintf("%.1f_%.1f", p[1], p[2])
nodo_ini <- vapply(red_xy, function(m) clave(m[1, ]), character(1))
nodo_fin <- vapply(red_xy, function(m) clave(m[nrow(m), ]), character(1))

grafo <- graph_from_data_frame(
  data.frame(from = nodo_ini, to = nodo_fin, weight = red_largo),
  directed = FALSE
)
# --- Conexión a través de los lagos ---
# Los tributarios desembocan en los lagos (Villarrica, Caburgua, ...) y otros
# ríos nacen de ellos (Toltén, Blanco), pero la red DGA no tiene líneas dentro
# de los lagos. Para cada lago se agregan conexiones virtuales en línea recta
# entre todos los extremos de la red que tocan su orilla (a <= 300 m).
# (Las líneas DGA están digitalizadas de aguas arriba hacia aguas abajo.)
rios_xy <- lapply(st_geometry(rios), function(l) unclass(l)[, 1:2, drop = FALSE])
ini_lineas <- vapply(rios_xy, function(m) clave(m[1, ]), character(1))
fin_lineas <- vapply(rios_xy, function(m) clave(m[nrow(m), ]), character(1))
grado <- degree(grafo)
terminales <- names(grado)[grado == 1]
xy_term <- do.call(rbind, lapply(strsplit(terminales, "_"), as.numeric))
lagos_red <- lagos |> filter(area_km2 >= 0.5)
cerca <- st_is_within_distance(lagos_red, a_sf(xy_term), dist = 300)

conexiones_lago <- do.call(rbind, lapply(seq_along(cerca), function(k) {
  idx <- cerca[[k]]
  if (length(idx) < 2) return(NULL)
  pares <- t(combn(idx, 2))
  data.frame(from = terminales[pares[, 1]], to = terminales[pares[, 2]],
             weight = sqrt(rowSums((xy_term[pares[, 1], , drop = FALSE] - xy_term[pares[, 2], , drop = FALSE])^2)),
             lago = lagos_red$nombre_lago[k])
}))
nacimiento_tolten <- intersect(terminales,
                               ini_lineas[str_detect(normalizar(rios$nombre_rio), "tolten")])
if (!any(c(conexiones_lago$from, conexiones_lago$to) %in% nacimiento_tolten))
  warning("El nacimiento del Toltén no quedó conectado al Lago Villarrica")
grafo <- add_edges(grafo, t(as.matrix(conexiones_lago[, c("from", "to")])),
                   weight = conexiones_lago$weight)
nodos_lago <- unique(c(conexiones_lago$from, conexiones_lago$to))

message("Red hídrica: ", length(red), " segmentos, ", length(nodos_lago),
        " extremos conectados a través de ", length(unique(conexiones_lago$lago)), " lagos, ",
        components(grafo)$no, " componente(s) conectada(s)")

# Ubica puntos sobre la red: segmento más cercano y distancia a lo largo de él
ubicar_en_red <- function(pts) {
  e <- st_nearest_feature(pts, red)
  xy <- st_coordinates(pts)[, 1:2, drop = FALSE]
  d <- vapply(seq_along(e), function(i)
    proyectar_en_linea(red_xy[[e[i]]], xy[i, ])$dist_a_lo_largo, numeric(1))
  data.frame(e = e, d = d)
}

# Matriz de distancias por la red entre dos conjuntos de puntos ubicados
dist_por_red <- function(A, B) {
  vA <- unique(c(nodo_ini[A$e], nodo_fin[A$e]))
  vB <- unique(c(nodo_ini[B$e], nodo_fin[B$e]))
  D <- distances(grafo, v = vA, to = vB, weights = E(grafo)$weight)
  out <- matrix(NA_real_, nrow(A), nrow(B))
  for (i in seq_len(nrow(A))) {
    ea <- A$e[i]; a0 <- A$d[i]; a1 <- red_largo[ea] - A$d[i]
    for (j in seq_len(nrow(B))) {
      eb <- B$e[j]; b0 <- B$d[j]; b1 <- red_largo[eb] - B$d[j]
      if (ea == eb) { out[i, j] <- abs(a0 - b0); next }
      out[i, j] <- min(
        a0 + D[nodo_ini[ea], nodo_ini[eb]] + b0,
        a0 + D[nodo_ini[ea], nodo_fin[eb]] + b1,
        a1 + D[nodo_fin[ea], nodo_ini[eb]] + b0,
        a1 + D[nodo_fin[ea], nodo_fin[eb]] + b1)
    }
  }
  out[is.infinite(out)] <- NA
  out
}

# =============================================================================
# 2. Ubicación aleatoria de sitios y transectos
# =============================================================================
# Cauces con nombre, unidos por nombre, como líneas continuas candidatas
# (solo la parte de la red dentro del área de estudio + 500 m)
candidatas <- rios |>
  filter(!is.na(nombre_rio), en_area) |>
  st_intersection(st_buffer(st_geometry(area), 500)) |>
  st_difference(st_union(st_buffer(st_geometry(lagos), 50))) |>   # fuera de lagos
  group_by(nombre_rio) |>
  summarise(strahler = max(strahler), .groups = "drop") |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  st_cast("MULTILINESTRING") |>
  st_cast("LINESTRING", warn = FALSE) |>
  mutate(largo = as.numeric(st_length(geom)),
         clave_rio = normalizar(nombre_rio)) |>
  filter(largo >= LARGO_TRANSECTO + 200)

cand_xy <- lapply(st_geometry(candidatas), function(l) unclass(l)[, 1:2, drop = FALSE])

sitios_xy  <- matrix(numeric(0), ncol = 2)
sitios_lin <- integer(0)
sitios_pos <- numeric(0)

# Agrega n sitios al azar sobre las líneas 'idx' respetando la separación mínima
agregar_sitios <- function(idx, n) {
  if (length(idx) == 0 || n == 0) return(invisible(0))
  utiles <- candidatas$largo[idx] - LARGO_TRANSECTO
  agregados <- 0; intentos <- 0
  while (agregados < n && intentos < 20000) {
    intentos <- intentos + 1
    k <- if (length(idx) == 1) idx else sample(idx, 1, prob = utiles)
    pos <- runif(1, LARGO_TRANSECTO / 2, candidatas$largo[k] - LARGO_TRANSECTO / 2)
    p <- punto_a_distancia(cand_xy[[k]], pos)$punto
    dmin <- if (nrow(sitios_xy) == 0) Inf else
      min(sqrt((sitios_xy[, 1] - p[1])^2 + (sitios_xy[, 2] - p[2])^2))
    if (dmin >= SEP_MIN_SITIOS) {
      sitios_xy  <<- rbind(sitios_xy, p)
      sitios_lin <<- c(sitios_lin, k)
      sitios_pos <<- c(sitios_pos, pos)
      agregados <- agregados + 1
    }
  }
  if (agregados < n) warning("Solo se ubicaron ", agregados, " de ", n, " sitios")
  invisible(agregados)
}

# 2a. Cupos en ríos principales
for (r in names(CUPOS)) {
  idx <- which(str_detect(candidatas$clave_rio, r))
  if (length(idx) == 0) {
    message("  (sin cauce que coincida con '", r, "'; se omite su cupo)")
    next
  }
  agregar_sitios(idx, CUPOS[[r]])
}
# 2b. Resto al azar en toda la red con nombre
agregar_sitios(seq_len(nrow(candidatas)), N_SITIOS - nrow(sitios_xy))
stopifnot(nrow(sitios_xy) == N_SITIOS)

# Orden de los sitios de oeste a este (S01 = más al oeste)
ord <- order(sitios_xy[, 1])
sitios_xy <- sitios_xy[ord, ]; sitios_lin <- sitios_lin[ord]; sitios_pos <- sitios_pos[ord]

id_sitio <- sprintf("S%02d", seq_len(N_SITIOS))

# Atributos del cauce de cada sitio (orden de Strahler del tramo DGA más cercano)
strahler_sitio <- rios$strahler[st_nearest_feature(a_sf(sitios_xy), rios)]
ancho_base <- c(1, 2, 4, 8, 15, 30, 60, 120)          # m, por orden de Strahler 1-8
ancho_cauce <- round(ancho_base[strahler_sitio] * exp(rnorm(N_SITIOS, 0, 0.25)), 1)

# Transectos (eje del cauce) y cámara desplazada hacia la ribera
transectos <- vector("list", N_SITIOS)
camara_xy <- matrix(NA_real_, N_SITIOS, 2)
for (i in seq_len(N_SITIOS)) {
  xy <- cand_xy[[sitios_lin[i]]]
  transectos[[i]] <- st_linestring(subcadena(xy, sitios_pos[i] - LARGO_TRANSECTO / 2,
                                                 sitios_pos[i] + LARGO_TRANSECTO / 2))
  pt <- punto_a_distancia(xy, sitios_pos[i])
  normal <- c(-pt$tangente[2], pt$tangente[1])
  lado <- sample(c(-1, 1), 1)
  desplaz <- ancho_cauce[i] / 2 + runif(1, 3, 15)     # cámara a 3-15 m del borde del agua
  camara_xy[i, ] <- pt$punto + lado * desplaz * normal
}
transectos <- st_sfc(transectos, crs = crs_utm)
ext_ini <- t(vapply(transectos, function(l) unclass(l)[1, 1:2], numeric(2)))
ext_fin <- t(vapply(transectos, function(l) { m <- unclass(l); m[nrow(m), 1:2] }, numeric(2)))

sitios <- a_sf(camara_xy) |>
  mutate(id_sitio = id_sitio,
         id_transecto = sprintf("T%02d", seq_len(N_SITIOS)),
         id_camara = sprintf("CT-%02d", seq_len(N_SITIOS)),
         rio = candidatas$nombre_rio[sitios_lin],
         orden_strahler = strahler_sitio,
         ancho_cauce_m = ancho_cauce,
         utm_este = round(camara_xy[, 1]), utm_norte = round(camara_xy[, 2]),
         utm_este_ini_transecto = round(ext_ini[, 1]), utm_norte_ini_transecto = round(ext_ini[, 2]),
         utm_este_fin_transecto = round(ext_fin[, 1]), utm_norte_fin_transecto = round(ext_fin[, 2]))

# Comuna y subsubcuenca (la más cercana si la cámara quedó justo fuera de un límite)
sitios <- sitios |>
  st_join(comunas |> select(comuna), join = st_nearest_feature) |>
  st_join(ssc |> select(cod_cuenca, cod_subcuenca, cod_subsubcuenca, nom_subsubcuenca),
          join = st_nearest_feature) |>
  mutate(nom_cuenca = "Río Toltén")

# =============================================================================
# 3. Distancias a centros productivos
# =============================================================================
D_euc <- st_distance(sitios, centros) |> units::drop_units()

# Para la distancia por el río se usa el punto del eje del cauce en el centro
# del transecto (la cámara está a pocos metros en la ribera)
ub_sitios  <- ubicar_en_red(a_sf(sitios_xy))
ub_centros <- ubicar_en_red(centros)
D_rio <- dist_por_red(ub_sitios, ub_centros)

min_por_tipo <- function(D, tipo) {
  cols <- which(centros$tipo == tipo)
  apply(D[, cols, drop = FALSE], 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
}
mas_cercano <- function(D, tipo = NULL) {
  cols <- if (is.null(tipo)) seq_len(ncol(D)) else which(centros$tipo == tipo)
  cols[apply(D[, cols, drop = FALSE], 1, which.min)]
}

i_pisc <- mas_cercano(D_euc, "piscicultura")
i_gana <- mas_cercano(D_euc, "ganaderia")
i_avic <- mas_cercano(D_euc, "avicola")
i_cual <- mas_cercano(D_euc)

contar <- function(radio, tipo = NULL) {
  cols <- if (is.null(tipo)) seq_len(ncol(D_euc)) else which(centros$tipo == tipo)
  rowSums(D_euc[, cols, drop = FALSE] <= radio)
}

sitios <- sitios |>
  mutate(
    dist_euc_piscicultura_m = round(min_por_tipo(D_euc, "piscicultura")),
    dist_euc_avicola_m      = round(min_por_tipo(D_euc, "avicola")),
    dist_euc_ganaderia_m    = round(min_por_tipo(D_euc, "ganaderia")),
    dist_euc_centro_m       = round(apply(D_euc, 1, min)),
    dist_rio_piscicultura_m = round(min_por_tipo(D_rio, "piscicultura")),
    dist_rio_avicola_m      = round(min_por_tipo(D_rio, "avicola")),
    dist_rio_ganaderia_m    = round(min_por_tipo(D_rio, "ganaderia")),
    dist_rio_centro_m       = round(apply(D_rio, 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))),
    tipo_centro_cercano     = centros$tipo[i_cual],
    id_centro_cercano       = centros$id_centro[i_cual],
    n_centros_500m          = contar(500),
    n_centros_1km           = contar(1000),
    n_centros_3km           = contar(3000),
    dens_centros_3km_km2    = round(n_centros_3km / (pi * 3^2), 3),
    n_pisciculturas_3km     = contar(3000, "piscicultura"),
    n_avicolas_3km          = contar(3000, "avicola"),
    n_ganaderias_3km        = contar(3000, "ganaderia"),
    prod_piscicultura_cercana_ton = centros$produccion_ton_anio[i_pisc],
    cabezas_ganaderia_cercana     = centros$cabezas_ganado[i_gana],
    aves_avicola_cercana          = centros$n_aves[i_avic]
  )

# =============================================================================
# 4. Paisaje (buffer 1 km) -- valores simulados con gradientes plausibles
# =============================================================================
D_pob <- st_distance(sitios, poblados) |> units::drop_units()
x_rel <- (sitios$utm_este - min(sitios$utm_este)) / diff(range(sitios$utm_este))  # 0 = oeste, 1 = este
xy_pucon <- st_coordinates(poblados[normalizar(poblados$poblado) == "pucon", ])
xy_villa <- st_coordinates(poblados[normalizar(poblados$poblado) == "villarrica", ])

dist_poblado <- apply(D_pob, 1, min)

# Proporciones de cobertura: Dirichlet con medias que dependen del gradiente
medias <- cbind(
  bosque     = 15 + 55 * x_rel,
  plantacion = 2 + 20 * (1 - x_rel),
  agricola   = 5 + 45 * exp(-dist_poblado / 8000) * (1 - 0.6 * x_rel),
  urbano     = 0.5 + 25 * exp(-dist_poblado / 1500),
  otros      = 8
)
alfa <- medias / rowSums(medias) * 15
cob <- t(apply(alfa, 1, function(a) { g <- rgamma(length(a), shape = a); g / sum(g) * 100 }))
colnames(cob) <- colnames(medias)
cob <- round(cob, 1)
cob[, "otros"] <- round(100 - rowSums(cob[, 1:4]), 1)

# Altitud aproximada: el lago está a ~230 m; sube hacia la cordillera (este y
# norte de Pucón) y baja suavemente por el Toltén hacia el oeste
xy_s <- st_coordinates(sitios)
km_este_pucon  <- pmax(0, xy_s[, 1] - xy_pucon[1]) / 1000
km_norte_pucon <- pmax(0, xy_s[, 2] - xy_pucon[2]) / 1000
km_oeste_villa <- pmax(0, xy_villa[1] - xy_s[, 1]) / 1000
altitud <- 230 + 4.5 * km_este_pucon + 6 * km_norte_pucon - 1.8 * km_oeste_villa +
  pmax(0, 7 - sitios$orden_strahler) * runif(N_SITIOS, 10, 35) + rnorm(N_SITIOS, 0, 15)

sitios <- sitios |>
  mutate(
    pct_bosque_nativo_1km = cob[, "bosque"],
    pct_plantacion_1km    = cob[, "plantacion"],
    pct_agricola_1km      = cob[, "agricola"],
    pct_urbano_1km        = cob[, "urbano"],
    pct_otros_1km         = cob[, "otros"],
    dist_poblado_m        = round(dist_poblado),
    poblado_cercano       = poblados$poblado[apply(D_pob, 1, which.min)],
    dist_camino_m         = round(rgamma(N_SITIOS, shape = 1.5, rate = 1.5 / (120 + 0.06 * dist_poblado))),
    dens_caminos_1km      = round(0.4 + 3 * exp(-dist_poblado / 4000) * exp(rnorm(N_SITIOS, 0, 0.3)), 2),
    altitud_msnm          = round(pmax(altitud, 150))
  )

# =============================================================================
# 5. Características de la ribera (nivel sitio)
# =============================================================================
p_artif <- 0.05 + 0.25 * exp(-dist_poblado / 2000)
p_inter <- 0.20 + 0.30 * sitios$pct_agricola_1km / 100
tipo_ribera <- vapply(seq_len(N_SITIOS), function(i)
  sample(c("artificial", "intervenida", "natural"), 1,
         prob = c(p_artif[i], p_inter[i], max(0.05, 1 - p_artif[i] - p_inter[i]))), character(1))

media_cob <- c(natural = 0.75, intervenida = 0.45, artificial = 0.20)[tipo_ribera] +
  0.15 * (sitios$pct_bosque_nativo_1km / 100 - 0.4)
media_cob <- pmin(pmax(media_cob, 0.05), 0.95)
cob_veg <- round(100 * rbeta(N_SITIOS, media_cob * 8, (1 - media_cob) * 8), 1)

sust_grande <- c("limo", "arena", "grava", "bolones", "roca")
p_sust_bajo  <- c(0.15, 0.30, 0.30, 0.20, 0.05)   # ríos grandes, zonas bajas
p_sust_alto  <- c(0.05, 0.15, 0.25, 0.35, 0.20)   # tributarios, zonas altas
sustrato <- vapply(seq_len(N_SITIOS), function(i)
  sample(sust_grande, 1, prob = if (sitios$orden_strahler[i] >= 6) p_sust_bajo else p_sust_alto),
  character(1))

sitios <- sitios |>
  mutate(
    tipo_ribera             = tipo_ribera,
    cob_veg_riberena_pct    = cob_veg,
    ancho_franja_riberena_m = round(exp(rnorm(N_SITIOS,
                                log(c(natural = 25, intervenida = 10, artificial = 4)[tipo_ribera]), 0.6)), 1),
    densidad_veg            = cut(pmin(pmax(cob_veg + rnorm(N_SITIOS, 0, 8), 0), 100),
                                  c(-Inf, 40, 70, Inf), labels = c("baja", "media", "alta")) |> as.character(),
    cob_dosel_pct           = round(pmin(pmax(cob_veg * (0.85 - 0.004 * ancho_cauce_m) +
                                               rnorm(N_SITIOS, 0, 8), 0), 100), 1),
    sustrato_ribera         = sustrato,
    pendiente_ribera_grados = round(pmin(rgamma(N_SITIOS, shape = 3,
                                 rate = 3 / ifelse(orden_strahler >= 6, 12, 22)), 70), 1),
    refugio_troncos         = rbinom(N_SITIOS, 1, 0.25 + 0.45 * cob_veg / 100),
    refugio_rocas           = rbinom(N_SITIOS, 1, ifelse(sustrato %in% c("bolones", "roca"), 0.75, 0.25)),
    refugio_madrigueras     = rbinom(N_SITIOS, 1, ifelse(tipo_ribera == "natural", 0.55, 0.20)),
    permanencia_agua        = ifelse(orden_strahler <= 3 & runif(N_SITIOS) < 0.15,
                                     "intermitente", "permanente"),
    conectividad_riberena   = round(plogis(qlogis(pmin(pmax(cob_veg / 100, 0.02), 0.98)) * 0.8 +
                                             rnorm(N_SITIOS, 0, 0.4)), 2)
  )

# =============================================================================
# 6. Estado real de ocupación del visón (modelo con parámetros conocidos)
# =============================================================================
z_std <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
# si un sitio no está conectado por la red con algún centro, se usa la euclidiana
d_pisc <- coalesce(sitios$dist_rio_piscicultura_m, sitios$dist_euc_piscicultura_m)
d_gana <- coalesce(sitios$dist_rio_ganaderia_m,    sitios$dist_euc_ganaderia_m)
d_avic <- coalesce(sitios$dist_rio_avicola_m,      sitios$dist_euc_avicola_m)
n_refugios <- sitios$refugio_troncos + sitios$refugio_rocas + sitios$refugio_madrigueras

beta <- c(intercepto = 0.10,
          cercania_piscicultura = 0.90,   # efecto positivo moderado-fuerte
          cercania_ganaderia    = 0.35,
          cercania_avicola      = 0.25,
          cob_veg_riberena      = 0.50,
          n_refugios            = 0.30,
          altitud               = -0.40,
          pct_urbano            = -0.30)

eta <- beta["intercepto"] +
  beta["cercania_piscicultura"] * z_std(-log(d_pisc + 50)) +
  beta["cercania_ganaderia"]    * z_std(-log(d_gana + 50)) +
  beta["cercania_avicola"]      * z_std(-log(d_avic + 50)) +
  beta["cob_veg_riberena"]      * z_std(sitios$cob_veg_riberena_pct) +
  beta["n_refugios"]            * z_std(n_refugios) +
  beta["altitud"]               * z_std(sitios$altitud_msnm) +
  beta["pct_urbano"]            * z_std(sitios$pct_urbano_1km)
psi <- plogis(eta)
z   <- rbinom(N_SITIOS, 1, psi)

# Otras especies: presencia latente por sitio
psi_nutria <- plogis(-1.3 + 0.8 * z_std(sitios$pct_bosque_nativo_1km) -
                       0.5 * z_std(sitios$pct_urbano_1km) + 0.4 * z_std(sitios$ancho_cauce_m))
psi_zorro  <- rep(0.7, N_SITIOS)
psi_perro  <- plogis(0.5 - 1.0 * z_std(sitios$dist_poblado_m) + 0.5 * z_std(sitios$dens_caminos_1km))
psi_ganado <- plogis(-0.5 + 0.8 * z_std(sitios$pct_agricola_1km) + 0.6 * z_std(sitios$n_ganaderias_3km))
z_nutria <- rbinom(N_SITIOS, 1, psi_nutria)
z_zorro  <- rbinom(N_SITIOS, 1, psi_zorro)
z_perro  <- rbinom(N_SITIOS, 1, psi_perro)
z_ganado <- rbinom(N_SITIOS, 1, psi_ganado)

# =============================================================================
# 7. Visitas: fechas, clima, observadores, caudal
# =============================================================================
# Serie diaria regional de lluvia (cadena de Markov; más lluviosa en noviembre)
dias <- seq(INICIO_TEMPORADA - 20, INICIO_TEMPORADA + 140, by = "day")
# probabilidad diaria de lluvia: ~0,30 a fines de octubre, ~0,07 en febrero (más persistencia)
p_lluvia <- 0.30 - 0.23 * as.numeric(dias - min(dias)) / length(dias)
llueve <- logical(length(dias)); llueve[1] <- runif(1) < p_lluvia[1]
for (t in 2:length(dias)) {
  p <- if (llueve[t - 1]) min(p_lluvia[t] + 0.30, 0.9) else p_lluvia[t]
  llueve[t] <- runif(1) < p
}
lluvia_dia <- ifelse(llueve, rgamma(length(dias), shape = 0.8, scale = 11), 0)

observadores <- tibble(observador = c("obs_A", "obs_B", "obs_C"),
                       experiencia_obs_anios = c(6, 2.5, 0.5))

# Grupos de terreno: 5 sitios por día, de oeste a este
grupo_dia <- ceiling(seq_len(N_SITIOS) / 5) - 1

visitas <- expand_grid(i = seq_len(N_SITIOS), n_visita = seq_len(N_VISITAS)) |>
  mutate(
    id_sitio  = id_sitio[i],
    id_visita = paste0(id_sitio, "-V", n_visita),
    fecha     = INICIO_TEMPORADA + DIAS_ENTRE_VISITAS * (n_visita - 1) + grupo_dia[i] +
                sample(-1:1, n(), replace = TRUE),
    hora_dec  = runif(n(), 8.5, 15.5),
    hora      = sprintf("%02d:%02d", floor(hora_dec), round((hora_dec %% 1) * 59)),
    observador = sample(observadores$observador, n(), replace = TRUE, prob = c(0.40, 0.35, 0.25))
  ) |>
  left_join(observadores, by = "observador") |>
  group_by(i) |>
  mutate(fecha_inicio_camara = lag(fecha),
         dias_periodo = as.numeric(fecha - lag(fecha))) |>
  ungroup()

# Clima por visita
mult_lluvia <- exp(rnorm(N_SITIOS, 0, 0.2)) * (1 + 0.5 * x_rel)   # más lluvia hacia la cordillera
idx_dia <- match(visitas$fecha, dias)
visitas <- visitas |>
  mutate(
    lluvia_48h_mm = round((lluvia_dia[idx_dia - 1] + lluvia_dia[idx_dia - 2]) * mult_lluvia[i], 1),
    dias_desde_lluvia = vapply(idx_dia, function(k) {
      previos <- which(lluvia_dia[1:(k - 1)] >= 1)
      if (length(previos) == 0) 20L else as.integer(k - max(previos))
    }, integer(1)),
    temperatura_c = round(12 + 6 * as.numeric(fecha - INICIO_TEMPORADA) / 100 +
                            0.8 * (hora_dec - 8.5) -
                            0.0065 * (sitios$altitud_msnm[i] - 230) -
                            0.15 * lluvia_48h_mm / 10 + rnorm(n(), 0, 2.5), 1)
  )

# Caudal: base según tamaño del cauce x estacionalidad (deshielo nov-dic) x lluvia
caudal_base <- c(0.02, 0.1, 0.5, 2, 8, 25, 80, 250)[sitios$orden_strahler] *   # m3/s por orden
  exp(rnorm(N_SITIOS, 0, 0.3))
factor_mes <- c(`11` = 1.30, `12` = 1.10, `01` = 0.90, `02` = 0.75, `03` = 0.70)
visitas <- visitas |>
  mutate(
    caudal_m3s = round(caudal_base[i] * factor_mes[format(fecha, "%m")] *
                         (1 + 0.012 * lluvia_48h_mm) * exp(rnorm(n(), 0, 0.12)), 2)
  ) |>
  group_by(i) |>
  mutate(relativo = caudal_m3s / median(caudal_m3s),
         nivel_rio = case_when(relativo > 1.25 ~ "alto",
                               relativo < 0.80 ~ "bajo",
                               TRUE ~ "medio")) |>
  ungroup() |>
  select(-relativo)

# =============================================================================
# 8. Registros por método: esfuerzo, detección y evidencias
# =============================================================================
registros <- visitas |>
  expand_grid(metodo = METODOS) |>
  mutate(metodo = factor(metodo, levels = METODOS)) |>
  arrange(i, n_visita, metodo) |>
  mutate(metodo = as.character(metodo))

n_reg <- nrow(registros)

# Esfuerzo de transecto: un mismo recorrido por visita para huellas, fecas y avistamiento
recorrido <- visitas |>
  transmute(id_visita,
            longitud_rec = ifelse(runif(n()) < 0.06, round(runif(n(), 1800, 2900), -1), 3000),
            horas_rec    = round(pmin(pmax(rnorm(n(), 3, 0.5), 1.5), 5) * longitud_rec / 3000, 1))
registros <- registros |> left_join(recorrido, by = "id_visita")

es_transecto <- registros$metodo != "camara_trampa"
es_camara    <- registros$metodo == "camara_trampa"

# Estado de la cámara por periodo
u <- runif(n_reg)
registros <- registros |>
  mutate(
    estado_camara = case_when(
      !es_camara     ~ NA_character_,
      n_visita == 1  ~ "instalacion",
      u < 0.04       ~ "falla_total",
      u < 0.14       ~ "falla_parcial",
      TRUE           ~ "operativa"),
    motivo_falla_camara = case_when(
      estado_camara == "falla_total" ~
        sample(c("robo", "crecida del río", "batería agotada al inicio", "tarjeta SD dañada"), n_reg, TRUE),
      estado_camara == "falla_parcial" ~
        sample(c("batería agotada", "tarjeta SD llena", "cámara desplazada"), n_reg, TRUE),
      TRUE ~ NA_character_),
    dias_camara = case_when(
      !es_camara                       ~ NA_real_,
      estado_camara == "instalacion"   ~ 0,
      estado_camara == "falla_total"   ~ 0,
      estado_camara == "falla_parcial" ~ round(dias_periodo * runif(n_reg, 0.2, 0.8)),
      TRUE                             ~ dias_periodo),
    longitud_tramo_m = ifelse(es_transecto, longitud_rec, NA),
    horas_busqueda   = ifelse(es_transecto, horas_rec, NA)
  )

# Sustrato donde se buscaron huellas y fecas: suele ser el de la ribera
registros <- registros |>
  mutate(sustrato_busqueda = ifelse(
    metodo %in% c("huellas", "fecas"),
    ifelse(runif(n_reg) < 0.6, sitios$sustrato_ribera[i],
           sample(sust_grande, n_reg, TRUE, prob = c(0.2, 0.3, 0.25, 0.15, 0.1))),
    NA_character_))

# Probabilidad de detección (condicional a presencia) por método
exp_c <- registros$experiencia_obs_anios - 3
p_det <- with(registros, case_when(
  metodo == "camara_trampa" ~ 1 - (1 - 0.03)^coalesce(dias_camara, 0),
  metodo == "huellas" ~ plogis(-0.1 +
                                 0.9 * (sustrato_busqueda %in% c("arena", "limo")) -
                                 0.6 * (sustrato_busqueda %in% c("bolones", "roca")) -
                                 0.05 * lluvia_48h_mm -
                                 0.7 * (nivel_rio == "alto") +
                                 0.15 * exp_c) * longitud_tramo_m / 3000,
  metodo == "fecas"   ~ plogis(-0.7 - 0.04 * lluvia_48h_mm +
                                 0.10 * pmin(dias_desde_lluvia, 10) -
                                 0.5 * (nivel_rio == "alto") +
                                 0.12 * exp_c) * longitud_tramo_m / 3000,
  metodo == "avistamiento" ~ 0.07
))
# Falsos positivos (identificación errónea) en sitios no ocupados
p_falso <- c(huellas = 0.04, fecas = 0.02, camara_trampa = 0, avistamiento = 0)[registros$metodo]

z_reg <- z[registros$i]
deteccion <- ifelse(z_reg == 1, rbinom(n_reg, 1, p_det), rbinom(n_reg, 1, p_falso))
deteccion[es_camara & registros$estado_camara %in% c("instalacion", "falla_total")] <- NA

evidencia <- c(huellas = "huella", fecas = "fecas", camara_trampa = "foto",
               avistamiento = "observacion_directa")
conf_verdadera <- list(huellas = c(0.15, 0.35, 0.50), fecas = c(0.20, 0.50, 0.30),
                       camara_trampa = c(0, 0.10, 0.90), avistamiento = c(0, 0.40, 0.60))
conf_falsa <- c(0.70, 0.30, 0)

confiab <- vapply(seq_len(n_reg), function(k) {
  if (is.na(deteccion[k]) || deteccion[k] == 0) return(NA_character_)
  pr <- if (z_reg[k] == 1) conf_verdadera[[registros$metodo[k]]] else conf_falsa
  sample(c("baja", "media", "alta"), 1, prob = pr)
}, character(1))

n_evid <- case_when(
  is.na(deteccion) ~ NA_real_,
  deteccion == 0   ~ 0,
  registros$metodo == "huellas"       ~ 1 + rpois(n_reg, 1.5),
  registros$metodo == "fecas"         ~ 1 + rpois(n_reg, 0.8),
  registros$metodo == "camara_trampa" ~ 1 + rpois(n_reg, 0.08 * coalesce(registros$dias_camara, 0)),
  TRUE ~ 1)

registros <- registros |>
  mutate(deteccion = deteccion,
         tipo_evidencia = ifelse(!is.na(deteccion) & deteccion == 1, evidencia[metodo], NA),
         confiabilidad = confiab,
         n_evidencias = n_evid)

# Otras especies: detección por método, condicional a su presencia en el sitio
p_otras <- list(
  nutria = c(huellas = 0.30, fecas = 0.25, camara_trampa = 0.45, avistamiento = 0.05),
  zorro  = c(huellas = 0.35, fecas = 0.20, camara_trampa = 0.55, avistamiento = 0.05),
  perro  = c(huellas = 0.40, fecas = 0.10, camara_trampa = 0.60, avistamiento = 0.20),
  ganado = c(huellas = 0.50, fecas = 0.30, camara_trampa = 0.50, avistamiento = 0.40))
detectar_otra <- function(z_sp, p) {
  out <- rbinom(n_reg, 1, z_sp[registros$i] * p[registros$metodo])
  out[es_camara & registros$estado_camara %in% c("instalacion", "falla_total")] <- NA
  out
}
registros <- registros |>
  mutate(presencia_nutria        = detectar_otra(z_nutria, p_otras$nutria),
         presencia_zorro         = detectar_otra(z_zorro,  p_otras$zorro),
         presencia_perro         = detectar_otra(z_perro,  p_otras$perro),
         presencia_ganado_ribera = detectar_otra(z_ganado, p_otras$ganado))

# =============================================================================
# 9. Tabla única (versión "verdadera", antes de ensuciar)
# =============================================================================
tabla <- registros |>
  mutate(id_camara_reg = ifelse(es_camara, sitios$id_camara[i], NA),
         fecha_inicio_camara = if_else(es_camara, fecha_inicio_camara, as.Date(NA))) |>
  left_join(st_drop_geometry(sitios) |> select(-id_camara), by = "id_sitio") |>
  mutate(id_camara = id_camara_reg,
         id_registro = row_number()) |>
  select(all_of(dicc$variable))

stopifnot(nrow(tabla) == N_SITIOS * N_VISITAS * length(METODOS))

# =============================================================================
# 10. Errores típicos de digitación -> archivo crudo
# =============================================================================
source("scripts/funciones/ensuciar_tabla.R")
set.seed(777)
crudo <- ensuciar_tabla(tabla, dicc)

dir.create("data/simulacion", showWarnings = FALSE)
write_delim(crudo, "data/raw/vison_muestreo_raw.csv", delim = ";", na = "")

# =============================================================================
# 11. Capas del diseño y estado real (para validar modelos)
# =============================================================================
diseno <- "data/intermedio/diseno_muestreo.gpkg"
if (file.exists(diseno)) invisible(file.remove(diseno))
st_write(sitios |> select(id_sitio, id_camara, rio, comuna), diseno, layer = "sitios", quiet = TRUE)
st_write(st_sf(id_sitio = id_sitio, id_transecto = sitios$id_transecto,
               rio = sitios$rio, geometry = transectos),
         diseno, layer = "transectos", quiet = TRUE)

write_csv(tibble(id_sitio, psi = round(psi, 3), ocupado = z,
                 nutria = z_nutria, zorro = z_zorro, perro = z_perro, ganado = z_ganado),
          "data/simulacion/estado_real_sitios.csv")
write_csv(tibble(parametro = names(beta), valor = unname(beta),
                 escala = "logit; predictores estandarizados (z); cercanía = -log(dist_rio + 50)"),
          "data/simulacion/parametros_simulacion.csv")

message("Listo: ", nrow(crudo), " filas crudas -> data/raw/vison_muestreo_raw.csv",
        "\n  sitios ocupados (real): ", sum(z), " de ", N_SITIOS,
        "\n  sitios con >= 1 detección: ",
        sum(tapply(tabla$deteccion, tabla$id_sitio, function(x) any(x == 1, na.rm = TRUE))))
