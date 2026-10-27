# =============================================================================
# 01_simular_centros_productivos.R
# Crea una capa SIMULADA de centros productivos del área de estudio:
#   - pisciculturas  (ubicadas junto a cauces, como las reales)
#   - ganaderías     (predios con ganado, en fondos de valle)
#   - avícolas       (granjas de aves, cerca de poblados)
#
# IMPORTANTE: no se obtuvo el registro de pisciculturas de Sernapesca desde
# este entorno, por lo que todas las ubicaciones, nombres y tamaños son
# ficticios. Cuando se disponga del registro real, basta con reemplazar
# data/raw/centros_productivos.csv manteniendo las mismas columnas; los
# scripts siguientes recalculan todo.
#
# Entrada : data/intermedio/capas_base.gpkg
# Salida  : data/raw/centros_productivos.csv
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

set.seed(20261101)
crs_utm <- 32719
capas   <- "data/intermedio/capas_base.gpkg"

area    <- st_read(capas, layer = "area_estudio", quiet = TRUE) |> st_geometry()
comunas <- st_read(capas, layer = "comunas",      quiet = TRUE)
rios    <- st_read(capas, layer = "rios",         quiet = TRUE)
lagos   <- st_read(capas, layer = "lagos",        quiet = TRUE) |> st_geometry()

n_pisciculturas <- 12
n_ganaderias    <- 45
n_avicolas      <- 15

# Poblados de referencia (coordenadas aproximadas, WGS84)
poblados <- read_csv("data/raw/espacial/poblados_referencia.csv", show_col_types = FALSE) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_utm)

# --- Funciones auxiliares -----------------------------------------------------
# Se trabaja con matrices de coordenadas (x, y) en metros y se convierte a sf al final.
source("scripts/funciones/geometria.R")   # puntos_en_lineas(), a_sf(), etc.

# Desplaza puntos una distancia (m) en una dirección aleatoria
desplazar <- function(xy, dist_min, dist_max) {
  ang <- runif(nrow(xy), 0, 2 * pi)
  d   <- runif(nrow(xy), dist_min, dist_max)
  cbind(xy[, 1] + d * cos(ang), xy[, 2] + d * sin(ang))
}

dentro_area <- function(xy) {
  pts <- st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = crs_utm)
  lengths(st_intersects(pts, area)) > 0 &      # dentro del área de estudio
    lengths(st_intersects(pts, lagos)) == 0     # y fuera de lagos y lagunas
}

# Muestreo con rechazo: genera candidatos hasta tener n puntos dentro del área
# y separados al menos sep_min metros entre sí (y de los puntos 'previos')
muestrear <- function(generar, n, sep_min, previos = NULL) {
  todos <- if (is.null(previos)) matrix(numeric(0), ncol = 2) else previos
  nuevos <- matrix(numeric(0), ncol = 2)
  intentos <- 0
  while (nrow(nuevos) < n && intentos < 500) {
    intentos <- intentos + 1
    cand <- generar(50)
    cand <- cand[dentro_area(cand), , drop = FALSE]
    for (k in seq_len(nrow(cand))) {
      if (nrow(nuevos) >= n) break
      d <- if (nrow(todos) == 0) Inf else sqrt((todos[, 1] - cand[k, 1])^2 + (todos[, 2] - cand[k, 2])^2)
      if (min(d) >= sep_min) {
        nuevos <- rbind(nuevos, cand[k, ])
        todos  <- rbind(todos, cand[k, ])
      }
    }
  }
  if (nrow(nuevos) < n) stop("No se lograron ubicar ", n, " puntos; revisar parámetros")
  nuevos
}

# --- 1. Pisciculturas: junto a ríos y esteros tributarios ----------------------
# (se evita el cauce principal del Toltén, que sale del lago)
rios_pisc <- rios |>
  filter(is.na(nombre_rio) | !grepl("tolt", nombre_rio, ignore.case = TRUE))

gen_pisc <- function(k) desplazar(puntos_en_lineas(rios_pisc, k), 20, 120)
xy_pisc  <- muestrear(gen_pisc, n_pisciculturas, sep_min = 2000)

pisc <- a_sf(xy_pisc) |>
  mutate(
    tipo                = "piscicultura",
    subtipo             = sample(c("Salmón del Atlántico", "Trucha arcoíris", "Salmón coho"),
                                 n(), replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    fase                = sample(c("Alevinaje y smolt", "Reproductores", "Ovas y alevines"),
                                 n(), replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    produccion_ton_anio = pmin(pmax(round(rlnorm(n(), log(120), 0.9)), 8), 1500),
    cabezas_ganado      = NA_real_,
    n_aves              = NA_real_
  )

# --- 2. Ganaderías: fondos de valle, cerca de cauces --------------------------
gen_gana <- function(k) desplazar(puntos_en_lineas(rios, k), 150, 2500)
xy_gana  <- muestrear(gen_gana, n_ganaderias, sep_min = 600, previos = xy_pisc)

gana <- a_sf(xy_gana) |>
  mutate(
    tipo                = "ganaderia",
    subtipo             = sample(c("Bovino de carne", "Bovino de leche", "Ovino"),
                                 n(), replace = TRUE, prob = c(0.55, 0.30, 0.15)),
    fase                = NA_character_,
    produccion_ton_anio = NA_real_,
    cabezas_ganado      = pmin(pmax(round(rlnorm(n(), log(80), 0.8)), 5), 1200),
    n_aves              = NA_real_
  )

# --- 3. Avícolas: en torno a los poblados --------------------------------------
xy_poblados <- st_coordinates(poblados)
gen_avic <- function(k) {
  i <- sample(nrow(xy_poblados), k, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15))
  desplazar(xy_poblados[i, , drop = FALSE], 1000, 9000)
}
xy_avic <- muestrear(gen_avic, n_avicolas, sep_min = 800,
                     previos = rbind(xy_pisc, xy_gana))

avic <- a_sf(xy_avic) |>
  mutate(
    tipo                = "avicola",
    subtipo             = sample(c("Postura", "Broiler"), n(), replace = TRUE,
                                 prob = c(0.6, 0.4)),
    fase                = NA_character_,
    produccion_ton_anio = NA_real_,
    cabezas_ganado      = NA_real_,
    n_aves              = pmin(pmax(round(rlnorm(n(), log(3000), 1)), 200), 60000)
  )

# --- 4. Unir, asignar comuna e identificadores ---------------------------------
prefijo <- c(piscicultura = "PIS", ganaderia = "GAN", avicola = "AVI")

centros <- bind_rows(pisc, gana, avic) |>
  st_join(comunas |> select(comuna), join = st_nearest_feature) |>
  group_by(tipo) |>
  mutate(id_centro = sprintf("%s-%02d", prefijo[tipo], row_number())) |>
  ungroup() |>
  mutate(
    nombre    = paste(c(piscicultura = "Piscicultura simulada",
                        ganaderia    = "Predio ganadero simulado",
                        avicola      = "Granja avícola simulada")[tipo],
                      sub(".*-", "", id_centro)),
    utm_este  = round(st_coordinates(geometry)[, 1]),
    utm_norte = round(st_coordinates(geometry)[, 2]),
    fuente    = "simulado"
  ) |>
  st_drop_geometry() |>
  select(id_centro, nombre, tipo, subtipo, fase, produccion_ton_anio,
         cabezas_ganado, n_aves, comuna, utm_este, utm_norte, fuente)

write_csv(centros, "data/raw/centros_productivos.csv", na = "")
message("Centros productivos simulados: ", nrow(centros), " -> data/raw/centros_productivos.csv")
print(table(centros$tipo, centros$comuna))
