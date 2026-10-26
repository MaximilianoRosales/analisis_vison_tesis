# =============================================================================
# 03_limpiar_datos.R
# Limpia la planilla cruda de terreno y produce la tabla única analizable.
#
# Entrada : data/raw/vison_muestreo_raw.csv   (";" y coma decimal, como Excel CL)
#           docs/diccionario_datos.csv        (nombres, tipos, rangos, categorías)
# Salidas : data/clean/vison_muestreo_limpio.csv  (tabla única, "," y punto decimal)
#           data/clean/vison_muestreo_limpio.rds  (misma tabla con tipos de R)
#           data/clean/registro_limpieza.csv      (qué se corrigió y cuántas veces)
#           data/clean/vison_espacial.gpkg        (capas para mapas)
#
# Pasos:
#   1. Leer todo como texto y renombrar columnas según el diccionario
#   2. Quitar espacios sobrantes y unificar códigos de dato faltante
#   3. Eliminar filas duplicadas
#   4. Estandarizar categorías (método, comuna, observador, sí/no, etc.)
#   5. Convertir números (coma -> punto), fechas y horas
#   6. Corregir inconsistencias: las variables de sitio deben ser iguales en
#      todas las filas del sitio, y las de visita en todas las filas de la visita
#   7. Validar rangos y categorías permitidas (lo inválido pasa a NA)
#   8. Guardar
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
})

dicc <- read_csv("docs/diccionario_datos.csv", show_col_types = FALSE,
                 col_types = cols(.default = col_character()))
dir.create("data/clean", showWarnings = FALSE)

# Registro de cambios -----------------------------------------------------------
registro <- tibble(paso = character(), variable = character(), n = integer(), detalle = character())
anotar <- function(paso, variable, n, detalle) {
  if (n > 0) registro <<- add_row(registro, paso = paso, variable = variable,
                                  n = as.integer(n), detalle = detalle)
}

# Texto en minúsculas, sin tildes ni espacios sobrantes (para comparar)
normalizar <- function(x) {
  x |> stringi::stri_trans_general("Latin-ASCII") |> tolower() |>
    str_replace_all("[._]", " ") |> str_squish()
}

# =============================================================================
# 1. Lectura y nombres
# =============================================================================
raw <- read_delim("data/raw/vison_muestreo_raw.csv", delim = ";",
                  col_types = cols(.default = col_character()),
                  na = character(), trim_ws = FALSE,
                  locale = locale(encoding = "UTF-8"))

faltan <- setdiff(names(raw), dicc$nombre_raw)
if (length(faltan) > 0) stop("Columnas no descritas en el diccionario: ", paste(faltan, collapse = ", "))
names(raw) <- dicc$variable[match(names(raw), dicc$nombre_raw)]
d <- raw
message("Planilla cruda: ", nrow(d), " filas x ", ncol(d), " columnas")

# =============================================================================
# 2. Espacios y códigos de dato faltante
# =============================================================================
for (v in names(d)) {
  x <- d[[v]]
  limpio <- str_squish(x)
  anotar("2. espacios", v, sum(x != limpio, na.rm = TRUE), "espacios sobrantes eliminados")
  d[[v]] <- limpio
}

codigos_na <- c("", "NA", "-999", "s/d", "-")
for (v in names(d)) {
  x <- d[[v]]
  anotar("2. faltantes", v, sum(x %in% c("-999", "s/d", "-")), "códigos -999 / s/d / - convertidos a NA")
  d[[v]] <- ifelse(x %in% codigos_na, NA_character_, x)
}

# =============================================================================
# 3. Duplicados
# =============================================================================
n_antes <- nrow(d)
d <- distinct(d)
anotar("3. duplicados", "(fila completa)", n_antes - nrow(d), "filas duplicadas eliminadas")
stopifnot(!anyDuplicated(d$id_registro))

# =============================================================================
# 4. Categorías
# =============================================================================
# equivalencias: valor_limpio = c(formas normalizadas aceptadas)
recodificar <- function(x, equivalencias, variable) {
  clave <- normalizar(x)
  tabla <- unlist(lapply(names(equivalencias), function(v)
    setNames(rep(v, length(equivalencias[[v]])), equivalencias[[v]])))
  out <- unname(tabla[clave])
  sin_match <- !is.na(x) & is.na(out)
  if (any(sin_match)) {
    warning(variable, ": valores no reconocidos -> NA: ",
            paste(unique(x[sin_match]), collapse = ", "))
    anotar("4. categorías", variable, sum(sin_match), "valores no reconocidos convertidos a NA")
  }
  anotar("4. categorías", variable, sum(!is.na(x) & !is.na(out) & x != out),
         "valores escritos de otra forma unificados")
  out
}

equivalencias <- list(
  metodo = list(huellas = c("huellas", "huella"),
                fecas = c("fecas", "feca"),
                camara_trampa = c("camara trampa", "camara", "ct"),
                avistamiento = c("avistamiento", "avist", "obs directa")),
  comuna = list("Villarrica" = c("villarrica", "villarica"),
                "Pucón"      = c("pucon"),
                "Curarrehue" = c("curarrehue", "curarehue")),
  observador = list(obs_A = c("obs a"), obs_B = c("obs b"), obs_C = c("obs c")),
  confiabilidad = list(baja = "baja", media = c("media", "med"), alta = "alta"),
  tipo_evidencia = list(huella = c("huella", "huellas"), fecas = c("fecas", "feca"),
                        foto = c("foto", "fotografia"),
                        observacion_directa = c("observacion directa", "obs directa", "avistamiento")),
  estado_camara = list(instalacion = "instalacion", operativa = c("operativa", "ok"),
                       falla_parcial = "falla parcial", falla_total = "falla total"),
  nivel_rio = list(bajo = "bajo", medio = c("medio", "normal"), alto = c("alto", "crecido"))
)
for (v in names(equivalencias)) d[[v]] <- recodificar(d[[v]], equivalencias[[v]], v)

for (v in dicc$variable[dicc$tipo == "binaria"]) {
  d[[v]] <- recodificar(d[[v]], list("1" = c("1", "si"), "0" = c("0", "no")), v)
}

# =============================================================================
# 5. Números, fechas y horas
# =============================================================================
vars_num <- dicc$variable[dicc$tipo %in% c("numérico", "entero", "binaria")]
for (v in vars_num) {
  x <- d[[v]]
  con_coma <- str_detect(x, ",") & !is.na(x)
  anotar("5. números", v, sum(con_coma), "coma decimal convertida a punto")
  num <- suppressWarnings(as.numeric(str_replace(x, ",", ".")))
  anotar("5. números", v, sum(!is.na(x) & is.na(num)), "texto no numérico convertido a NA")
  d[[v]] <- num
}

leer_fecha <- function(x, variable) {
  out <- as.Date(rep(NA_character_, length(x)))
  iso  <- str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$") & !is.na(x)
  dmy4 <- str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$") & !is.na(x)
  dmy2 <- str_detect(x, "^\\d{1,2}-\\d{1,2}-\\d{2}$") & !is.na(x)
  out[iso]  <- as.Date(x[iso], "%Y-%m-%d")
  out[dmy4] <- as.Date(x[dmy4], "%d/%m/%Y")
  out[dmy2] <- as.Date(x[dmy2], "%d-%m-%y")
  anotar("5. fechas", variable, sum(dmy4 | dmy2), "fechas dd/mm/aaaa o d-m-aa llevadas a aaaa-mm-dd")
  anotar("5. fechas", variable, sum(!is.na(x) & is.na(out)), "fechas ilegibles convertidas a NA")
  out
}
for (v in dicc$variable[dicc$tipo == "fecha"]) d[[v]] <- leer_fecha(d[[v]], v)

leer_hora <- function(x, variable) {
  m1 <- str_match(x, "^(\\d{1,2})[:.](\\d{2})$")   # 9:05, 09:05, 09.05
  m2 <- str_match(x, "^(\\d{1,2})(\\d{2})$")        # 0905, 905
  h  <- coalesce(m1[, 2], m2[, 2]); mi <- coalesce(m1[, 3], m2[, 3])
  out <- ifelse(is.na(h), NA, sprintf("%02d:%02s", as.integer(h), mi))
  anotar("5. horas", variable, sum(!is.na(x) & !is.na(out) & x != out), "horas llevadas a HH:MM")
  anotar("5. horas", variable, sum(!is.na(x) & is.na(out)), "horas ilegibles convertidas a NA")
  out
}
for (v in dicc$variable[dicc$tipo == "hora"]) d[[v]] <- leer_hora(d[[v]], v)

# =============================================================================
# 6. Consistencia dentro de sitio y de visita
# =============================================================================
# Una variable de nivel "sitio" debe tener el mismo valor en las 16 filas del
# sitio; una de nivel "visita", en las 4 filas de la visita. Un valor distinto
# al de la mayoría es un error de digitación y se reemplaza por la moda.
moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(x[NA_integer_])
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
corregir_por_grupo <- function(d, vars, grupo, paso) {
  for (v in vars) {
    antes <- d[[v]]
    d <- d |> group_by(.data[[grupo]]) |> mutate("{v}" := moda(.data[[v]])) |> ungroup()
    cambiados <- !is.na(antes) & antes != d[[v]]
    completados <- is.na(antes) & !is.na(d[[v]])
    anotar(paso, v, sum(cambiados), paste("valor distinto al resto del", grupo, "-> moda"))
    anotar(paso, v, sum(completados), paste("NA completado con el valor del", grupo))
  }
  d
}
vars_sitio  <- setdiff(dicc$variable[dicc$nivel == "sitio"], "id_sitio")
vars_visita <- setdiff(dicc$variable[dicc$nivel == "visita"], c("id_visita", "n_visita"))
d <- corregir_por_grupo(d, vars_sitio,  "id_sitio",  "6. consistencia sitio")
d <- corregir_por_grupo(d, vars_visita, "id_visita", "6. consistencia visita")

# =============================================================================
# 7. Validación de rangos y categorías
# =============================================================================
for (k in seq_len(nrow(dicc))) {
  v <- dicc$variable[k]; tp <- dicc$tipo[k]
  mn <- dicc$min[k]; mx <- dicc$max[k]
  if (tp %in% c("numérico", "entero", "binaria") && (!is.na(mn) || !is.na(mx))) {
    fuera <- !is.na(d[[v]]) &
      ((!is.na(mn) & d[[v]] < as.numeric(mn)) | (!is.na(mx) & d[[v]] > as.numeric(mx)))
    anotar("7. rangos", v, sum(fuera), paste0("fuera de rango [", mn, ", ", mx, "] -> NA"))
    d[[v]][fuera] <- NA
  }
  if (tp == "fecha" && !is.na(mn)) {
    fuera <- !is.na(d[[v]]) & (d[[v]] < as.Date(mn) | d[[v]] > as.Date(mx))
    anotar("7. rangos", v, sum(fuera), paste0("fecha fuera de temporada [", mn, ", ", mx, "] -> NA"))
    d[[v]][fuera] <- NA
  }
  if (str_starts(tp, "categórica") && !is.na(dicc$valores_permitidos[k])) {
    permitidos <- str_split(dicc$valores_permitidos[k], " \\| ")[[1]]
    malo <- !is.na(d[[v]]) & !(d[[v]] %in% permitidos)
    anotar("7. categorías", v, sum(malo), "categoría no permitida -> NA")
    d[[v]][malo] <- NA
  }
}

# Coherencia entre columnas: sin detección no hay tipo de evidencia ni confiabilidad
sin_det <- is.na(d$deteccion) | d$deteccion == 0
anotar("7. coherencia", "tipo_evidencia", sum(sin_det & !is.na(d$tipo_evidencia)),
       "tipo de evidencia sin detección -> NA")
d$tipo_evidencia[sin_det] <- NA
d$confiabilidad[sin_det]  <- NA

# =============================================================================
# 8. Tipos finales, orden y guardado
# =============================================================================
for (v in dicc$variable[dicc$tipo %in% c("entero", "binaria")]) d[[v]] <- as.integer(d[[v]])

limpio <- d |>
  arrange(as.integer(id_registro)) |>
  mutate(id_registro = as.integer(id_registro)) |>
  select(all_of(dicc$variable))

write_csv(limpio, "data/clean/vison_muestreo_limpio.csv", na = "")

# Versión .rds con factores ordenados (útil para graficar en R)
limpio_r <- limpio |>
  mutate(metodo = factor(metodo, levels = c("huellas", "fecas", "camara_trampa", "avistamiento")),
         comuna = factor(comuna, levels = c("Villarrica", "Pucón", "Curarrehue")),
         confiabilidad = factor(confiabilidad, levels = c("baja", "media", "alta"), ordered = TRUE),
         densidad_veg  = factor(densidad_veg,  levels = c("baja", "media", "alta"), ordered = TRUE),
         nivel_rio     = factor(nivel_rio,     levels = c("bajo", "medio", "alto"), ordered = TRUE))
saveRDS(limpio_r, "data/clean/vison_muestreo_limpio.rds")

write_csv(registro, "data/clean/registro_limpieza.csv")

# --- Capas espaciales para mapas --------------------------------------------------
crs_utm <- 32719
gpkg <- "data/clean/vison_espacial.gpkg"
if (file.exists(gpkg)) invisible(file.remove(gpkg))

vars_sitio_mapa <- c("id_sitio", dicc$variable[dicc$nivel == "sitio"])
sitios <- limpio |>
  group_by(id_sitio) |>
  summarise(n_registros_validos = sum(!is.na(deteccion)),
            n_detecciones = sum(deteccion, na.rm = TRUE),
            deteccion_naive = as.integer(n_detecciones > 0),
            .groups = "drop") |>
  left_join(limpio |> select(all_of(unique(vars_sitio_mapa))) |> distinct(id_sitio, .keep_all = TRUE),
            by = "id_sitio") |>
  st_as_sf(coords = c("utm_este", "utm_norte"), crs = crs_utm, remove = FALSE)

st_write(sitios, gpkg, layer = "sitios", quiet = TRUE)
st_write(st_read("data/intermedio/diseno_muestreo.gpkg", layer = "transectos", quiet = TRUE),
         gpkg, layer = "transectos", quiet = TRUE)
st_write(read_csv("data/raw/centros_productivos.csv", show_col_types = FALSE) |>
           st_as_sf(coords = c("utm_este", "utm_norte"), crs = crs_utm, remove = FALSE),
         gpkg, layer = "centros_productivos", quiet = TRUE)
for (capa in c("area_estudio", "comunas", "subsubcuencas", "rios", "lagos")) {
  st_write(st_read("data/intermedio/capas_base.gpkg", layer = capa, quiet = TRUE),
           gpkg, layer = capa, quiet = TRUE)
}

message("Tabla limpia: ", nrow(limpio), " filas x ", ncol(limpio), " columnas",
        "\n  -> data/clean/vison_muestreo_limpio.csv / .rds",
        "\n  -> data/clean/vison_espacial.gpkg",
        "\n  correcciones registradas: ", sum(registro$n),
        " (ver data/clean/registro_limpieza.csv)")
