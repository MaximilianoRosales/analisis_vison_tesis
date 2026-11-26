# =============================================================================
# 05_resumen_datos.R
# Resume la base limpia (SIMULADA) en tablas pequeñas para la página web.
#
# Entradas: data/clean/vison_muestreo_limpio.csv
#           data/clean/registro_limpieza.csv
# Salidas : data/resultados/resumen_base.csv         (totales de la base)
#           data/resultados/detecciones_metodo.csv   (registros y detecciones por método)
#           data/resultados/resumen_limpieza.csv     (correcciones por paso)
#
# Ejecutar desde la raíz del proyecto:  Rscript scripts/05_resumen_datos.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

dir.create("data/resultados", showWarnings = FALSE, recursive = TRUE)

limpio   <- read_csv("data/clean/vison_muestreo_limpio.csv", show_col_types = FALSE)
registro <- read_csv("data/clean/registro_limpieza.csv",     show_col_types = FALSE)

# 1. Totales de la base ------------------------------------------------------
resumen_base <- tibble(
  indicador = c("Sitios de muestreo", "Visitas", "Registros (sitio × visita × método)",
                "Registros con detección válida", "Detecciones de visón"),
  valor = c(n_distinct(limpio$id_sitio),
            n_distinct(limpio$id_visita),
            nrow(limpio),
            sum(!is.na(limpio$deteccion)),
            sum(limpio$deteccion, na.rm = TRUE))
)
write_csv(resumen_base, "data/resultados/resumen_base.csv")

# 2. Registros y detecciones por método ------------------------------------------
detecciones_metodo <- limpio |>
  group_by(metodo) |>
  summarise(registros   = n(),
            validos     = sum(!is.na(deteccion)),
            detecciones = sum(deteccion, na.rm = TRUE),
            .groups = "drop") |>
  mutate(porcentaje_deteccion = round(100 * detecciones / validos, 1)) |>
  arrange(desc(detecciones))
write_csv(detecciones_metodo, "data/resultados/detecciones_metodo.csv")

# 3. Correcciones de limpieza por paso --------------------------------------------
resumen_limpieza <- registro |>
  group_by(paso) |>
  summarise(correcciones = sum(n), variables_afectadas = n_distinct(variable),
            .groups = "drop") |>
  arrange(paso)
write_csv(resumen_limpieza, "data/resultados/resumen_limpieza.csv")

message("Tablas guardadas en data/resultados/")
