# =============================================================================
# ejecutar_todo.R
# Reproduce la base de datos completa desde las capas crudas.
# Ejecutar desde la raíz del proyecto:  Rscript R/ejecutar_todo.R
# =============================================================================

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  warning("La sesión de R no está en UTF-8; los nombres con tildes (Pucón, Toltén) ",
          "pueden quedar mal escritos. En Linux/Mac: LANG=es_CL.UTF-8 Rscript R/ejecutar_todo.R")
}

paquetes <- c("sf", "dplyr", "tidyr", "readr", "stringr", "stringi", "igraph", "units")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) stop("Instalar antes: install.packages(c(\"", paste(faltan, collapse = "\", \""), "\"))")

pasos <- c("R/00_preparar_capas.R",
           "R/01_simular_centros_productivos.R",
           "R/02_simular_muestreo.R",
           "R/03_limpiar_datos.R")

for (p in pasos) {
  message("\n==== ", p, " ====")
  source(p, local = new.env(), encoding = "UTF-8")
}
message("\nListo. Tabla final: data/clean/vison_muestreo_limpio.csv")
