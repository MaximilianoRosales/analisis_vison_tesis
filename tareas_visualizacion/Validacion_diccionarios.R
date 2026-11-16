library(readr)
library(tidyverse)
library(dplyr)

vison_limpio <- vison
dicc <- read_csv("docs/diccionario_datos.csv", show_col_types = FALSE,
                 col_types = cols(.default = col_character()))

sub_diccionario <- dicc |>
  filter(variable %in% c("deteccion", "dist_rio_centro_m", "metodo")) |>
  select(variable, tipo, valores_permitidos, min, max, descripcion)



validar_visones <- function(datos) {
  # Estructural: detección binaria (NA permitido)
  deteccion_ok <- all(datos$deteccion %in% c(0, 1) | is.na(datos$deteccion))

  # Estructural: método dentro de las categorías permitidas (sin NA)
  metodo_ok <- all(datos$metodo %in% c("huellas", "fecas",
                                       "camara_trampa", "avistamiento"))

  # Contenido: distancia por río entre 0 y 300000 m
  dist_ok <- all(datos$dist_rio_centro_m >= 0 &
                   datos$dist_rio_centro_m <= 300000,
                 na.rm = TRUE)

  # Completitud: metodo y distancia no deben tener NA
  completas_ok <- !anyNA(datos[, c("deteccion", "dist_rio_centro_m")])

  marca <- function(x) ifelse(isTRUE(x), "OK", "FALLA")

  cat("Detección 0/1 (estructural):        ", marca(deteccion_ok),  "\n")
  cat("Método válido (estructural):        ", marca(metodo_ok),     "\n")
  cat("Distancia río 0-300000 (contenido): ", marca(dist_ok),       "\n")
  cat("Distancia sin NA:                   ", marca(completas_ok),  "\n")
}

validar_visones(vison_limpio)