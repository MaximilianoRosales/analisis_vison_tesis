# =============================================================================
# 06_figuras_resultados.R
# Resultados exploratorios (con datos SIMULADOS):
#   1. Detección de visón por método de muestreo
#   2. Detección en cámaras trampa según distancia al predio ganadero más cercano
#   3. Regresión logística simple (detección ~ distancia), como exploración
#
# Entrada : data/clean/vison_muestreo_limpio.csv
# Salidas : figures/detecciones_metodo.png
#           figures/deteccion_distancia_ganaderia.png
#           data/resultados/modelo_ganaderia.csv
#
# Ejecutar desde la raíz del proyecto:  Rscript scripts/06_figuras_resultados.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

dir.create("figures", showWarnings = FALSE)
dir.create("data/resultados", showWarnings = FALSE, recursive = TRUE)

limpio <- read_csv("data/clean/vison_muestreo_limpio.csv", show_col_types = FALSE)

# --- Figura 1: % de detección por método -------------------------------------
por_metodo <- limpio |>
  filter(!is.na(deteccion)) |>
  group_by(metodo) |>
  summarise(n = n(), detecciones = sum(deteccion), .groups = "drop") |>
  mutate(pct = 100 * detecciones / n,
         metodo = recode(metodo,
                         camara_trampa = "Cámara trampa",
                         huellas       = "Huellas",
                         fecas         = "Fecas",
                         avistamiento  = "Avistamiento"),
         metodo = reorder(metodo, -pct))

fig1 <- ggplot(por_metodo, aes(metodo, pct)) +
  geom_col(fill = "#4D4D4D", width = 0.65) +
  geom_text(aes(label = paste0(detecciones, "/", n)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Registros con detección (%)",
       title = "Detección de visón por método de muestreo",
       subtitle = "Datos simulados. Sobre cada barra: detecciones / registros válidos.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("figures/detecciones_metodo.png", fig1, width = 7, height = 4.5, dpi = 300, bg = "white")

# --- Figura 2: detección en cámaras según distancia a ganadería --------------
# Una fila por sitio: ¿hubo alguna detección en sus cámaras trampa?
camaras <- limpio |>
  filter(metodo == "camara_trampa", !is.na(deteccion)) |>
  group_by(id_sitio) |>
  summarise(deteccion_camara = as.integer(any(deteccion == 1)),
            dist_ganaderia_km = first(dist_euc_ganaderia_m) / 1000,
            .groups = "drop") |>
  filter(!is.na(dist_ganaderia_km))

fig2 <- ggplot(camaras, aes(dist_ganaderia_km, deteccion_camara)) +
  geom_jitter(height = 0.04, width = 0, alpha = 0.6, size = 2, colour = "#8C510A") +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              colour = "#8C510A", fill = "#8C510A", alpha = 0.15) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No detectado", "Detectado")) +
  labs(x = "Distancia al predio ganadero más cercano (km)", y = NULL,
       title = "Detección en cámaras trampa según distancia a ganadería",
       subtitle = "Datos simulados. Cada punto es un sitio; la curva es una regresión logística simple.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("figures/deteccion_distancia_ganaderia.png", fig2, width = 7.5, height = 4.5, dpi = 300, bg = "white")

# --- Modelo logístico simple ---------------------------------------------------
m <- glm(deteccion_camara ~ dist_ganaderia_km, data = camaras, family = binomial)
coef <- summary(m)$coefficients
ic   <- confint.default(m)

modelo <- tibble(
  termino     = rownames(coef),
  estimador   = coef[, "Estimate"],
  error_est   = coef[, "Std. Error"],
  p_valor     = coef[, "Pr(>|z|)"],
  razon_odds  = exp(coef[, "Estimate"]),
  or_ic_inf   = exp(ic[, 1]),
  or_ic_sup   = exp(ic[, 2]),
  n_sitios    = nrow(camaras)
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

write_csv(modelo, "data/resultados/modelo_ganaderia.csv")
message("Figuras en figures/ y modelo en data/resultados/modelo_ganaderia.csv")
