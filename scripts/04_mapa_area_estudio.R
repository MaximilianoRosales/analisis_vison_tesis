# =============================================================================
# 04_mapa_area_estudio.R
# Mapa del área de estudio: comunas, red hídrica, lagos, sitios de muestreo y
# centros productivos (SIMULADOS).
#
# Entrada : data/clean/vison_espacial.gpkg   (generado por 03_limpiar_datos.R)
# Salida  : figures/mapa_area_estudio.png
#
# Ejecutar desde la raíz del proyecto:  Rscript scripts/04_mapa_area_estudio.R
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggimage)
})

gpkg <- "data/clean/vison_espacial.gpkg"
leer <- function(capa) st_read(gpkg, layer = capa, quiet = TRUE)

comunas <- leer("comunas")
area    <- leer("area_estudio")
rios    <- leer("rios")
lagos   <- leer("lagos")
sitios  <- leer("sitios")
centros <- leer("centros_productivos") |>
  mutate(tipo = recode(tipo,
                       piscicultura = "Piscicultura",
                       ganaderia    = "Ganadería",
                       avicola      = "Avícola"))

col_agua <- "#2C7FB8"

# Ruta del ícono según el tipo de centro, y coordenadas como columnas
iconos <- c("Piscicultura" = "figures/iconos/piscicultura.png",
            "Ganadería"    = "figures/iconos/ganaderia.png",
            "Avícola"      = "figures/iconos/avicola.png")

colores <- c("Piscicultura" = "#7B3294",
             "Ganadería"    = "#8C510A",
             "Avícola"      = "#E66101")

centros_img <- centros |>
  st_drop_geometry() |>
  mutate(icono = unname(iconos[tipo]),
         color = unname(colores[tipo]))

mapa <- ggplot() +
  geom_sf(data = comunas, fill = "grey95", colour = "grey55", linewidth = 0.3) +
  geom_sf(data = area, fill = NA, colour = "grey20", linewidth = 0.6, linetype = "dashed") +
  geom_sf(data = lagos, fill = col_agua, colour = col_agua, alpha = 0.6) +
  geom_sf(data = rios, colour = col_agua, linewidth = 0.25, alpha = 0.8) +
  geom_image(data = centros_img,
             aes(x = utm_este, y = utm_norte, image = icono, color = color),
             size = 0.03) +
  scale_color_identity() +
  geom_sf(data = sitios, aes(fill = "Sitio de muestreo"),
          shape = 21, colour = "white", size = 1.8, stroke = 0.3) +
  scale_shape_manual(name = "Centro productivo (simulado)",
                     values = c("Piscicultura" = 17, "Ganadería" = 15, "Avícola" = 16)) +
  scale_fill_manual(name = NULL, values = c("Sitio de muestreo" = "#1B1B1B")) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  labs(title = "Área de estudio",
       subtitle = "Comunas de Villarrica, Pucón y Curarrehue. Datos simulados.",
       caption = "Proyección UTM huso 19S (EPSG:32719). Fuente de capas base: DGA, chilemapas.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_line(colour = "grey90", linewidth = 0.2),
        legend.position = "right",
        plot.title = element_text(face = "bold"))

# Escala y flecha norte, si el paquete ggspatial está instalado
if (requireNamespace("ggspatial", quietly = TRUE)) {
  mapa <- mapa +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      style = ggspatial::north_arrow_minimal())
}
mapa
dir.create("figures", showWarnings = FALSE)
ggsave("figures/mapa_area_estudio.png", mapa, width = 8, height = 6.5, dpi = 300, bg = "white")
message("Mapa guardado en figures/mapa_area_estudio.png")
