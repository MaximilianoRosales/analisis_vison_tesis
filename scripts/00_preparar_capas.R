# =============================================================================
# 00_preparar_capas.R
# Prepara las capas espaciales base del área de estudio.
#
# Área de estudio = comunas de Villarrica, Pucón y Curarrehue, dentro de la
# cuenca del río Toltén (código DGA 094): Lago Villarrica, sus afluentes
# (ríos Pucón/Trancura, Liucura, Maichín, etc.) y el río Toltén aguas abajo
# del lago dentro de la comuna de Villarrica.
#
# Entradas (data/raw/espacial/):
#   - mapa_comunas_chilemapas.rda  : límites comunales (paquete chilemapas)
#   - SubSubCuencas_BNA.shp        : subsubcuencas DGA (Banco Nacional de Aguas)
#   - red_hidrografica/*.shp       : red hídrica DGA (líneas), recortada al área
#   - masas_lacustres/*.shp        : lagos y lagunas DGA (polígonos)
#
# Salida:
#   - data/intermedio/capas_base.gpkg (capas: comunas, area_estudio,
#     subsubcuencas, rios, lagos)
#
# Todas las capas se proyectan a WGS84 / UTM huso 19S (EPSG:32719).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
})

crs_utm   <- 32719
dir_esp   <- "data/raw/espacial"
salida    <- "data/intermedio/capas_base.gpkg"
dir.create("data/intermedio", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Comunas
# -----------------------------------------------------------------------------
load(file.path(dir_esp, "mapa_comunas_chilemapas.rda"))   # objeto: mapa_comunas

cod_comunas <- c("09120" = "Villarrica", "09115" = "Pucón", "09104" = "Curarrehue")

comunas <- st_as_sf(mapa_comunas) |>
  filter(codigo_comuna %in% names(cod_comunas)) |>
  mutate(comuna = unname(cod_comunas[codigo_comuna])) |>
  select(codigo_comuna, comuna) |>
  st_transform(crs_utm) |>
  st_make_valid()

# -----------------------------------------------------------------------------
# 2. Subsubcuencas DGA
# -----------------------------------------------------------------------------
ssc <- st_read(file.path(dir_esp, "SubSubCuencas_BNA.shp"), quiet = TRUE) |>
  st_transform(crs_utm) |>
  st_make_valid() |>
  # el .dbf trae la "ñ" codificada como CP850 (byte A4) dentro de texto UTF-8
  mutate(NOM_SSUBC = str_squish(str_replace_all(NOM_SSUBC, "\\x{00A4}", "ñ"))) |>
  select(cod_cuenca = COD_CUEN, cod_subcuenca = COD_SUBC,
         cod_subsubcuenca = COD_SSUBC, nom_subsubcuenca = NOM_SSUBC)

# Área de estudio: comunas ∩ cuenca del Toltén (094). Así se excluye la parte
# sur de la comuna de Villarrica que drena al Lago Calafquén (cuenca 101).
cuenca_tolten <- ssc |> filter(cod_cuenca == "094") |> st_union()

area_estudio <- st_intersection(st_union(comunas), cuenca_tolten) |>
  st_make_valid() |>
  st_as_sf() |>
  mutate(nombre = "Área de estudio: comunas Villarrica, Pucón y Curarrehue (cuenca río Toltén)")
st_geometry(area_estudio) <- "geom"

ssc_area <- ssc |>
  filter(cod_cuenca == "094") |>
  st_intersection(st_geometry(area_estudio)) |>
  mutate(area_km2 = round(as.numeric(st_area(geometry)) / 1e6, 2)) |>
  filter(area_km2 > 1)   # descarta astillas de borde

# -----------------------------------------------------------------------------
# 3. Red hídrica DGA
# -----------------------------------------------------------------------------
archivo_rios <- list.files(file.path(dir_esp, "red_hidrografica"),
                           pattern = "\\.shp$", full.names = TRUE)
if (length(archivo_rios) == 0) {
  stop("No se encontró la red hídrica en data/raw/espacial/red_hidrografica/")
}

# Nota: la capa nacional "Red_Hidrografica_Final" (DGA, ~600 MB) se recortó al
# rectángulo del área de estudio + 2 km antes de incluirla en el repositorio.
# Con la capa completa, el recorte equivalente en R es:
#   bb <- st_as_text(st_as_sfc(st_bbox(c(xmin = 202010, ymin = 5607532,
#                                        xmax = 297211, ymax = 5685492), crs = crs_utm)))
#   st_read("Red_Hidrografica_Final.shp", wkt_filter = bb)
rios_raw <- st_read(archivo_rios[1], quiet = TRUE) |>
  st_transform(crs_utm) |>
  st_zm(drop = TRUE)

source("scripts/funciones/estandarizar_red_hidrica.R")   # nombres de columnas DGA -> estándar
source("scripts/funciones/geometria.R")

# La red se conserva hasta 5 km fuera del área de estudio para no cortar ríos
# que corren por el límite comunal (p. ej. el Toltén) y para mantener las
# conexiones entre tributarios que confluyen justo afuera. Los sitios de
# muestreo se ubican solo dentro del área (ver 02_simular_muestreo.R).
dominio_red <- st_buffer(st_geometry(area_estudio), 5000)

rios <- estandarizar_red_hidrica(rios_raw) |>
  st_intersection(dominio_red) |>
  st_cast("MULTILINESTRING") |>
  st_cast("LINESTRING") |>
  st_simplify(preserveTopology = TRUE, dTolerance = 5) |>   # vértices cada ~1,5 m -> tolerancia 5 m
  mutate(longitud_m = as.numeric(st_length(geometry))) |>
  filter(longitud_m > 5)

# Las líneas DGA están digitalizadas de aguas arriba hacia aguas abajo, pero hay
# pequeñas brechas (< 75 m) en algunas confluencias. Se cierran extendiendo el
# extremo suelto hasta el cauce más cercano e insertando ese vértice en él.
rios <- cerrar_brechas(rios, tolerancia = 75)

rios <- rios |>
  mutate(longitud_m = as.numeric(st_length(geometry)),
         en_area = lengths(st_intersects(geometry, st_buffer(st_geometry(area_estudio), 500))) > 0,
         id_tramo = sprintf("TR%05d", row_number())) |>
  select(id_tramo, nombre_rio, tipo_cauce, strahler, longitud_m, en_area)

# -----------------------------------------------------------------------------
# 4. Lagos y lagunas (DGA, masas lacustres)
# -----------------------------------------------------------------------------
# Se usan para (a) no ubicar centros productivos ni cámaras dentro de un lago,
# (b) conectar la red hídrica a través de los lagos y (c) los mapas.
lagos <- st_read(file.path(dir_esp, "masas_lacustres", "masas_lacustres.shp"), quiet = TRUE) |>
  st_transform(crs_utm) |>
  st_make_valid() |>
  st_filter(dominio_red) |>
  transmute(nombre_lago = Nombre, tipo = Tipo) |>
  mutate(area_km2 = round(as.numeric(st_area(geometry)) / 1e6, 3))

# -----------------------------------------------------------------------------
# 5. Guardar
# -----------------------------------------------------------------------------
if (file.exists(salida)) invisible(file.remove(salida))
st_write(comunas,      salida, layer = "comunas",       quiet = TRUE)
st_write(area_estudio, salida, layer = "area_estudio",  quiet = TRUE)
st_write(ssc_area,     salida, layer = "subsubcuencas", quiet = TRUE)
st_write(rios,         salida, layer = "rios",          quiet = TRUE)
st_write(lagos,        salida, layer = "lagos",         quiet = TRUE)

message("Capas base guardadas en ", salida,
        "\n  subsubcuencas: ", nrow(ssc_area),
        "\n  tramos de río: ", nrow(rios),
        " (", round(sum(rios$longitud_m) / 1000), " km)")
