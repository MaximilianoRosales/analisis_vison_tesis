# =============================================================================
# estandarizar_red_hidrica(): deja la red hídrica DGA con columnas estándar
#   nombre_rio : nombre del cauce (nombre_bcn; si falta, nombre_mop)
#   tipo_cauce : Río, Estero, Quebrada, ... (tipo_bcn; si falta, tipo_mop)
#   strahler   : orden de Strahler (strahler_n)
# =============================================================================
estandarizar_red_hidrica <- function(x) {
  vacio_a_na <- function(v) {
    v <- stringr::str_squish(as.character(v))
    v[v %in% c("", "S/N", "Sin nombre", "SIN NOMBRE")] <- NA
    v
  }
  x |>
    dplyr::transmute(
      nombre_rio = dplyr::coalesce(vacio_a_na(nombre_bcn), vacio_a_na(nombre_mop)),
      tipo_cauce = dplyr::coalesce(vacio_a_na(tipo_bcn),   vacio_a_na(tipo_mop)),
      strahler   = as.integer(strahler_n)
    )
}
