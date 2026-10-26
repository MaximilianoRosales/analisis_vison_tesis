# =============================================================================
# ensuciar_tabla(): introduce errores típicos de una planilla de terreno
# digitada a mano en Excel (configuración regional de Chile), para que el paso
# raw -> clean tenga sentido. Los errores son deliberados y documentados:
#
#   - Encabezados "de planilla" (con espacios, tildes, unidades)
#   - Separador ";" y coma decimal (con algunos valores escritos con punto)
#   - Fechas en formatos mezclados: dd/mm/aaaa, aaaa-mm-dd, d-m-aa
#   - Horas en formatos mezclados: 09:05, 9:05, 0905, 09.05
#   - Categorías con mayúsculas, tildes, abreviaturas y espacios sobrantes
#   - Binarias como 1/0, si/no, Sí/No
#   - Datos faltantes codificados como "", NA, -999, s/d
#   - Errores de digitación puntuales en variables de sitio y de visita
#     (dígito extra, coordenadas intercambiadas, dígito faltante)
#   - Un valor fuera de rango repetido en toda una visita (no recuperable)
#   - Filas duplicadas
# =============================================================================

ensuciar_tabla <- function(tabla, dicc) {
  n <- nrow(tabla)
  tipo <- setNames(dicc$tipo, dicc$variable)
  azar <- function(p) runif(n) < p

  # --- 1. Errores de digitación en valores (antes de pasar a texto) -----------
  sitios_err <- sample(unique(tabla$id_sitio), 8)
  fila_de <- function(s) sample(which(tabla$id_sitio == s), 1)

  # cobertura con un dígito extra (85.0 -> 850) en una fila
  k <- fila_de(sitios_err[1]); tabla$cob_veg_riberena_pct[k] <- tabla$cob_veg_riberena_pct[k] * 10
  k <- fila_de(sitios_err[2]); tabla$cob_dosel_pct[k] <- tabla$cob_dosel_pct[k] * 10 + 1
  # coordenadas intercambiadas en una fila
  k <- fila_de(sitios_err[3])
  tmp <- tabla$utm_este[k]; tabla$utm_este[k] <- tabla$utm_norte[k]; tabla$utm_norte[k] <- tmp
  # dígito faltante en la coordenada norte
  k <- fila_de(sitios_err[4]); tabla$utm_norte[k] <- floor(tabla$utm_norte[k] / 10)
  # ancho de cauce con coma mal puesta (12.5 -> 125)
  k <- fila_de(sitios_err[5]); tabla$ancho_cauce_m[k] <- tabla$ancho_cauce_m[k] * 10
  # altitud con dígito extra
  k <- fila_de(sitios_err[6]); tabla$altitud_msnm[k] <- tabla$altitud_msnm[k] * 10

  # temperatura sin coma decimal en UNA fila de una visita (recuperable)
  v <- sample(unique(tabla$id_visita), 3)
  k <- sample(which(tabla$id_visita == v[1]), 1); tabla$temperatura_c[k] <- tabla$temperatura_c[k] * 10
  # temperatura fuera de rango en TODA la visita (no recuperable -> NA)
  k <- which(tabla$id_visita == v[2]); tabla$temperatura_c[k] <- tabla$temperatura_c[k] * 10
  # lluvia negativa en toda una visita (no recuperable -> NA)
  k <- which(tabla$id_visita == v[3]); tabla$lluvia_48h_mm[k] <- -tabla$lluvia_48h_mm[k] - 1

  # faltantes reales (olvidos en terreno), ~2 %
  for (var in c("temperatura_c", "cob_dosel_pct", "pendiente_ribera_grados", "horas_busqueda")) {
    tabla[[var]][azar(0.02)] <- NA
  }

  # --- 2. Pasar todo a texto con formato de planilla -------------------------
  a_texto_num <- function(x) {
    s <- format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
    s[is.na(x)] <- NA
    con_coma <- !azar(0.07)                       # 7 % de los decimales quedan con punto
    s[con_coma] <- gsub(".", ",", s[con_coma], fixed = TRUE)
    s
  }
  fmt_fecha <- function(x) {
    u <- runif(n)
    s <- ifelse(u < 0.70, format(x, "%d/%m/%Y"),
         ifelse(u < 0.92, format(x, "%Y-%m-%d"),
                paste0(as.integer(format(x, "%d")), "-", as.integer(format(x, "%m")), "-", format(x, "%y"))))
    s[is.na(x)] <- NA
    s
  }
  fmt_hora <- function(x) {
    u <- runif(n)
    sin_cero <- sub("^0", "", x)
    ifelse(u < 0.80, x, ifelse(u < 0.90, sin_cero, ifelse(u < 0.96, sub(":", "", x), sub(":", ".", x))))
  }
  variantes <- function(x, mapa, p = 0.15) {
    # mapa: lista nombrada valor_limpio -> c(forma_base, variante1, variante2, ...)
    out <- x
    for (val in names(mapa)) {
      k <- which(x == val)
      if (length(k) == 0) next
      formas <- mapa[[val]]
      out[k] <- formas[1]
      cambia <- k[runif(length(k)) < p]
      if (length(formas) > 1 && length(cambia) > 0)
        out[cambia] <- sample(formas[-1], length(cambia), replace = TRUE)
    }
    out
  }

  crudo <- tabla
  for (var in names(crudo)) {
    tp <- tipo[[var]]
    x <- crudo[[var]]
    crudo[[var]] <- switch(tp,
      "numérico" = a_texto_num(x),
      "entero"   = ifelse(is.na(x), NA, as.character(x)),
      "binaria"  = ifelse(is.na(x), NA, as.character(x)),
      "fecha"    = fmt_fecha(x),
      "hora"     = fmt_hora(x),
      as.character(x))
  }

  # --- 3. Variantes en categorías ---------------------------------------------
  crudo$metodo <- variantes(crudo$metodo, list(
    huellas       = c("Huellas", "huellas", "HUELLAS", "huella"),
    fecas         = c("Fecas", "fecas ", "FECAS", "feca"),
    camara_trampa = c("Cámara trampa", "camara trampa", "Camara", "CT"),
    avistamiento  = c("Avistamiento", "avistamiento", "Avist.", "obs. directa")))
  crudo$comuna <- variantes(crudo$comuna, list(
    "Villarrica" = c("Villarrica", "Villarica", "VILLARRICA", "villarrica "),
    "Pucón"      = c("Pucón", "Pucon", "PUCON", "pucón"),
    "Curarrehue" = c("Curarrehue", "Curarehue", "CURARREHUE", "curarrehue ")))
  crudo$observador <- variantes(crudo$observador, list(
    obs_A = c("obs_A", "OBS_A", "Obs A", "obs a"),
    obs_B = c("obs_B", "OBS_B", "Obs B", "obs b"),
    obs_C = c("obs_C", "OBS_C", "Obs C", "obs c")))
  crudo$confiabilidad <- variantes(crudo$confiabilidad, list(
    baja = c("Baja", "baja", "BAJA", "baja "), media = c("Media", "media", "MEDIA", "med."),
    alta = c("Alta", "alta", "ALTA", "alta ")), p = 0.25)
  crudo$tipo_evidencia <- variantes(crudo$tipo_evidencia, list(
    huella = c("Huella", "huella", "huellas"), fecas = c("Fecas", "feca", "fecas"),
    foto = c("Foto", "foto", "fotografía"),
    observacion_directa = c("Observación directa", "obs directa", "Avistamiento")))
  crudo$estado_camara <- variantes(crudo$estado_camara, list(
    instalacion = c("Instalación", "instalacion"), operativa = c("Operativa", "operativa", "OK"),
    falla_parcial = c("Falla parcial", "falla parcial"), falla_total = c("Falla total", "falla total")))
  crudo$nivel_rio <- variantes(crudo$nivel_rio, list(
    bajo = c("Bajo", "bajo", "BAJO"), medio = c("Medio", "medio", "normal"),
    alto = c("Alto", "alto", "ALTO", "crecido")))
  crudo$rio <- ifelse(azar(0.08), tolower(crudo$rio), crudo$rio)

  for (var in dicc$variable[dicc$tipo == "binaria"]) {
    crudo[[var]] <- variantes(crudo[[var]], list(
      "1" = c("1", "si", "Sí", "SI"), "0" = c("0", "no", "No", "NO")), p = 0.10)
  }

  # --- 4. Códigos de dato faltante --------------------------------------------
  # Faltantes "estructurales" (la variable no aplica a ese método o no hubo
  # detección) suelen quedar en blanco; los faltantes reales (dato no tomado)
  # aparecen con distintos códigos.
  estructurales <- c("id_camara", "fecha_inicio_camara", "tipo_evidencia", "confiabilidad",
                     "n_evidencias", "longitud_tramo_m", "horas_busqueda", "dias_camara",
                     "estado_camara", "motivo_falla_camara", "sustrato_busqueda", "deteccion",
                     "presencia_nutria", "presencia_zorro", "presencia_perro",
                     "presencia_ganado_ribera")
  for (var in names(crudo)) {
    k <- which(is.na(crudo[[var]]))
    if (length(k) == 0) next
    codigos <- if (var %in% estructurales) {
      setNames(c(0.85, 0.15), c("", "NA"))
    } else if (tipo[[var]] %in% c("numérico", "entero", "binaria")) {
      setNames(c(0.45, 0.15, 0.30, 0.10), c("", "NA", "-999", "s/d"))
    } else {
      setNames(c(0.60, 0.20, 0.20), c("", "NA", "s/d"))
    }
    crudo[[var]][k] <- sample(names(codigos), length(k), replace = TRUE, prob = codigos)
  }

  # --- 5. Filas duplicadas -----------------------------------------------------
  dup <- sample(n, 3)
  orden <- sort(c(seq_len(n), dup))
  crudo <- crudo[orden, ]

  names(crudo) <- dicc$nombre_raw[match(names(crudo), dicc$variable)]
  crudo
}
