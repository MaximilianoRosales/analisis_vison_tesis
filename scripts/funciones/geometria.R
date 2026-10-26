# =============================================================================
# Funciones geométricas auxiliares (coordenadas en metros, UTM 19S)
# Se trabaja directamente con matrices de coordenadas para no depender de
# funciones de referenciación lineal que no están en todas las versiones de sf.
# =============================================================================

# Distancia acumulada a lo largo de una línea (matriz n x 2)
dist_acumulada <- function(xy) {
  c(0, cumsum(sqrt(rowSums(diff(xy)^2))))
}

# Punto (y dirección tangente) a una distancia d desde el inicio de la línea
punto_a_distancia <- function(xy, d) {
  acum <- dist_acumulada(xy)
  d <- min(max(d, 0), max(acum))
  i <- findInterval(d, acum, rightmost.closed = TRUE)
  i <- min(max(i, 1), nrow(xy) - 1)
  largo_seg <- acum[i + 1] - acum[i]
  f <- if (largo_seg > 0) (d - acum[i]) / largo_seg else 0
  p <- xy[i, ] + f * (xy[i + 1, ] - xy[i, ])
  t <- (xy[i + 1, ] - xy[i, ]) / max(largo_seg, 1e-9)
  list(punto = p, tangente = t)
}

# Tramo de línea entre las distancias d_ini y d_fin (matriz de coordenadas)
subcadena <- function(xy, d_ini, d_fin) {
  acum <- dist_acumulada(xy)
  interior <- which(acum > d_ini & acum < d_fin)
  rbind(punto_a_distancia(xy, d_ini)$punto,
        xy[interior, , drop = FALSE],
        punto_a_distancia(xy, d_fin)$punto)
}

# Proyecta un punto p sobre una línea: devuelve la distancia a lo largo de la
# línea (desde su inicio) del punto más cercano y la distancia perpendicular
proyectar_en_linea <- function(xy, p) {
  a <- xy[-nrow(xy), , drop = FALSE]
  b <- xy[-1, , drop = FALSE]
  ab <- b - a
  l2 <- rowSums(ab^2)
  t <- ((p[1] - a[, 1]) * ab[, 1] + (p[2] - a[, 2]) * ab[, 2]) / pmax(l2, 1e-12)
  t <- pmin(pmax(t, 0), 1)
  proy <- a + t * ab
  d2 <- (proy[, 1] - p[1])^2 + (proy[, 2] - p[2])^2
  k <- which.min(d2)
  acum <- dist_acumulada(xy)
  list(dist_a_lo_largo = acum[k] + t[k] * sqrt(l2[k]), dist_perp = sqrt(d2[k]))
}

# k puntos aleatorios sobre un conjunto de líneas sf (probabilidad
# proporcional a la longitud). Devuelve matriz k x 2.
puntos_en_lineas <- function(lineas, k) {
  geoms <- suppressWarnings(st_geometry(st_cast(st_cast(lineas, "MULTILINESTRING"), "LINESTRING")))
  largos <- as.numeric(st_length(geoms))
  idx <- sample(length(geoms), k, replace = TRUE, prob = largos)
  t(vapply(seq_len(k), function(j) {
    xy <- st_coordinates(geoms[[idx[j]]])[, 1:2, drop = FALSE]
    punto_a_distancia(xy, runif(1, 0, largos[idx[j]]))$punto
  }, numeric(2)))
}

# Matriz de coordenadas -> sf de puntos
a_sf <- function(xy, crs = 32719) {
  st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = crs)
}

# Inserta el punto p como vértice de la línea xy (en el segmento más cercano)
insertar_vertice <- function(xy, p) {
  a <- xy[-nrow(xy), , drop = FALSE]
  b <- xy[-1, , drop = FALSE]
  ab <- b - a
  l2 <- rowSums(ab^2)
  t <- ((p[1] - a[, 1]) * ab[, 1] + (p[2] - a[, 2]) * ab[, 2]) / pmax(l2, 1e-12)
  t <- pmin(pmax(t, 0), 1)
  proy <- a + t * ab
  k <- which.min((proy[, 1] - p[1])^2 + (proy[, 2] - p[2])^2)
  if (any(abs(xy[, 1] - p[1]) < 1e-6 & abs(xy[, 2] - p[2]) < 1e-6)) return(xy)
  rbind(xy[1:k, , drop = FALSE], p, xy[(k + 1):nrow(xy), , drop = FALSE])
}

# Cierra brechas pequeñas en una red de líneas: cada extremo que no toca otra
# línea pero está a menos de 'tolerancia' m de una, se prolonga hasta el punto
# más cercano de esa línea, y ese punto se agrega como vértice de la línea
# receptora (así ambas comparten exactamente el nodo de confluencia).
cerrar_brechas <- function(lineas, tolerancia = 75) {
  g <- st_geometry(lineas)
  n <- length(g)
  coords <- lapply(g, function(l) unclass(l)[, 1:2, drop = FALSE])
  extremos <- rbind(t(vapply(coords, function(m) m[1, ], numeric(2))),
                    t(vapply(coords, function(m) m[nrow(m), ], numeric(2))))
  linea_de <- rep(seq_len(n), 2)
  es_inicio <- rep(c(TRUE, FALSE), each = n)
  pts <- a_sf(extremos, crs = st_crs(lineas))
  cerca <- st_is_within_distance(pts, g, dist = tolerancia)
  n_cerradas <- 0
  for (k in seq_along(cerca)) {
    otras <- setdiff(cerca[[k]], linea_de[k])
    if (length(otras) == 0) next
    g_otras <- st_sfc(lapply(coords[otras], st_linestring), crs = st_crs(lineas))
    d <- as.numeric(st_distance(st_geometry(pts)[k], g_otras))
    if (min(d) < 0.01) next                     # ya está conectado
    j <- otras[which.min(d)]
    destino <- st_coordinates(st_nearest_points(st_geometry(pts)[k], g_otras[which.min(d)]))[2, 1:2]
    coords[[j]] <- insertar_vertice(coords[[j]], destino)
    m <- coords[[linea_de[k]]]
    coords[[linea_de[k]]] <- if (es_inicio[k]) rbind(destino, m) else rbind(m, destino)
    n_cerradas <- n_cerradas + 1
  }
  message("  brechas cerradas en la red hídrica: ", n_cerradas)
  st_geometry(lineas) <- st_sfc(lapply(coords, function(m) st_linestring(unname(m))), crs = st_crs(lineas))
  lineas
}
