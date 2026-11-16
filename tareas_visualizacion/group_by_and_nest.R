
# Trabajo final: group_by / nest / map con datos de visón
library(tidyverse)
library(broom)

vison <- limpio
# ¿Qué grupos tengo y cuántas detecciones hay en cada uno?
vison |>
  filter(!is.na(deteccion)) |>
  group_by(comuna, metodo) |>
  summarise(n = n(), detecciones = sum(deteccion), .groups = "drop") |>
  #arrange(comuna, desc(detecciones)) |>
  print(n = Inf)

# Paso 2: preparar datos
# - saco filas sin deteccion (cámaras en instalación o con falla total)
# - saco filas sin distancia por río al centro (el modelo igual las descartaría)
# - log10 de la distancia, porque va de 65 a 29.000 m
# - me quedo solo con grupos (comuna x río) con >= 5 detecciones
vison_mod <- vison |>
  filter(!is.na(deteccion), !is.na(dist_rio_centro_m)) |>
  mutate(log_dist = log10(dist_rio_centro_m)) |>
  group_by(comuna, metodo) |>
  filter(sum(deteccion) >= 5) |>
  ungroup()

# Chequeo: ¿cuántos grupos y de qué tamaño quedaron?
vison_mod |>
  group_by(comuna, metodo) |>
  summarise(n = n(), detecciones = sum(deteccion), .groups = "drop")

# Paso 3: anidar. Cada fila = un río; la columna `data` guarda su mini base de datos
#pénsarlo como carpetas por rios.
by_metodo <- vison_mod |>
  group_by(comuna, metodo) |>
  nest() |>
  ungroup()

by_metodo

# Paso 4: una función que ajusta el modelo, y map para aplicarla a cada río
# (la regla de la clase: el mismo modelo 12 veces -> una función, no 12 copias)
ajustar_logit <- function(df) {
  glm(deteccion ~ log_dist, data = df, family = binomial)
}

by_metodo<- by_metodo |>
  mutate(modelo = map(data, ajustar_logit))

by_metodo  # ahora hay una columna `modelo` con un glm por fila

# Paso 5: extraer información de cada modelo
by_metodo <- by_metodo |>
  mutate(
    glance  = map(modelo, glance),
    tidy    = map(modelo, tidy),
    augment = map(modelo, augment, type.predict = "response")
  )
by_metodo

by_metodo <- by_metodo |>
  mutate(pseudo_r2 = map_dbl(glance, \(g) 1 - g$deviance / g$null.deviance),
         grupo     = paste(metodo, comuna, sep = " · "))
by_metodo

# glance: una fila por modelo. glm no da R², así que calculo un pseudo-R²
# (1 - devianza / devianza nula): cuánto mejora el modelo respecto a no usar predictor
resumen <- by_metodo |>
  select(comuna, metodo, glance) |>
  unnest(glance) |>
  mutate(pseudo_r2 = 1 - deviance / null.deviance)

# otros -------------------------------------------------------------------



# tidy: me quedo con la pendiente de log_dist (el efecto de la distancia)
coefs <- by_metodo |>
  select(comuna, metodo, tidy) |>
  unnest(tidy) |>
  filter(term == "log_dist")

# augment: valores ajustados por observación (probabilidad predicha)
ajustes <- by_metodo |>
  select(comuna, metodo, augment) |>
  unnest(augment)

resumen |> select(comuna, metodo, nobs, null.deviance, deviance, AIC, pseudo_r2)
coefs   |> select(comuna, metodo, estimate, std.error, p.value)
head(ajustes)


# diagnostico -------------------------------------------------------------



# Diagnóstico: por qué NO sirve agrupar por río
# (filas = visitas repetidas; sitios = valores distintos del predictor)
vison_mod |>
  group_by(comuna, rio) |>
  summarise(filas = n(), sitios = n_distinct(id_sitio),
            detecciones = sum(deteccion), .groups = "drop")

# Los pasos 4 y 5 empaquetados en una función
ajustar_todo <- function(nested) {
  nested |>
    mutate(
      modelo  = map(data, ajustar_logit),
      glance  = map(modelo, glance),
      tidy    = map(modelo, tidy),
      augment = map(modelo, augment, type.predict = "response")
    )
}

# Nueva agrupación: método x comuna (sin avistamiento)
by_metodo <- vison |>
  filter(!is.na(deteccion), !is.na(dist_rio_centro_m),
         metodo != "avistamiento") |>
  mutate(log_dist = log10(dist_rio_centro_m)) |>
  group_by(metodo, comuna) |>
  nest() |>
  ungroup() |>
  ajustar_todo()

resumen_m <- by_metodo |>
  select(metodo, comuna, glance) |>
  unnest(glance) |>
  mutate(pseudo_r2 = 1 - deviance / null.deviance)

coefs_m <- by_metodo |>
  select(metodo, comuna, tidy) |>
  unnest(tidy) |>
  filter(term == "log_dist")

resumen_m |> select(metodo, comuna, nobs, deviance, AIC, pseudo_r2)
coefs_m   |> select(metodo, comuna, estimate, std.error, p.value)

library(forcats)

# 6b. Exploratorio ANTES de modelar (el "spaghetti" de la lámina):
# una curva logística por grupo método x comuna
vison |>
  filter(!is.na(deteccion), !is.na(dist_rio_centro_m), metodo != "avistamiento") |>
  mutate(log_dist = log10(dist_rio_centro_m)) |>
  ggplot(aes(x = log_dist, y = deteccion,
             group = interaction(metodo, comuna),
             colour = metodo, linetype = comuna)) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(x = "log10(distancia por río al centro)", y = "Probabilidad de detección") +
  theme_minimal()

# 7a. R² ordenado, un punto por grupo (¿patrón por método o comuna?)
by_metodo |>
  ggplot(aes(x = pseudo_r2, y = fct_reorder(grupo, pseudo_r2), colour = comuna)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(x = expression(psudo-R^2), y = NULL, colour = "Comuna") +
  theme_classic(14)

# 7b. Pendientes con intervalo aproximado (±1,96 EE); a la izquierda del 0 = menos detección lejos del centro
ggplot(coefs_m, aes(x = estimate, y = metodo, color = comuna)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = estimate - 1.96 * std.error,
                      xmax = estimate + 1.96 * std.error),
                  position = position_dodge(width = 0.5)) +
  labs(x = "Pendiente de log10(distancia por río al centro)", y = NULL,
       color = "Comuna") +
  theme_minimal()

# 7c. Los 3 peores ajustes, directo con slice_min (como en la clase)
ß