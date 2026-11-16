
# Trabajo final: group_by / nest / map con datos de visón
library(tidyverse)
library(broom)

vison <- limpio

base <- vison |>
  filter(!is.na(deteccion), !is.na(dist_rio_centro_m)) |>
  mutate(log_dist = log10(dist_rio_centro_m))

base |>
  group_by(metodo, comuna) |>
  summarise(filas = n(), sitios = n_distinct(id_sitio),
            detecciones = sum(deteccion), .groups = "drop")

base_metodo <- base |> filter(metodo != "avistamiento")

ajustar_logit <- function(df) {
  glm(deteccion ~ log_dist, data = df, family = binomial)
}

ajustar_todo <- function(nested) {
  nested |>
    mutate(
      modelo    = map(data, ajustar_logit),
      glance    = map(modelo, glance),
      tidy      = map(modelo, tidy),
      augment   = map(modelo, augment, type.predict = "response"),
      pseudo_r2 = map_dbl(glance, \(g) 1 - g$deviance / g$null.deviance)
    )
}
base_metodo |>
  ggplot(aes(x = log_dist, y = deteccion,
             group = interaction(metodo, comuna),
             colour = metodo, linetype = comuna)) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "log10(distancia por río al centro)", y = "Probabilidad de detección",
       colour = "Método", linetype = "Comuna") +
  theme_minimal()

by_metodo <- base_metodo |>
  group_by(metodo, comuna) |>
  nest() |>
  ungroup() |>
  ajustar_todo() |>
  mutate(grupo = paste(metodo, comuna, sep = " · "))
by_metodo

by_metodo |> select(metodo, comuna, data, modelo)

by_metodo |>
  select(grupo, glance) |>
  unnest(glance) |>
  select(grupo, nobs, null.deviance, deviance, AIC)

coefs_m <- by_metodo |>
  select(metodo, comuna, grupo, tidy) |>
  unnest(tidy) |>
  filter(term == "log_dist") |>
  mutate(razon_odds_x10 = exp(estimate))

coefs_m |> select(grupo, estimate, std.error, p.value, razon_odds_x10)

by_metodo |>
  select(grupo, augment) |>
  unnest(augment) |>
  head()

by_metodo |>
  ggplot(aes(x = pseudo_r2, y = fct_reorder(grupo, pseudo_r2), colour = comuna)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(x = expression(R^2), y = NULL, colour = "Comuna") +
  theme_classic(14)

coefs_m |>
  ggplot(aes(x = estimate, y = metodo, colour = comuna)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = estimate - 1.96 * std.error,
                      xmax = estimate + 1.96 * std.error),
                  position = position_dodge(width = 0.5)) +
  labs(x = "Pendiente de log10(distancia por río al centro)", y = NULL,
       colour = "Comuna") +
  theme_minimal()

by_metodo |>
  slice_min(pseudo_r2, n = 12) |>
  unnest(augment) |>
  ggplot(aes(x = log_dist, y = deteccion)) +
  geom_jitter(height = 0.05, alpha = 0.5) +
  geom_line(aes(y = .fitted), colour = "red") +
  facet_wrap(vars(grupo)) +
  labs(x = "log10(distancia por río al centro)", y = "Detección (0/1)") +
  theme_minimal()ß