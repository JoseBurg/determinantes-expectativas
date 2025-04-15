source("work_data.R")
library(moments)
# Funciones ---------------------------------------------------------------

# ------------------------------ Funcion de impulso respuesta

impulso_respuestas <- function(data, var_impulso, var_reponse, ahead = 15){
  
  # Convertir data a serie de tiempo
  ts_data <- ts(data |> 
                  dplyr::select(all_of(var_impulso), all_of(var_reponse)) |> 
                  tidyr::drop_na(), 
                        start = c(min(lubridate::year(data$fecha)), 1), frequency = 12)
  # Data en primera diferencia
  ts_data_diff <- diff(ts_data)
  
  # Modelo var
  # Selección de rezagos optimos
  lag_selection <- vars::VARselect(ts_data_diff, lag.max = 10, type = "const")
  optimal_lag <- lag_selection$selection["AIC(n)"]
  
  
  var_model <- vars::VAR(ts_data_diff, p = optimal_lag, type = "const")
  irf_object <- vars::irf(var_model, impulse = var_impulso, response = var_reponse, n.ahead = ahead, boot = TRUE)
  
  # Convertir datos de series de tiempo a data.frame:
  data <-  data.frame(
    periodo = 1:length(irf_object$irf[[var_impulso]]),
    impulso = as.numeric(irf_object$irf[[var_impulso]]),
    lower = as.numeric(irf_object$Lower[[var_impulso]]),
    upper = as.numeric(irf_object$Upper[[var_impulso]])) |>
    tidyr::drop_na()
  
  # Gráfico
  data |> 
    ggplot2::ggplot(ggplot2::aes(x = periodo, y = impulso)) +
    ggplot2::geom_line(color = "black", size = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "gray50", alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = paste0("Impacto de ", var_impulso, " en las expectativas \n a 12 meses ", var_reponse),
      x = "Meses", y = "Impacto en Exp. Inflación") +
    theme_em()
}

# ------------------------------ Modelo de asimetría

modelo_asimetria <- function(data, variable){
  
  checkmate::check_choice(variable, c("exp_inf", "otros"))
  
  infl_expectativas <- data.frame(
    meta = data[["meta"]],
    expectativas = data[[variable]],
    inflacion = data[["dl4_ipc"]]) |> 
    tibble::as_tibble()
  
  desviaciones <- infl_expectativas |> 
    dplyr::mutate(
      dev_pos = pmax(inflacion - meta, 0),
      dev_neg = pmax(meta - inflacion, 0)) |> 
    tidyr::drop_na()
  
  modelo <- lm(expectativas ~ dev_pos + dev_neg, data = desviaciones)
  
  modelo
}


# Analisis  ---------------------------------------------------------------

# Modelo de asimetría

tipo_inflacion <- c("exp_inf", "bancos", "organismos_multilaterales", "consultores", "otros")

estimacion_inflacion_general <- list()

for (i in tipo_inflacion) {
  modelo_general <- modelo_asimetria(data = datos_exp_infl, variable = i)
  
  estimacion_inflacion_general[[i]] <- c(modelo_general$coefficients[-1], rsuared = summary(modelo_general)$r.squared)
}

estimacion_infl_alt <- list()

for (i in tipo_inflacion) {
  modelo_general <- modelo_asimetria(data = datos_exp_infl |> filter(dl4_ipc > mean(dl4_ipc, na.rm = TRUE)), variable = i)
  
  estimacion_infl_alt[[i]] <- c(modelo_general$coefficients[-1], rsuared = summary(modelo_general)$r.squared)
}

estimacion_infl_baja <- list()

for (i in tipo_inflacion) {
  modelo_general <- modelo_asimetria(data = datos_exp_infl |> filter(dl4_ipc > mean(dl4_ipc, na.rm = TRUE)), variable = i)
  
  estimacion_infl_baja[[i]] <- c(modelo_general$coefficients[-1], rsuared = summary(modelo_general)$r.squared)
}


regresiones <- list(
  inflacion_general = estimacion_inflacion_general,
  inflacion_alta = estimacion_infl_alt,
  inflacion_baja = estimacion_infl_baja
)


# Estadisticos descriptivos
data_general <- datos_exp_infl
data_alta_inflacion <- datos_exp_infl|> filter(dl4_ipc > mean(dl4_ipc, na.rm = TRUE))
data_baja_inflacion <- datos_exp_infl|> filter(dl4_ipc < mean(dl4_ipc, na.rm = TRUE))

estadisticos_general <- list()
for (i in tipo_inflacion) {
  data <-  data_general
  medidas <- data.frame(
    mediana = median(data[[i]], na.rm = TRUE),
    media = mean(data[[i]], na.rm = TRUE),
    varianza = var(data[[i]], na.rm = TRUE),
    coeficiente_cor = cor(data[["dl4_ipc"]], data[[i]], use = "complete.obs")
  )
  
  estadisticos_general[[i]] <- medidas
}

estadisticos_alt_inf <- list()
for (i in tipo_inflacion) {
  data <-  data_alta_inflacion
  medidas <- data.frame(
    mediana = median(data[[i]], na.rm = TRUE),
    media = mean(data[[i]], na.rm = TRUE),
    varianza = var(data[[i]], na.rm = TRUE),
    coeficiente_cor = cor(data[["dl4_ipc"]], data[[i]], use = "complete.obs")
  )
  
  estadisticos_alt_inf[[i]] <- medidas
}


estadisticos_baj_inf <- list()

for (i in tipo_inflacion) {
  data <-  data_baja_inflacion
  medidas <- data.frame(
    mediana = median(data[[i]], na.rm = TRUE),
    media = mean(data[[i]], na.rm = TRUE),
    varianza = var(data[[i]], na.rm = TRUE),
    coeficiente_cor = cor(data[["dl4_ipc"]], data[[i]], use = "complete.obs")
  )
  
  estadisticos_baj_inf[[i]] <- medidas
}

todos_escenarios <- list(
  general = estadisticos_general,
  alta = estadisticos_alt_inf,
  baja = estadisticos_baj_inf
)


# Funciones de impulso respuesta ------------------------------------------

impulso_respuestas(data = datos_exp_infl, var_impulso = "dl4_ipc", var_reponse = "exp_inf", ahead = 12)
impulso_respuestas(data = datos_exp_infl, var_impulso = "dl4_ipc", var_reponse = "bancos", ahead = 15)
impulso_respuestas(data = datos_exp_infl, var_impulso = "dl4_ipc", var_reponse = "organismos_multilaterales", ahead = 25)
impulso_respuestas(data = datos_exp_infl, var_impulso = "dl4_ipc", var_reponse = "consultores", ahead = 9)
impulso_respuestas(data = datos_exp_infl, var_impulso = "dl4_ipc", var_reponse = "otros", ahead = 10)

impulso_respuestas(data = datos_exp_infl, var_impulso = "tia_180", var_reponse = "exp_inf")
impulso_respuestas(data = datos_exp_infl, var_impulso = "tia_180", var_reponse = "bancos", ahead = 15)
impulso_respuestas(data = datos_exp_infl, var_impulso = "tia_180", var_reponse = "organismos_multilaterales", ahead = 8)
impulso_respuestas(data = datos_exp_infl, var_impulso = "tia_180", var_reponse = "consultores", ahead = 9)
impulso_respuestas(data = datos_exp_infl, var_impulso = "tia_180", var_reponse = "otros", ahead = 9)


# Estadisticos descriptivos en diferentes escenario -----------------------
grupos <- c("bancos", "organismos_multilaterales", "consultores", "otros")


# General
tabla_resumen <- function(data) {map_dfr(grupos, function(col) {
  
  x <- data[[col]]
  ref <- data$exp_inf
  
  x_clean <- x[!is.na(x)]
  
  tibble(
    grupo = col,
    media = mean(x, na.rm = TRUE),
    varianza = var(x, na.rm = TRUE),
    coef_var = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    correlacion = cor(x, ref, use = "complete.obs"),
    autocor_1 = cor(x_clean[-length(x_clean)], x_clean[-1], use = "complete.obs"),
    asimetria = skewness(x, na.rm = TRUE),
    curtosis = kurtosis(x, na.rm = TRUE),
    rango_intercuartil = IQR(x, na.rm = TRUE)
  )
})}

tabla_resumen(data = expec_group) |> 
  clipr::write_clip()

# Datos Escenarios:
alta_vix <- expec_group |> # Alta volatilidad 
  filter(vixcls > quantile(vixcls, 0.7))

baja_vix <- expec_group |> # Baja volatilidad 
  filter(vixcls < quantile(vixcls, 0.7))

alta_inflacion <- expec_group |> # Alta inflacionk
  filter(dl4_ipc > mean(dl4_ipc, na.rm  = TRUE))

baja_inflacion <- expec_group |> # Baja inflacionk
  filter(dl4_ipc < mean(dl4_ipc, na.rm  = TRUE))

bf_covid <- expec_group[c(70:127),] # antes del covid
af_covid <- expec_group[c(128:186),] # Después del covid

# Alta volatilidad
tabla_resumen(alta_vix) |>
  clipr::write_clip()
# Baja volatilidad
tabla_resumen(baja_vix) |> clipr::write_clip()

# Alta y baja inflacion ------------
# Alta inflacion
tabla_resumen(alta_inflacion) |> clipr::write_clip()
# Baja Inflación
tabla_resumen(baja_inflacion) |> clipr::write_clip()

# pre y post covid ------------
# Antes del covid
tabla_resumen(bf_covid) |> clipr::write_clip()

# Después del covid
tabla_resumen(af_covid) |> clipr::write_clip()


# Datos con deltas, en diferentes Escenarios:--------------
deltas <- expec_group |> 
  mutate(
    across(c(bancos:otros),
           \(x) x - lag(x))
  )

alta_vix <- deltas |> # Alta volatilidad 
  filter(vixcls > quantile(vixcls, 0.7))

baja_vix <- deltas |> # Baja volatilidad 
  filter(vixcls < quantile(vixcls, 0.7))

alta_inflacion <- deltas |> # Alta inflacionk
  filter(dl4_ipc > mean(dl4_ipc, na.rm  = TRUE))

baja_inflacion <- deltas |> # Baja inflacionk
  filter(dl4_ipc < mean(dl4_ipc, na.rm  = TRUE))

bf_covid <- deltas[c(70:127),] # antes del covid
af_covid <- deltas[c(128:186),] # Después del covid

# General
tabla_resumen(deltas) |> 
  clipr::write_clip()

# Alta volatilidad
tabla_resumen(alta_vix) |> clipr::write_clip()
# Baja volatilidad
tabla_resumen(baja_vix) |> clipr::write_clip()

# Alta y baja inflacion ------------
# Alta inflacion
tabla_resumen(alta_inflacion) |> clipr::write_clip()
# Baja Inflación
tabla_resumen(baja_inflacion) |> clipr::write_clip()

# pre y post covid ------------
# Antes del covid
tabla_resumen(bf_covid) |> clipr::write_clip()

# Después del covid
tabla_resumen(af_covid) |> clipr::write_clip()



# Sesgo sistematico -------------------------------------------------------

sesgo_sistematico <- function(data){
  data |> 
    dplyr::summarise(
      dplyr::across(c(bancos:otros),
                    \(x){mean(x, na.rm = TRUE) - mean(exp_inf, na.rm = TRUE)})
    )
}
expec_group |>
  sesgo_sistematico() |> 
  clipr::write_clip()

alta_vix |>
  sesgo_sistematico() |> 
  clipr::write_clip()

baja_vix |>
  sesgo_sistematico() |> 
  clipr::write_clip()


alta_inflacion |>
  sesgo_sistematico() |> 
  clipr::write_clip()

baja_inflacion |>
  sesgo_sistematico() |> 
  clipr::write_clip()
  
bf_covid |> 
  sesgo_sistematico() |> 
  clipr::write_clip()

af_covid |> 
  sesgo_sistematico() |> 
  clipr::write_clip()

# Persistencia ------------------------------------------------------------
# ---------------------------------------------
# PASO 1: Cargar paquetes necesarios
# ---------------------------------------------
library(dplyr)
library(zoo)
library(purrr)
library(broom)
library(moments)
library(tibble)
library(ggplot2)
library(tidyr)

# ---------------------------------------------
# PASO 2: Definir función para calcular persistencia (AR(1)) móvil
# ---------------------------------------------
calc_persistencia <- function(serie, width = 12) {
  serie_zoo <- zoo(serie)
  rollapply(
    data = serie_zoo,
    width = width,
    FUN = function(x) {
      y <- x[-1]
      x_lag <- x[-length(x)]
      modelo <- lm(y ~ x_lag)
      coef(modelo)[2]  # rho
    },
    by = 1,
    align = "right",
    fill = NA
  )
}

# ---------------------------------------------
# PASO 3: Calcular persistencia para todos los grupos
# ---------------------------------------------
grupos <- c("bancos", "organismos_multilaterales", "consultores", "otros")

persistencias <- map_dfc(grupos, ~calc_persistencia(expec_group[[.x]]))
names(persistencias) <- grupos
persistencias$fecha <- tail(expec_group$fecha, nrow(persistencias))

# ---------------------------------------------
# PASO 4: Calcular el sesgo sistemático en la persistencia (u = rho - mean(rho))
# ---------------------------------------------
persistencias_u <- persistencias %>%
  dplyr::select(-fecha) %>%
  mutate(across(everything(), ~ . - mean(., na.rm = TRUE)))

# ---------------------------------------------
# PASO 5: Clasificar el entorno: alta/baja volatilidad e inflación
# ---------------------------------------------
vol_corte <- quantile(vixcls, 0.7, na.rm = TRUE)
inf_corte <- mean(expec_group$dl4_ipc, na.rm = TRUE)

condiciones <- tibble(
  fecha = expec_group$fecha,
  alta_vol = expec_group$vixcls > vol_corte,
  alta_inf = expec_group$dl4_ipc > inf_corte
)

# ---------------------------------------------
# PASO 6: Unir todo y calcular medias del sesgo de persistencia por condición
# ---------------------------------------------
df_full <- cbind(condiciones, persistencias_u)

tabla_resultado <-
  df_full |> 
    rename("organimosMultilaterales" = organismos_multilaterales) |>
  summarise(
    across(bancos:otros, list(
      total = ~mean(., na.rm = TRUE),
      alta_vol = ~mean(.[alta_vol], na.rm = TRUE),
      baja_vol = ~mean(.[!alta_vol], na.rm = TRUE),
      alta_inf = ~mean(.[alta_inf], na.rm = TRUE),
      baja_inf = ~mean(.[!alta_inf], na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) |> 
  as_tibble() |>
  pivot_longer(everything(), names_to = "nombre_variable", values_to = "valor") |> 
    separate(nombre_variable, into = c("grupo", "condicion1", "condicion2"), fill = "right", sep = "_") |> 
    mutate(
      condicion = ifelse(is.na(condicion2), condicion1, paste(condicion1, condicion2, sep = "_"))
    ) %>%
    dplyr::select(grupo, condicion, valor) |> 
    pivot_wider(
      id_cols = grupo, 
      names_from = condicion,
      values_from = valor
    )

tabla_resultado |> clipr::write_clip()
# ---------------------------------------------
# PASO 7 (Opcional): Visualizar resultados
# ---------------------------------------------
ggplot(tabla_resultado, aes(x = grupo, y = total)) +
  geom_col(fill = "steelblue") +
  labs(title = "Sesgo sistemático promedio en la persistencia",
       x = "Grupo",
       y = "Sesgo de persistencia promedio") +
  theme_minimal()

# Puedes visualizar también otras condiciones como alta_vol, alta_inf, etc.

















# Elasticidad a nueva información ---------------------------------------

data_modelos <- expec_group |> 
  mutate(
    d_inflacion = dl4_ipc - lag(dl4_ipc),
    dumm_inflacion = dl4_ipc > mean(dl4_ipc, na.rm  = TRUE),
    dumm_volatilidad = vixcls > quantile(vixcls, 0.7),
    dumm_covid = fecha > "2020-01-01"
  )

modelo1 <- lm(exp_inf ~ lag(exp_inf) + d_inflacion, data = data_modelos)
modelo2 <- lm(
  exp_inf ~ lag(exp_inf) + d_inflacion + I(d_inflacion*dumm_inflacion), 
  data = data_modelos)

modelo3 <- lm(
  exp_inf ~ lag(exp_inf)+ d_inflacion + I(d_inflacion*dumm_volatilidad), 
  data = data_modelos)

stargazer::stargazer(modelo1, modelo2, modelo3, type = "text")


mod_covid <- lm(
  exp_inf ~ lag(exp_inf) + d_inflacion + I(d_inflacion*dumm_inflacion) + dumm_covid, 
  data = data_modelos)

mod_covid2 <- lm(
  exp_inf ~ lag(exp_inf)+ d_inflacion + I(d_inflacion*dumm_volatilidad) + dumm_covid, 
  data = data_modelos)

stargazer::stargazer(mod_covid, mod_covid2, type = "text")


# Anclaje de las expectativas ---------------------------------------------

eem_anclaje <- databcrd::get_expectativas(modalidad = "eem") |> 
  dplyr::filter(
    medida == "Promedio",
    variable_key == "inf",
    fecha >= "2009-06-01",
    fecha <= "2024-11-01") |> 
  dplyr::select(fecha, horizonte, expectativa) |> 
  # filter(horizonte %in% c("12 meses", "24 meses")) |> 
  pivot_wider(
    id_cols = fecha, 
    names_from = horizonte,
    values_from = expectativa
  ) |> janitor::clean_names()
  
modelo_anclaje <- lm(x24_meses~x12_meses, data = eem_anclaje |> 
                       dplyr::filter(!is.na(x24_meses)))
stargazer::stargazer(modelo_anclaje, type = "text")
