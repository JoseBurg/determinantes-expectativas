# Funciones de impulsos respuestas de las expectativas --------------------
library(tidyverse)
library(vars)
library(lmtest)
library(gt)
library(sandwich)
library(tseries)
library(forecast)
library(readxl)  # Para leer Excel

# Cargar datos y unir -----------------------------------------------------
data_ms <- read_excel("./data/data.xlsx", sheet = "data_expect")

data_infl <- data_ms |>
  mutate(dl4_ipc = 100 * (IPC / dplyr::lag(IPC,12) - 1),
         dev_pos = pmax(dl4_ipc - meta, 0),
         dev_neg = pmax(meta - dl4_ipc, 0),
         mas_meta = factor(dl4_ipc > meta)) |>
  filter(!is.na(dl4_ipc), fecha >= "2009-05-01")

expect_by_group <-read_excel("./data/expectativas_inflacion_grupo.xlsx") |> 
  dplyr::select(periodo, grupo, inf_12) |> 
  dplyr::mutate(
    periodo = as.Date(periodo),
    grupo = ifelse(
              stringr::str_detect(grupo,"Puestos de bolsa|Economistas|Académicos"),
                                 "Otros", grupo),
    grupo = stringr::str_to_lower(grupo)) |> 
  summarise(inf_12 = mean(inf_12, na.rm = TRUE),
            .by = c(periodo, grupo)) |> 
  pivot_wider(
    id_cols = periodo, 
    names_from = grupo, 
    values_from = inf_12
  ) |> janitor::clean_names()

datos_exp_infl <- data_infl |> 
  left_join(expect_by_group, by = c("fecha" = "periodo"))

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


# Modelo de asimetría  ------------------------------------------
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
  modelo_general <- modelo_asimetria(data = datos_exp_infl|> filter(dl4_ipc > mean(dl4_ipc, na.rm = TRUE)), variable = i)
  
  estimacion_infl_alt[[i]] <- c(modelo_general$coefficients[-1], rsuared = summary(modelo_general)$r.squared)
}

estimacion_infl_baja <- list()

for (i in tipo_inflacion) {
  modelo_general <- modelo_asimetria(data = datos_exp_infl|> filter(dl4_ipc > mean(dl4_ipc, na.rm = TRUE)), variable = i)
  
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



# 
# Instalar gt si no está instalado
# Crear el DataFrame con los valores más importantes de la regresión
df <- data.frame(
  Variable = c("Exp. General", "Bancos", "Organismos Multilaterales", "Consultores", "Otros"),
  dev_pos = c(0.402, 0.387, 0.262, 0.382, 0.494),
  dev_neg = c(-0.059, -0.099, -0.052, -0.149, -0.062),
  Constant = c(4.469, 4.263, 4.344, 4.573, 4.928),
  R2 = c(0.268, 0.280, 0.139, 0.308, 0.163),
  Adjusted_R2 = c(0.260, 0.272, 0.130, 0.299, 0.154),
  Residual_Std_Error = c(1.187, 1.168, 1.176, 1.173, 1.981),
  F_Statistic = c(33.520, 35.414, 14.623, 37.088, 17.762)
)

df %>%
  gt() %>%
  tab_header(
    title = "Modelo de asimetría",
    subtitle = md("$expectativas = \\beta_0 + \\beta_1 dev\\_pos + \\beta_2 dev\\_neg + \\varepsilon$")) %>%
  fmt_number(
    columns = c(dev_pos, dev_neg, Constant, R2, Adjusted_R2, Residual_Std_Error, F_Statistic),
    decimals = 2
  ) %>%
  cols_label(
    Variable = "Tipo",
    dev_pos = "dev_pos",
    dev_neg = "dev_neg",
    Constant = "Constante",
    R2 = "R²",
    Adjusted_R2 = "R² Aj",
    Residual_Std_Error = "Error Std",
    F_Statistic = "F Statistic"
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold"
  )
