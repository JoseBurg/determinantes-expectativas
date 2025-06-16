# Paquetes y manipulacion de datos --------------------
library(tidyverse)
library(vars)
library(lmtest)
library(gt)
library(sandwich)
library(tseries)
library(forecast)
library(readxl)
library(quantmod)

# Cargar datos y unir -----------------------------------------------------

# Descargar datos diarios del VIX
getSymbols("VIXCLS", src = "FRED")

# Convertir datos diarios a frecuencia mensual (por ejemplo usando el último dato del mes)
VIXCLS_monthly <- to.monthly(VIXCLS, indexAt = "lastof", OHLC = FALSE)



vix_df <- data.frame(
  fecha = index(VIXCLS_monthly), 
  vix = coredata(VIXCLS_monthly)) |> 
  janitor::clean_names() |> 
  filter(fecha >= "2009-05-01") |> 
  mutate(fecha = make_date(
                  year = year(fecha),
                  month = month(fecha),
                  "01"))



data_ms <- read_excel("./data/data.xlsx", sheet = "data_expect") |>
  mutate(fecha = as.Date(fecha))

data_infl <- data_ms |>
  mutate(dl4_ipc = 100 * (IPC / dplyr::lag(IPC,12) - 1),
         dev_pos = pmax(dl4_ipc - meta, 0),
         dev_neg = pmax(meta - dl4_ipc, 0),
         mas_meta = factor(dl4_ipc > meta)) |>
  filter(!is.na(dl4_ipc), fecha >= "2009-05-01") 

expect_all <- readRDS("data/eem_historico.rds")

# expect_by_group <- read_excel("./data/expectativas_inflacion_grupo.xlsx") |> 
#   dplyr::select(periodo, grupo, inf_12 = inflacion_12) |> 
#   

expect_by_group <- expect_all |> 
  filter(periodo < "2024-12-01") |> 
  dplyr::select(periodo, grupo, inf_12 = inflacion_interanual) |> 
  dplyr::mutate(
    periodo = as.Date(periodo),
    grupo = ifelse(
      stringr::str_detect(grupo,"Puestos de bolsa|Economistas|Académicos|Empresas|Especial"),
      "Otros", grupo),
    grupo = stringr::str_to_lower(grupo)) |> 
  summarise(inf_12 = mean(inf_12, na.rm = TRUE),
            .by = c(periodo, grupo)) |> 
  pivot_wider(
    id_cols = periodo, 
    names_from = grupo, 
    values_from = inf_12
  ) |> janitor::clean_names() |> 
  filter()

full_data <- data_infl |> 
  left_join(expect_by_group, by = c("fecha" = "periodo")) |> 
  left_join(vix_df)


expec_group <- full_data
