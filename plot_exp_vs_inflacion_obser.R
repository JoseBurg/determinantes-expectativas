source("work_data.R")

library(databcrd)
library(dplyr)
library(ggplot2)

data_expectativas <- function(estadistico = "Promedio"){
  checkmate::assert_choice(estadistico, 
                           choices = c("Mediana","Promedio","Desviación estándar"))
  
  eem <- databcrd::get_expectativas(modalidad = "eem") |> 
    dplyr::filter(
      medida == estadistico,
      variable_key == "inf")
  
  inflacion <- databcrd::get_ipc_long(desagregacion = "general") |> 
    dplyr::mutate(
      v_m = 100 * ((indice / lag(indice)) - 1),
      v_i = 100 * ((indice / lag(indice, 12)) - 1)) |> 
    dplyr::select(date, ipc = indice, v_i)
  
  data_plot <- eem |> 
    dplyr::left_join(inflacion, by = c("fecha" = "date"))
  
  data_plot
}



mediana <- data_expectativas("Mediana") 
promedio <- data_expectativas("Promedio") 

promedio |>
  left_join(data_ms |> dplyr::select(fecha, meta)) |> 
  ggplot(aes(fecha, expectativa)) + 
  geom_line(aes(color = horizonte)) + 
  geom_line(aes(y = v_i, color = "Inflación observada"), size = 0.7) +
  geom_line(aes(y = meta), color = "darkred", linetype = 8) +
  geom_line(aes(y = meta + 1), color = "darkred") +
  geom_line(aes(y = meta - 1), color = "darkred") +
  # geom_hline(yintercept = 4, color = "gray") +
  # geom_hline(yintercept = c(5, 3), color = "gray", linetype = 2) +
  labs(color = NULL, x = NULL, y = NULL) +
  theme_light() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
