# Analisis de comparación de medias ---------------------------------------
library(tidyverse)
library(ggplot2)
library(broom)
library(car)
library(emmeans)


expect_all <- readRDS("data/eem_historico.rds")

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
  mutate(grupo = as.factor(grupo))


expect_by_group |> 
  # summarise(
  #   promedio = mean(inflacion_interanual, na.rm = TRUE),
  #   .by = c(periodo, grupo)
  # ) |> 
  ggplot(aes(periodo, inf_12)) + 
  geom_line() + 
  geom_hline(yintercept = 5, linetype = 2, color = "#0466c8") +
  geom_hline(yintercept = 3, linetype = 2, color = "#0466c8") +
  facet_wrap(~grupo) +
  theme_light() +
  theme(
    panel.grid = element_line(colour = 'white')
  )

# Análisis de varianza: 

modelo_anova <- aov(inf_12 ~ grupo, data = expect_by_group)
summary(modelo_anova)

# Prueba de turkey
tukey_result <- TukeyHSD(modelo_anova)
print(tukey_result)

# Boxplot para comparar la distribución de expectativas
ggplot(expect_by_group, aes(x = grupo, y = inf_12, fill = grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de expectativas de inflación (12 meses)",
       x = "Grupo", y = "Inflación esperada (%)") +
  theme_minimal() + 
  theme(
    legend.position = "none"
  )
