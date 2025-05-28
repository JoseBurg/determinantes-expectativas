library(dplyr)
library(rugarch)
# 1. Especificar el modelo GARCH(1,1) con media ARMA(1,1)


spec <- ugarchspec(
  variance.model = list(model = "sGARCH"),
  mean.model = list(armaOrder = c(1, 1))
)

# 2. Obtener y preparar la serie de inflación esperada (mediana a 12 meses)
mediana_expec <- databcrd::get_expectativas(modalidad = "eem") |> 
  filter(medida == "Mediana", short_names == "inf_12m") |> 
  select(fecha, expectativa)

# 3. Convertir en serie de tiempo mensual
ts_expec <- ts(mediana_expec$expectativa, start = c(2009, 6), frequency = 12)

# 4. Ajustar el modelo GARCH
fit <- ugarchfit(spec = spec, data = ts_expec)

# 5. Mostrar resultados
show(fit)

# 6. Graficar la volatilidad condicional
plot(sigma(fit), type = "l", col = "blue", 
     ylab = "Volatilidad Condicional (σₜ)", 
     xlab = "Tiempo", 
     main = "Volatilidad GARCH sobre expectativas de inflación")
