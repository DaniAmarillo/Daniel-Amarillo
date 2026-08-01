
# Ejercicio 4: Regresión logística

# Use la variable promo (0 = sin promoción, 1 = con promoción) como variable dependiente. Ajuste un modelo logístico con publicidad y precio como predictores. 
# Reporte los odds ratios e interprete el efecto del precio sobre la probabilidad de aplicar una promoción.


set.seed(42)
n <- 120

ventas <- data.frame(
  mes        = rep(1:12, times = 10),
  publicidad = round(runif(n, min = 2.0, max = 4.5),1),
  precio     = round(runif(n, min = 13.0, max = 15.5),1),
  region     = sample(c("Norte", "Sur", "Este", "Oeste"), n, replace = TRUE),
  promo      = sample(c(0L, 1L), n, replace = TRUE)
)

ventas$ventas <- round(
  100 + 18 * ventas$publicidad - 5 * ventas$precio +
    rnorm(n, mean = 0, sd = 8)
)


modelo_log <- glm(promo ~ publicidad + precio, 
                  data = ventas,
                  family = binomial(link = "logit"))

summary(modelo_log)
  
# Odds ratios

exp(coef(modelo_log))

# Cuando el precio sube, es menos probable que se aplique una promoción. Específicamente, por cada peso (o unidad) que sube el precio, la probabilidad de que 
# haya promoción baja aproximadamente un 32%.

  
  