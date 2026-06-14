
# Ejercicio 3: Variable categórica

# Incluya la variable region como predictor en el modelo múltiple. Codifíquela correctamente como variable dummy. 
# ¿Existe diferencia significativa en ventas entre regiones? Interprete los coeficientes.

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

# Se convierte la variable " region" en una variable categórica (factor)

ventas$region <- factor(ventas$region)

modelo3 <- lm(ventas ~ publicidad + precio + region, data = ventas)
summary(modelo3)

## ¿Existe diferencia significativa en ventas entre regiones? Interprete los coeficientes.

# Ninguno de los tres coeficientes dummy supera el umbral α = 0.05, luego no hay diferencias significativas
# entre regiones.

# regionNorte = −1.49 ->  Norte vende en promedio 1.49 unidades menos que Este, manteniendo constantes publicidad y precio.
# regionOeste = −4.01 ->  Oeste vende 4.01 unidades menos que Este, manteniendo constantes publicidad y precio.
# regionSur = −1.82 ->  Sur vende 1.82 unidades menos que Este,manteniendo constantes publicidad y precio.

