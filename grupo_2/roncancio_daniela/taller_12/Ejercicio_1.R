
# Ejercicio 1: Regresión simple

# Ajuste un modelo de regresión lineal simple que explique las ventas a partir del gasto en publicidad. 
# Interprete el coeficiente de publicidad y el R². ¿El gasto en publicidad explica bien las ventas?

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


# Modelo simple

modelo <- lm(ventas ~ publicidad, data = ventas)
summary(modelo)

##  1. Coeficiente de publicidad = Por cada unidad adicional gastada en publicidad, las ventas
# aumentan en promedio aproximadamente 19 unidades.

## 2. Intercepto = Cuando el gasto de publicidad es 0, las ventas estimadas son de 26.47 unidades.

## 3. R² = El modelo explica el 73.7% de la variabilidad total en ventas usando únicamente
# publicidad. 

## 4. ¿Explica bien las ventas? Para un modelo de una sola variable, R² = 0.74 es bueno y la 
# variable es claramente relevante, per podría mejorar si se incluye la variable precio.





