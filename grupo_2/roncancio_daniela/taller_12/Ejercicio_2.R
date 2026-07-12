
# Ejercicio 2: Regresión múltiple

# Ajuste un modelo de regresión lineal múltiple con publicidad y precio como predictores. 
# Compare el R² ajustado con el del ejercicio anterior. ¿Cuál variable aporta más a explicar las ventas?

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

# Modelo múltiple

modelo2 <- lm(ventas ~ publicidad + precio, data = ventas)
summary(modelo2)

# El resultado es que precio simplemente mueve menos las ventas en términos absolutos dentro de este dataset, 
# aunque sea estadísticamente muy significativo (p = 2.02e-06).

# ¿Cuál variable aporta más?

# En magnitud del efecto real, un aumento de 1 unidad en publicidad genera +18.6 unidades de ventas; una 
# reducción de 1 unidad en precio genera solo +4.9. Y el rango de publicidad es igual de amplio que el del precio.

# La conclusión es clara: precio es un predictor significativo y útil para incluir en el modelo, pero publicidad 
# es el motor principal de las ventas en estos datos.

## Para el modelo múltiple del ejercicio 2, genere los gráficos de diagnóstico (residuales vs. ajustados, Q-Q de 
# residuales, escala-localización). Identifique si se cumplen los supuestos de linealidad, homocedasticidad y 
# normalidad. Redacte una conclusión de dos párrafos como si fuera para un informe de negocio.

par(mfrow = c(1, 3))
plot(modelo2, which = 1)   # Residuales vs. ajustados
plot(modelo2, which = 2)   # Q-Q normal
plot(modelo2, which = 3)   # Escala-localización
par(mfrow = c(1, 1))

# 1. Linealidad: La nube es horizontal y centrada en cero sin ninguna curvatura sistemática, lo que confirma linealidad.
# 2. Normalidad: Dispersión uniforme, los puntos siguen la diagonal con una adherencia excelente desde el cuantil −2 
# hasta +2, que cubre más del 95% de las observaciones, lo que confirma normalidad.
# 3. Homocedasticidad: La raíz de los residuos estandarizados absolutos se dispersa de forma relativamente uniforme 
# alrededor de una línea suavizada aproximadamente plana.Supuesto cumplido.





