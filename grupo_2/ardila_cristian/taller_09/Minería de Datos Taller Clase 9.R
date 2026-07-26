# ============================================================
# Minería de Datos - Taller Clase 9
# ============================================================


# ------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------

paquetes <- c(
  "tidyverse", "knitr", "FSA"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

library(tidyverse)
library(knitr)
library(FSA)


# ------------------------------------------------------------
# Función auxiliar para mostrar tablas si se desea
# ------------------------------------------------------------

mostrar_tabla <- function(x, n = Inf, caption = NULL, digits = 4) {
  if (is.infinite(n)) {
    knitr::kable(x, caption = caption, digits = digits)
  } else {
    knitr::kable(head(x, n), caption = caption, digits = digits)
  }
}


# ============================================================
# Dataset del enunciado
# ============================================================

set.seed(2025)

n <- 400

estudiantes <- tibble(
  id         = 1:n,
  programa   = sample(
    c("Estadística", "Sistemas", "Economía", "Matemáticas"),
    n,
    replace = TRUE,
    prob = c(.3, .3, .25, .15)
  ),
  semestre   = sample(1:10, n, replace = TRUE),
  sexo       = sample(c("Mujer", "Hombre"), n, replace = TRUE),
  nota_final = round(rnorm(n, mean = 3.5, sd = 0.7), 1) |>
    pmin(5) |>
    pmax(0),
  horas_sem  = round(rnorm(n, mean = 18, sd = 5)),
  beca       = sample(c("Sí", "No"), n, replace = TRUE, prob = c(.3, .7))
)

head(estudiantes)


# ============================================================
# Ejercicio 1. ¿Es normal la distribución de notas?
# ============================================================

normalidad_programa <- estudiantes |>
  group_by(programa) |>
  summarise(
    W = as.numeric(shapiro.test(nota_final)$statistic),
    p_valor = shapiro.test(nota_final)$p.value,
    .groups = "drop"
  ) |>
  mutate(
    rechaza_normalidad = if_else(
      p_valor < 0.05,
      "Sí",
      "No"
    )
  ) |>
  arrange(p_valor)

programas_rechazo <- normalidad_programa |>
  filter(p_valor < 0.05)

programa_menor_p <- normalidad_programa |>
  slice_min(p_valor, n = 1, with_ties = FALSE) |>
  pull(programa)

datos_qq <- estudiantes |>
  filter(programa == programa_menor_p)

grafico_qq <- ggplot(datos_qq, aes(sample = nota_final)) +
  stat_qq(alpha = 0.75, size = 1.8) +
  stat_qq_line(linewidth = 0.8) +
  labs(
    title = paste("QQ-plot de nota_final -", programa_menor_p),
    subtitle = "Programa con menor p-valor en Shapiro-Wilk",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9),
    plot.margin = margin(8, 8, 8, 8)
  )

normalidad_programa
programas_rechazo
programa_menor_p
grafico_qq

#' 1. ¿En qué programas se rechaza normalidad con alpha = 0.05?
#' 
#' No se rechaza normalidad en ningúna prueba, porque todos los p-valores son 
#' mayores que 0.05. Por tanto, no hay evidencia estadística suficiente para 
#' rechazar que las notas siguen una distribución normal dentro de cada programa.
#'
#' 2. Construir el QQ-plot para el programa con el p-valor más bajo.
#'
#' El programa con menor p-valor es Matemáticas. Por eso el QQ-plot se construye
#' usando las notas finales de ese programa.
#'
#' 3. ¿Qué patrón se observa en las colas?
#'
#' En el QQ-plot de Matemáticas no se observa una desviación suficientemente
#' fuerte como para rechazar normalidad al 5%. hay pequeñas separaciones
#' visuales en uno de los extremos.
#'
#' 4. Si la distribución no es normal, ¿qué implicaciones tiene para pruebas
#' posteriores?
#'
#' Si son pruebas paramétricas no tendrían validez ya que no se cumplen los
#' supuestos distribucionales que necesitan para dar veracidad a los cálculos de
#' los estadísticos paramétricos.


# ============================================================
# Ejercicio 2. Estadísticos robustos y sesgo
# ============================================================

estadisticos_robustos <- estudiantes |>
  group_by(programa) |>
  summarise(
    media = mean(nota_final),
    mediana = median(nota_final),
    media_recortada = mean(nota_final, trim = 0.1),
    mad = mad(nota_final),
    cv = sd(nota_final) / mean(nota_final),
    asimetria = (mean(nota_final) - median(nota_final)) / sd(nota_final),
    .groups = "drop"
  ) |>
  mutate(
    discrepancia_media = abs(media - media_recortada),
    direccion_sesgo = case_when(
      asimetria > 0 ~ "Sesgo a la derecha",
      asimetria < 0 ~ "Sesgo a la izquierda",
      TRUE ~ "Sin sesgo apreciable"
    )
  ) |>
  arrange(desc(discrepancia_media))

programa_mayor_discrepancia <- estadisticos_robustos |>
  slice_max(discrepancia_media, n = 1, with_ties = FALSE) |>
  select(programa, media, media_recortada, discrepancia_media)

sesgo_por_programa <- estadisticos_robustos |>
  select(programa, asimetria, direccion_sesgo)

estadisticos_robustos
programa_mayor_discrepancia
sesgo_por_programa

#' 1. ¿Qué programa muestra mayor discrepancia entre media y media recortada?
#'
#' Matemáticas
#'
#' 2. ¿Qué indica esa discrepancia?
#'
#' Que las colas de las distribución de los datos están afectando la media
#'
#' 3. Interpretar el coeficiente de asimetría de Pearson.
#'
#' 1 Matemáticas   0.0559  Sesgo a la derecha  
#' 2 Sistemas      0.0695  Sesgo a la derecha  
#' 3 Economía     -0.00420 Sesgo a la izquierda
#' 4 Estadística   0.00509 Sesgo a la derecha
#'
#' 4. ¿En qué dirección sesga cada distribución?
#'
#' Revisar respuesta en el punto anterior.
#' 
#' 5. ¿Cuándo se preferiría reportar MAD en lugar de desviación estándar?
#'
#' Se preferiría reportar MAD cuando hay valores atípicos, asimetría o falta de
#' normalidad. La MAD es más robusta porque se basa en la mediana y se altera
#' menos por observaciones extremas.


# ============================================================
# Ejercicio 3. Valores atípicos
# ============================================================

detectar_outliers <- function(x) {
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- q3 - q1
  z   <- (x - mean(x)) / sd(x)
  
  tibble(
    valor = x,
    outlier_iqr = x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr),
    outlier_z = abs(z) > 3
  )
}

outliers_estudiantes <- estudiantes |>
  group_by(programa) |>
  mutate(
    q1 = quantile(nota_final, 0.25),
    q3 = quantile(nota_final, 0.75),
    iqr = q3 - q1,
    z = (nota_final - mean(nota_final)) / sd(nota_final),
    outlier_iqr = nota_final < (q1 - 1.5 * iqr) |
      nota_final > (q3 + 1.5 * iqr),
    outlier_z = abs(z) > 3
  ) |>
  ungroup()

resumen_outliers <- outliers_estudiantes |>
  group_by(programa) |>
  summarise(
    n_iqr = sum(outlier_iqr),
    n_z = sum(outlier_z),
    coinciden = sum(outlier_iqr & outlier_z),
    .groups = "drop"
  )

grafico_outliers <- ggplot(
  outliers_estudiantes,
  aes(x = programa, y = nota_final)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    alpha = 0.30,
    size = 1.4,
    position = position_jitter(width = 0.12, height = 0)
  ) +
  geom_point(
    data = outliers_estudiantes |> filter(outlier_iqr),
    aes(x = programa, y = nota_final),
    color = "red",
    size = 2.2,
    position = position_jitter(width = 0.12, height = 0)
  ) +
  labs(
    title = "Outliers por IQR en nota_final",
    x = "Programa",
    y = "Nota final"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold", size = 12),
    plot.margin = margin(8, 8, 8, 8)
  )

coincidencia_outliers <- resumen_outliers |>
  mutate(
    hay_outliers = if_else(n_iqr > 0 | n_z > 0, "Sí", "No"),
    coinciden_totalmente = case_when(
      n_iqr == 0 & n_z == 0 ~ "No aplica: ningún criterio detectó outliers",
      n_iqr == n_z & n_iqr == coinciden ~ "Sí",
      TRUE ~ "No"
    )
  )

resumen_outliers
coincidencia_outliers
grafico_outliers

#' 1. ¿Coinciden los dos criterios?
#'
#' No se hayaron coincidencias en ninguno de los dos criterios
#'
#' 2. ¿Cuándo podría fallar el criterio Z con muestras pequeñas?
#'
#' Cuando la media y de desviación estándar son volátiles, además de la presencia
#' de datos atípicos.
#' 
#' 3 y 4. Visualizar los outliers detectados por IQR sobre un boxplot.
#'
#' V er gráfica en consola
#'
#' 5. ¿Se eliminarían esos puntos del análisis?
#'
#' No se hayaron puntos atípicos, sin embargo, si hubieran puntos atípicos se
#' buscaría un criterio para determinar si esos datos aportan información
#' importante para el fenómeno que se se quiere analizar para decidir su descarte
#' o no, si la información sea importante se buscaría una vía de modelamiento 
#' que permita analizar la información atípica sin penalizar las observaciones
#' usuales.


# ============================================================
# Ejercicio 4. Correlación y significancia
# ============================================================

correlaciones <- estudiantes |>
  group_by(programa) |>
  summarise(
    r_pearson  = cor(horas_sem, nota_final, method = "pearson"),
    r_spearman = cor(horas_sem, nota_final, method = "spearman"),
    p_pearson  = cor.test(horas_sem, nota_final, method = "pearson")$p.value,
    p_spearman = suppressWarnings(
      cor.test(horas_sem, nota_final, method = "spearman", exact = FALSE)$p.value
    ),
    .groups = "drop"
  ) |>
  mutate(
    pearson_significativo = if_else(p_pearson < 0.05, "Sí", "No"),
    spearman_significativo = if_else(p_spearman < 0.05, "Sí", "No"),
    spearman_mayor = if_else(abs(r_spearman) > abs(r_pearson), "Sí", "No"),
    coinciden_signo = if_else(
      sign(r_pearson) == sign(r_spearman),
      "Sí",
      "No"
    ),
    coinciden_significancia = if_else(
      pearson_significativo == spearman_significativo,
      "Sí",
      "No"
    )
  )

programas_correlacion_significativa <- correlaciones |>
  filter(p_pearson < 0.05 | p_spearman < 0.05)

comparacion_correlaciones <- correlaciones |>
  select(
    programa,
    r_pearson,
    r_spearman,
    pearson_significativo,
    spearman_significativo,
    coinciden_signo,
    coinciden_significancia,
    spearman_mayor
  )

grafico_correlacion <- ggplot(
  estudiantes,
  aes(x = horas_sem, y = nota_final)
) +
  geom_point(alpha = 0.45, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ programa) +
  labs(
    title = "Horas de estudio y nota final por programa",
    x = "Horas de estudio por semana",
    y = "Nota final"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(8, 8, 8, 8)
  )

correlaciones
programas_correlacion_significativa
comparacion_correlaciones
grafico_correlacion

#' 1. ¿En qué programas hay correlación significativa?
#'
#' Estadística y matemáticas
#'
#' 2. ¿Pearson y Spearman coinciden?
#'
#' Sí coinciden en significancia
#'
#' 3. Si Spearman es mayor que Pearson, ¿qué sugiere?
#'
#' Si Spearman supera a Pearson en valor absoluto, puede sugerir una presencia
#' de datos extremos, que afectan el valor del coeficiente de Spearman ya que es
#' poco sensible a datos lejanos.
#'
#' 4 y 5. Graficar dispersograma con geom_smooth y facet_wrap.
#'
#' Ver gráfica en consola.


# ============================================================
# Ejercicio 5. Comparación de grupos con tamaño del efecto
# ============================================================

prueba_kw <- kruskal.test(nota_final ~ programa, data = estudiantes)

H <- as.numeric(prueba_kw$statistic)
p_kw <- prueba_kw$p.value
k <- n_distinct(estudiantes$programa)
n_total <- nrow(estudiantes)

eta2 <- max(0, (H - k + 1) / (n_total - k))

resultado_kw <- tibble(
  estadistico_H = H,
  p_valor = p_kw,
  eta2 = eta2,
  decision = if_else(
    p_kw < 0.05,
    "Se rechaza H0",
    "No se rechaza H0"
  ),
  interpretacion_p_valor = if_else(
    p_kw < 0.05,
    "Hay evidencia de diferencias en nota_final entre programas.",
    "No hay evidencia suficiente de diferencias en nota_final entre programas."
  ),
  tamano_efecto = case_when(
    eta2 < 0.06 ~ "Pequeño",
    eta2 >= 0.06 & eta2 < 0.14 ~ "Mediano",
    eta2 >= 0.14 ~ "Grande"
  )
)

if (p_kw < 0.05) {
  
  dunn <- FSA::dunnTest(
    nota_final ~ programa,
    data = estudiantes,
    method = "bonferroni"
  )
  
  resultado_dunn <- dunn$res |>
    as_tibble() |>
    mutate(
      diferencia_significativa = if_else(P.adj < 0.05, "Sí", "No")
    )
  
  pares_difieren <- resultado_dunn |>
    filter(P.adj < 0.05)
  
} else {
  
  dunn <- NULL
  
  resultado_dunn <- tibble(
    mensaje = "No se aplica Dunn porque Kruskal-Wallis no fue significativo con alpha = 0.05."
  )
  
  pares_difieren <- tibble(
    mensaje = "No se identifican pares porque no se aplicó prueba post-hoc."
  )
}

prueba_kw
resultado_kw
resultado_dunn
pares_difieren

#' 1. ¿Se rechaza la hipótesis nula?
#'
#' No se rechaza.
#'
#' 2. Interpretar el p-valor en contexto.
#'
#' No hay evidencia suficiente para rechazar la hipótesis de que no hay 
#' diferencia significativa entre los programas.
#'
#' 3 y 4. Interpretar eta-cuadrado y clasificar el tamaño del efecto.
#'
#' El valor calculado de eta-cuadrado es -0.00481. Este resultado se toma como
#' aproximadamente cero. Por tanto, el efecto del programa sobre nota_final es 
#' despreciable.
#'
#' Este resultado se clasifica como un tamaño de efecto pequeño, pues está por
#' debajo de 0.06. Su interpretación sería similar a la prueba KW
#' 
#' 5 y 6. Si hay diferencias, aplicar Dunn con Bonferroni e identificar pares.
#'
#' No fue significativo, por lo tanto no se realizó test de Dunn.
