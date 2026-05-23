##Taller 7##  
# Usando el dataset house_prices.csv:
# Identifica las 5 columnas con mayor porcentaje de NA. ¿Cuáles eliminarías y cuáles imputarías? Justifica.
install.packages("tidyverse") 
library(tidyverse)
df_house  <- read_csv("Datos/house_prices.csv")
N1 = nrow(df_house)
df_house |> 
  map_df(~ sum(is.na(.x)/N1)*100) |>
  pivot_longer(everything())|>
  arrange(desc(value)) |>
  transmute(Columna = name,
            pct = value) |>
  head(5)
# Imputa las columnas numéricas con NA usando la mediana y las categóricas con la moda. ¿Cuántas filas quedan completas (sin ningún NA) después?
moda <- function(x) {
  return(as.character(names(which.max(table(x)))))
}
df_house_limpia <- df_house |> 
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), median(.x, na.rm=TRUE), .x))) |>
  mutate(across(where(is.character), ~ if_else(is.na(.x), moda(.x), .x))) |>
  glimpse()
# Crea una variable antiguedad = 2024 - YearBuilt. Discretízala en 4 grupos con cut usando los cuartiles como puntos de corte.##
 
df_house_limpia <- df_house |> 
  mutate( antiguedad =  2024 - YearBuilt,
    cuartil_antiguedad = ntile(antiguedad, 4) |>
      factor(labels = c("Q1 (bajo)", "Q2", "Q3", "Q4 (alto)"))
  )
df_house_limpia|>
  select(antiguedad,cuartil_antiguedad)
## Ejercicio 2 ##
titanic  <- read_csv("Datos/titanic.csv") 
N2 = nrow(titanic)
# a) Aplica el criterio IQR para detectar outliers en fare. ¿Qué porcentaje de pasajeros son outliers?
detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}
 titanic <- titanic |>
  mutate(
    outlier_fare = detectar_outliers_iqr(fare))
 
 titanic |> select(cat("Porcentaje outliers en fare:", round((sum(titanic$outlier_fare, na.rm = TRUE)/N2)*100,2), "% \n"))
 
# b) Compara tres versiones de fare: original, winsorizada al percentil 5–95, y transformación log(1 + fare). Grafica sus distribuciones.
 titanic <- titanic |>
   mutate(
     # Winsorización: recortar en percentiles 5 y 95
     fare_winsor = pmin(pmax(fare,
                             quantile(fare, 0.05, na.rm = TRUE)),
                        quantile(fare, 0.95, na.rm = TRUE)),
     
     # Transformación logarítmica: comprime la escala de valores extremos
     fare_log    = log1p(fare)
   )
 titanic |>
   select(fare, fare_winsor, fare_log) |>
   pivot_longer(everything(), names_to = "version", values_to = "valor") |>
   ggplot(aes(x = valor, fill = version)) +
   geom_density(alpha = 0.6) +
   facet_wrap(~ version, scales = "free") +
   scale_fill_brewer(palette = "Set2", guide = "none") +
   labs(title = "Distribución de fare según tratamiento de outliers",
        x = "Valor", y = "Densidad") +
   theme_minimal(base_size = 12)
# c) Crea un nuevo dataset donde fare esté winsorizada y age esté imputada con la mediana por grupo de pclass. Verifica que no queden NA en esas columnas.
titanic2 <- titanic |> mutate( fare_winsor = replace_na(fare_winsor,median(fare_winsor,na.rm = TRUE)),
                               fare = fare_winsor,
                               age = replace_na(age,median(age,na.rm = TRUE)))
        
sum(is.na(titanic2$age)) == 0
sum(is.na(titanic2$fare_winsor)) == 0
titanic2 |> filter(is.na(fare))

# EJERCICIO 3.
# Aplica Min-Max y Z-score a las variables ram, battery_power y px_height. Verifica propiedades: ¿el rango de Min-Max es [0,1]? ¿La media de Z-score es 0?
  mobile  <- read_csv("Datos/mobile_data.csv") 
  N3 = nrow(mobile)
  min_max <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  estandarizar <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  mobile_norm <- mobile |>
    mutate(
      across(c(ram, battery_power, px_height),
             list(mm = min_max, zscore = estandarizar), 
             .names = "{.col}_{.fn}")
    )
  
  mobile_norm |>
    select(ram, ram_mm, battery_power, battery_power_mm) |>
    slice(1:5) |>
    mostrar_tabla(caption = "Min-Max scaling — primeras 5 filas")
  ### Tabla de medias de variables normalizadas 
  mobile_norm |>
    select(ram_mm:px_height_zscore) |>
    map_df(~round(mean(.x),2)) 
#   Grafica un scatter plot de ram vs. battery_power para las tres versiones (original, Min-Max, Z-score). ¿Cambia la forma de la nube de puntos?
  par(mfrow = c(1,3), oma=c(0, 0, 3, 0)) # 1 row, 2 columns
  mtext("Ram vs Poder de Batería", side=3, line=1, outer=TRUE, cex=1.5, font=2)
  plot(mobile_norm$ram, mobile_norm$battery_power,main = "Sin normalizar")
  plot(mobile_norm$ram_mm, mobile_norm$battery_power_mm,main = "Min-Max")
  plot(mobile_norm$ram_zscore, mobile_norm$battery_power_zscore,main = "Z-score")
  
  #No cambia la forma de la nube debido a que la normalización no afecta la interaccion entre variables
  
#   ¿En qué situación el Min-Max da resultados problemáticos? Genera un ejemplo sintético con un outlier extremo y muestra el efecto.
  # Fijar semilla para reproducibilidad
  set.seed(123)
  
  # Datos normales: 100 observaciones
  datos_normales <- rnorm(100, mean = 50, sd = 10)
  
  # Añadir un outlier extremo
  outlier <- 500
  datos <- c(datos_normales, outlier)
  
  # Aplicar normalización Min-Max
  min_max <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  datos_minmax <- min_max(datos)
  
  # Mostrar resumen de los datos originales y normalizados
  cat("Resumen de datos originales:\n")
  print(summary(datos))
  
  cat("\nResumen de datos normalizados con Min-Max:\n")
  print(summary(datos_minmax))
  
  # Visualización
  par(mfrow = c(1, 2))
  hist(datos, breaks = 20, col = "skyblue", main = "Datos originales",
       xlab = "Valor", ylab = "Frecuencia")
  abline(v = c(min(datos), max(datos)), col = "red", lty = 2, lwd = 2)
  legend("topright", legend = c("Mínimo", "Máximo"), col = "red", lty = 2)
  
  hist(datos_minmax, breaks = 20, col = "lightgreen", 
       main = "Datos normalizados (Min-Max)",
       xlab = "Valor normalizado", ylab = "Frecuencia")
  par(mfrow = c(1, 1))
  
  # Observación: casi todos los valores se aprietan cerca de 0
  cat("\nProporción de datos normales (originales <= 70) comprimidos en el rango [0,", 
      max(datos_minmax[datos <= 70]), "]:\n")
  rango_comprimido <- max(datos_minmax[datos <= 70])
  cat(round(rango_comprimido, 4), "\n")                
  