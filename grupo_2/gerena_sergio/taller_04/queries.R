library(DBI)
library(RSQLite)
library(dplyr)


url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"
genero <- dbGetQuery(con, "
                     SELECT *
                     FROM Invoice")

track <- dbGetQuery(con, "
                     SELECT *
                     FROM InvoiceLine")

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}
# Abrimos la conexión
con <- DBI::dbConnect(RSQLite::SQLite(), "chinook.db")
############ Ejercicio 1 ######################
#1. pistas totales
resultado_1_1 <- dbGetQuery(con, "
  SELECT   COUNT(*) as total
  FROM     Track
")
#2. generos distintos
resultado_1_2 <- dbGetQuery(con, "
  SELECT  COUNT(DISTINCT Name) AS generos_distintos
  FROM Genre
")

#3. numero de pistas y duracion promedio por genero
resultado_1_3 <- dbGetQuery(con, "
  SELECT  g.Name AS nombre_genero,
  COUNT(t.Name) AS total_tracks,
  ROUND(AVG(Milliseconds) / 60000, 2) AS minutos_promedio
  FROM Genre g JOIN Track t ON g.GenreId = t.GenreId
  GROUP BY g.Name
  ORDER BY total_tracks DESC
")
#4. top 5 tracks con mayor duracion
resultado_1_4 <- dbGetQuery(con, "
  SELECT  Title AS album,
  Name AS nombre_track,
  ROUND(Milliseconds / 60000, 2) AS duracion_minutos
  FROM Album a JOIN Track t ON a.AlbumId = t.AlbumId
  ORDER BY duracion_minutos DESC
  LIMIT 5
")

#5. percentage of unitPrice > 0.99
resultado_1_5 <- dbGetQuery(con, "
WITH b AS (
    SELECT COUNT(Name) AS total
    FROM Track
    WHERE UnitPrice > 0.99
)
SELECT 
    CONCAT(ROUND((b.total * 1.0 / COUNT(t.Name)) * 100, 2), '%') AS porcentaje,
    b.total AS total_superior
FROM Track t
CROSS JOIN b
")

#6. resumen milisegundos
resultado_1_6 <- dbGetQuery(con, "
  SELECT  ROUND(AVG(Milliseconds), 2) AS promedio,
  MIN(Milliseconds) AS minimo,
  MAX(Milliseconds) AS maximo,
  ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds)*AVG(Milliseconds), 2) AS varianza
  FROM Track
")

############ Ejercicio 2 ######################
#1. total ingresos
resultado_2_1 <- dbGetQuery(con, "
  SELECT 
    SUM(total_por_invoice) AS total_global,
    ROUND(AVG(total_por_invoice), 2) AS promedio_por_invoice
  FROM (
    SELECT 
      SUM(Total) AS total_por_invoice
    FROM Invoice
    GROUP BY InvoiceId
  ) t
  
")

#2. facturas por año
resultado_2_2 <- dbGetQuery(con, "
  SELECT 
    STRFTIME('%Y', InvoiceDate) AS año,
    COUNT(*) AS facturas_totales
  FROM Invoice
  GROUP BY STRFTIME('%Y', InvoiceDate)
  ORDER BY año
")

#3. top 5 paises con mas ingresos
resultado_2_3 <- dbGetQuery(con, "
  SELECT
    pais,
    SUM(total_por_invoice) AS ingreso_total,
    ROUND(AVG(total_por_invoice), 2) AS promedio_por_factura
  FROM (
    SELECT 
      COUNT(InvoiceId) AS numero_invoice,
      BillingCountry AS pais,
      SUM(Total) AS total_por_invoice
    FROM Invoice
    GROUP BY InvoiceId
  ) t
  GROUP BY pais
  ORDER BY ingreso_total DESC
  LIMIT 5
  
")

#4. desviacion total
resultado_2_4 <- dbGetQuery(con, "
  SELECT AVG(Total * Total) - AVG(Total)*AVG(Total) AS varianza
  FROM Invoice
")
standar_deviation <- round(sqrt(resultado_2_4$varianza), 2)

#5.meses con mayor y menor ingreso
resultado_2_5 <- dbGetQuery(con, "
  WITH ventas_mensuales AS (
    SELECT 
      STRFTIME('%Y-%m', InvoiceDate) AS mes,
      AVG(Total) AS promedio_mensual
    FROM Invoice
    GROUP BY mes
  ),
  ranked AS (
    SELECT *,
      RANK() OVER (ORDER BY promedio_mensual DESC) AS rk_max,
      RANK() OVER (ORDER BY promedio_mensual ASC) AS rk_min
    FROM ventas_mensuales
  )
  SELECT 
    mes,
    promedio_mensual
  FROM ranked
  WHERE rk_max = 1 OR rk_min = 1;
")

dbDisconnect(con)
