library(DBI)
library(RSQLite)
library(dplyr)


url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}
# Abrimos la conexión
con <- DBI::dbConnect(RSQLite::SQLite(), "chinook.db")
############ Ejercicio 1 ######################
#1. informacion compras
resultado_1_1 <- dbGetQuery(con, "
  SELECT CONCAT(FirstName, ' ', LastName) AS nombre_cliente,
  Country AS pais,
  t.Name AS nombre_pista,
  g.Name AS genero,
  il.UnitPrice AS precio_unitario
  FROM Customer cu
  INNER JOIN Invoice inv ON cu.CustomerId = inv.CustomerId
  INNER JOIN InvoiceLine il ON il.InvoiceId = inv.InvoiceId
  INNER JOIN Track t ON t.TrackId = il.TrackId
  INNER JOIN Genre g ON g.GenreId = t.GenreId
  LIMIT 15
")
#2. tracks sin venta
resultado_1_2 <- dbGetQuery(con, "
  SELECT t.Name as pista
  FROM Track t LEFT JOIN InvoiceLine il ON t.TrackId = il.TrackId
  WHERE InvoiceId IS NULL
")

#3. total de ingresos por artista
resultado_1_3 <- dbGetQuery(con, "
WITH base AS (SELECT a.Name AS artista,
               il.UnitPrice AS precio_unitario,
               il.Quantity AS cantidad
        FROM Artist a
        LEFT JOIN Album al ON al.ArtistId = a.ArtistId
        LEFT JOIN Track t ON t.AlbumId = al.AlbumId
        LEFT JOIN InvoiceLine il ON il.TrackId = t.TrackId
)

  SELECT artista,  
  COALESCE(SUM(precio_unitario * cantidad), 0) AS total_artista
  FROM base
  GROUP BY artista
  ORDER BY total_artista DESC
  LIMIT 10
")
#4. participacion porcentual
resultado_1_4 <- dbGetQuery(con, "
WITH base AS (SELECT a.Name AS artista,
               il.UnitPrice AS precio_unitario,
               il.Quantity AS cantidad
        FROM Artist a
        LEFT JOIN Album al ON al.ArtistId = a.ArtistId
        LEFT JOIN Track t ON t.AlbumId = al.AlbumId
        LEFT JOIN InvoiceLine il ON il.TrackId = t.TrackId
),
conglomerate AS (SELECT artista,  
  COALESCE(SUM(precio_unitario * cantidad), 0) AS total_artista
  FROM base
  GROUP BY artista
)

SELECT artista,
  ROUND((total_artista / SUM(total_artista) OVER()) * 100, 2) AS porcentaje_participacion,
  RANK() OVER(ORDER BY total_artista DESC) AS rankin_ingresos
FROM conglomerate
")
############ Ejercicio 2 ######################
#1. ingresos mensuales
resultado_2_1 <- dbGetQuery(con, "
  SELECT 
    SUBSTR(InvoiceDate, 1, 7) AS mes,
    SUM(Total) AS total_ventas
  FROM Invoice
  GROUP BY mes
  ORDER BY mes
  
")

#2. ventas mensuales con resago
resultado_2_2 <- dbGetQuery(con, "
  WITH ventas_mensuales AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 7) AS mes,
      SUM(Total) AS total_ventas
    FROM Invoice
    GROUP BY mes
  )

  SELECT
    mes,
    total_ventas,
    LAG(total_ventas) OVER (ORDER BY mes) AS ventas_mes_anterior,
    total_ventas - LAG(total_ventas) OVER (ORDER BY mes) AS variacion_absoluta,
    AVG(total_ventas) OVER (
      ORDER BY mes
      ROWS BETWEEN 2 PRECEDING AND CURRENT ROW
    ) AS media_movil_3m
  FROM ventas_mensuales
  ORDER BY mes
")

#3. cuartil 4 meses
resultado_2_3 <- dbGetQuery(con, "
  WITH ventas_mensuales AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 7) AS mes,
      SUM(Total) AS total_ventas
    FROM Invoice
    GROUP BY mes
  ),
  cuartiles AS (
      SELECT
          mes,
          total_ventas,
          NTILE(4) OVER (ORDER BY total_ventas) AS cuartil
      FROM ventas_mensuales
  )
  SELECT *
  FROM cuartiles
  WHERE cuartil = 4;
")

#4. top mes de cada año
resultado_2_4 <- dbGetQuery(con, "
  WITH ventas_mensuales AS (
      SELECT 
          SUBSTR(InvoiceDate, 1, 7) AS mes,
          SUBSTR(InvoiceDate, 1, 4) AS año,
          SUM(Total) AS total_ventas
      FROM Invoice
      GROUP BY mes, año
  ),
  ranking AS (
      SELECT
          mes,
          año,
          total_ventas,
          RANK() OVER (
              PARTITION BY año 
              ORDER BY total_ventas DESC
          ) AS rank_ventas
      FROM ventas_mensuales
  )
  SELECT
      año,
      mes,
      total_ventas
  FROM ranking
  WHERE rank_ventas = 1
  ORDER BY año;
")
dbDisconnect(con)
