#--------------------------
#       EJERCICIO 1
#--------------------------

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

# Instalamos los paquetes necesarios
paquetes  <- c("DBI", "RSQLite", "dplyr", "knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")

dbListTables(con)

# 1.1: INNER JOIN – Cliente, país, pista comprada, género y precio unitario (15 filas)

inner_join_clientes <- dbGetQuery(con, "
  SELECT Customer.FirstName || ' ' || Customer.LastName  AS cliente,
         Customer.Country                                AS pais,
         Track.Name                                      AS pista,
         Genre.Name                                      AS genero,
         InvoiceLine.UnitPrice                           AS precio_unitario
  FROM   InvoiceLine
  JOIN   Track    ON InvoiceLine.TrackId  = Track.TrackId
  JOIN   Genre    ON Track.GenreId        = Genre.GenreId
  JOIN   Invoice  ON InvoiceLine.InvoiceId = Invoice.InvoiceId
  JOIN   Customer ON Invoice.CustomerId   = Customer.CustomerId
  LIMIT  15;
")

inner_join_clientes

# 1.2: LEFT JOIN – Pistas que nunca han sido vendidas

pistas_sin_venta <- dbGetQuery(con, "
  SELECT Track.TrackId,
         Track.Name     AS pista
  FROM   Track
  LEFT JOIN InvoiceLine ON Track.TrackId = InvoiceLine.TrackId
  WHERE  InvoiceLine.TrackId IS NULL
  LIMIT  20;
")

pistas_sin_venta

# 1.3: CTE – Ingresos totales por artista (UnitPrice × Quantity) – Top 10

top_artistas_cte <- dbGetQuery(con, "
  WITH ingresos_artista AS (
    SELECT Artist.Name                                      AS artista,
           SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS ingresos
    FROM   Artist
    JOIN   Album       ON Artist.ArtistId = Album.ArtistId
    JOIN   Track       ON Album.AlbumId   = Track.AlbumId
    JOIN   InvoiceLine ON Track.TrackId   = InvoiceLine.TrackId
    GROUP BY Artist.ArtistId, Artist.Name
  )
  SELECT artista,
         ROUND(ingresos, 2) AS ingresos
  FROM   ingresos_artista
  ORDER  BY ingresos DESC
  LIMIT  10;
")

top_artistas_cte

# 1.4: CTE extendida – Participación porcentual y RANK() sobre ingresos

ranking_artistas <- dbGetQuery(con, "
  WITH ingresos_artista AS (
    SELECT Artist.Name                                      AS artista,
           SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS ingresos
    FROM   Artist
    JOIN   Album       ON Artist.ArtistId = Album.ArtistId
    JOIN   Track       ON Album.AlbumId   = Track.AlbumId
    JOIN   InvoiceLine ON Track.TrackId   = InvoiceLine.TrackId
    GROUP BY Artist.ArtistId, Artist.Name
  ),
  total AS (
    SELECT SUM(ingresos) AS total_ingresos
    FROM   ingresos_artista
  )
  SELECT ingresos_artista.artista,
         ROUND(ingresos_artista.ingresos, 2)                            AS ingresos,
         ROUND(100.0 * ingresos_artista.ingresos / total.total_ingresos, 2) AS pct_mercado,
         RANK() OVER (ORDER BY ingresos_artista.ingresos DESC)          AS ranking
  FROM   ingresos_artista, total
  ORDER  BY ranking
  LIMIT  15;
")

ranking_artistas

#--------------------------
#       EJERCICIO 2
#--------------------------

# 2.1: Ventas totales por año y mes (orden cronológico)

ventas_anio_mes <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
         ROUND(SUM(Total), 2)       AS ventas
  FROM   Invoice
  GROUP BY anio_mes
  ORDER BY anio_mes;
")

ventas_anio_mes

# 2.2: Agregar LAG, variación absoluta y media móvil de 3 meses

ventas_mensuales <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
           ROUND(SUM(Total), 2)       AS ventas
    FROM   Invoice
    GROUP BY anio_mes
  )
  SELECT anio_mes,
         ventas,
         LAG(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_anterior,
         ROUND(ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes), 2) AS variacion_absoluta,
         ROUND(AVG(ventas) OVER (ORDER BY anio_mes
                                 ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) AS media_movil_3m
  FROM   ventas_mes
  ORDER  BY anio_mes;
")

ventas_mensuales

# 2.3: NTILE(4) – Meses del cuartil superior (Q4)

cuartiles_ventas <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
           ROUND(SUM(Total), 2)       AS ventas
    FROM   Invoice
    GROUP BY anio_mes
  ),
  cuartiles AS (
    SELECT anio_mes,
           ventas,
           NTILE(4) OVER (ORDER BY ventas) AS cuartil
    FROM   ventas_mes
  )
  SELECT anio_mes, ventas
  FROM   cuartiles
  WHERE  cuartil = 4
  ORDER  BY anio_mes;
")

cuartiles_ventas

# 2.4: RANK() – Mejor mes de cada año

mejor_mes_anio <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 4)  AS anio,
           SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
           ROUND(SUM(Total), 2)       AS ventas
    FROM   Invoice
    GROUP BY anio, anio_mes
  ),
  ranking_mes AS (
    SELECT anio,
           anio_mes,
           ventas,
           RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rank_mes
    FROM   ventas_mes
  )
  SELECT anio, anio_mes, ventas
  FROM   ranking_mes
  WHERE  rank_mes = 1
  ORDER  BY anio;
")

mejor_mes_anio

# Cierre de conexión
dbDisconnect(con)

