# Sesión 5

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
cat("Conexión establecida.\n")

# Listamos las tablas disponibles
dbListFields(con, "Customer")
dbListFields(con, "Invoice")
dbListFields(con, "InvoiceLine")
dbListFields(con, "Track")
dbListFields(con, "Album")
dbListFields(con, "Genre")

## Ejercicio 1: Análisis de ventas con JOINs y CTEs

#  Usando las tablas Invoice, InvoiceLine, Track, Album, Artist y Customer de Chinook:

### 1.Escribe una consulta con INNER JOIN que devuelva: nombre del cliente, país,
# nombre de la pista comprada, género y precio unitario. Limita a 15 filas.

resultado1 <- dbGetQuery (con, "
   SELECT c.FirstName || '' || c.LastName AS nombre_cliente,
          c.Country                       AS pais,
          t.Name                          As pista,
          g.Name                          As genero,
          il.UnitPrice                    As precio_unitario
   From Customer c
   INNER JOIN Invoice i ON c.CustomerId = i.CustomerId
   INNER JOIN InvoiceLine il On i.invoiceId = il.invoiceId
   INNER JOIN Track T ON il.TrackId = t.TrackId
   INNER JOIN Genre g ON t.GenreId = g.GenreId
   Limit 15
")

resultado1

### 2.Usa un LEFT JOIN para encontrar pistas que nunca han sido vendidas (que no aparecen en InvoiceLine).

resultado2 <- dbGetQuery(con,"
   SELECT t.Name         As pista,
          t.UnitPrice    As precio
   From      Track t
   LEFT JOIN InvoiceLine il ON t.TrackId = il.TrackId
   WHERE il.TrackId IS NULL
   Limit 10;
")

resultado2

### 3.Con una CTE, calcula el total de ingresos por artista (suma de UnitPrice × Quantity de sus pistas vendidas). 
# Muestra los 10 artistas con más ingresos.

resultado3 <- dbGetQuery(con, "
  WITH ingresos_artistas AS (
    SELECT ar.Name                                   AS artista,
           ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS total_ingreso
    FROM        InvoiceLine il
    JOIN        Track       t  ON il.TrackId  = t.TrackId
    JOIN        Album       al ON t.AlbumId   = al.AlbumId
    JOIN        Artist      ar ON al.ArtistId = ar.ArtistId
    GROUP BY ar.Name
  )
  SELECT *
  FROM   ingresos_artistas
  ORDER  BY total_ingreso DESC
  LIMIT  10;
")
resultado3

### 4. Extiende la CTE anterior para agregar:

#### 4.1 La participación porcentual de cada artista sobre el total de ingresos.

resultado4.1 <- dbGetQuery(con, "
  WITH ingresos_artistas AS (
    SELECT ar.Name                                   AS artista,
           ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS total_ingreso
    FROM        InvoiceLine il
    JOIN        Track       t  ON il.TrackId  = t.TrackId
    JOIN        Album       al ON t.AlbumId   = al.AlbumId
    JOIN        Artist      ar ON al.ArtistId = ar.ArtistId
    GROUP BY ar.Name
  )
  SELECT *,
         ROUND(total_ingreso * 100.0 / (SELECT SUM(total_ingreso) FROM ingresos_artistas), 2) AS porcentaje
  FROM   ingresos_artistas
  ORDER  BY total_ingreso DESC
  LIMIT  10;
")
resultado4.1

#### 4.2 Un RANK() por ingresos.

resultado4.2 <- dbGetQuery(con, "
  WITH ingresos_artistas AS (
    SELECT ar.Name                                   AS artista,
           ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS total_ingreso
    FROM        InvoiceLine il
    JOIN        Track       t  ON il.TrackId  = t.TrackId
    JOIN        Album       al ON t.AlbumId   = al.AlbumId
    JOIN        Artist      ar ON al.ArtistId = ar.ArtistId
    GROUP BY ar.Name
  )
  SELECT *,
         ROUND (total_ingreso * 100.0 / (SELECT SUM(total_ingreso) FROM ingresos_artistas),2) AS porcentaje,
         RANK()       OVER (ORDER BY total_ingreso DESC) AS rank_total_ingreso
  FROM   ingresos_artistas
  ORDER  BY total_ingreso DESC
  LIMIT  10;
")

resultado4.2

## Ejercicio 2: Análisis temporal con funciones de ventana
# Usando la tabla Invoice:
  
### 5. Calcula el total de ventas por año y mes (SUBSTR(InvoiceDate, 1, 7)). Ordena cronológicamente.

resultado5 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
         ROUND(SUM(Total), 2)      AS ventas
  FROM   Invoice
  GROUP BY anio_mes
  ORDER BY anio_mes
  LIMIT 10
")

resultado5

### 6.Agrega a la consulta anterior:
  
#### LAG: ventas del mes anterior.

resultado6.1 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
         ROUND(SUM(Total), 2)      AS ventas,
         LAG(ROUND(SUM(Total),2)) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7)) AS ventas_mes_anterior
  FROM   Invoice
  GROUP BY anio_mes
  ORDER BY anio_mes
  LIMIT 10
")

resultado6.1

#### Variación absoluta respecto al mes anterior.

resultado6.2 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
         ROUND(SUM(Total), 2)      AS ventas,
         LAG(ROUND(SUM(Total),2)) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7))                     AS ventas_mes_anterior,
         ROUND(ROUND(SUM(Total),2) - LAG(ROUND(SUM(Total),2), 1) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7)), 2)  AS variacion_absoluta
  FROM   Invoice
  GROUP BY anio_mes
  ORDER BY anio_mes
  LIMIT 10
")

resultado6.2

#### Media móvil de 3 meses (ROWS BETWEEN 2 PRECEDING AND CURRENT ROW).

resultado6.3 <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
         ROUND(SUM(Total), 2)      AS ventas,
         LAG(ROUND(SUM(Total),2)) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7))                     AS ventas_mes_anterior,
         ROUND(ROUND(SUM(Total),2) - LAG(ROUND(SUM(Total),2), 1) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7)), 2)  AS variacion_absoluta,
         ROUND(AVG(ROUND(SUM(Total),2)) OVER (ORDER BY SUBSTR(InvoiceDate, 1, 7) ROWS BETWEEN 2 PRECEDING AND CURRENT ROW),2) AS media_movil
  FROM   Invoice
  GROUP BY anio_mes
  ORDER BY anio_mes
  LIMIT 10
")

resultado6.3


### 7. Usando NTILE(4), clasifica cada mes en cuartiles según su nivel de ventas. ¿Cuáles meses pertenecen al cuartil superior (Q4)?

resultado7 <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7)                    AS anio_mes,
           ROUND(SUM(Total), 2)                          AS ventas,
           NTILE(4) OVER (ORDER BY ROUND(SUM(Total), 2)) AS cuartil
    FROM   Invoice
    GROUP BY anio_mes
  )
  SELECT *
  FROM   ventas_mes
  WHERE  cuartil = 4
  ORDER BY ventas DESC
  LIMIT 10;
")
resultado7


### 8. Con RANK(), identifica el mes con mayores ventas de cada año. Usa una CTE para calcular primero las ventas mensuales, luego aplica RANK() con PARTITION BY el año.

resultado8 <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
           SUBSTR(InvoiceDate, 1, 4) AS anio,
           ROUND(SUM(Total), 2)      AS ventas
    FROM   Invoice
    GROUP BY anio_mes
  ),
  ventas_rank AS (
    SELECT *,
           RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS ranking
    FROM   ventas_mes
  )
  SELECT *
  FROM   ventas_rank
  WHERE  ranking = 1
  ORDER BY anio;
")
resultado8

