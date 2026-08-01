# --------------------------------- CLASE SQL. PARTE 1 ---------------------------------------------
# Autor: Michel Mendivenson Barragán Zabala mbarraganz@unal.edu.co
# Asignatura: Minería de datos. 2026-1 
# --------------------------------------------------------------------------------------------------

setwd(here::here())

# Conexión a bases de datos de ejemplo
library(DBI)
library(RSQLite)
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), "../_resources/chinook.db"); cat("Conexión establecida.\n")

dbListTables(con)

# ================= EJERCICIO 1: ANÁLISIS DESCRIPTIVO DE LA COLECCIÓN MUSICAL ======================
# Con la base de datos Chinook, responde las siguientes preguntas usando sentencias SQL.

# ---> 1. ¿Cuántas pistas hay en total en la base de datos?
dbGetQuery(con, '
  SELECT    COUNT (DISTINCT TrackId) as "No Songs" 
  FROM      Track
') |> print()
# ANSWER: 3503 canciones

# ----> 2. ¿Cuántos géneros distintos existen?
dbGetQuery(con, '
  SELECT    COUNT(DISTINCT(GenreId)) as "No Genres" 
  FROM      Genre') |> print()
# ANSWER: 25 géneros

# ----> 3. Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para
#          cada género. Ordena de mayor a menor número de pistas.
dbGetQuery(con, "
  SELECT   g.Name        AS genre,
           COUNT(t.TrackId) AS songs,
           AVG(t.Milliseconds)/60000.0 as `avg duration`
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY songs DESC;
") |>  print()

# ---> 4. ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.
dbGetQuery(con, '
  SELECT    t.Name as name,
            a.Title as album,
            t.Milliseconds/60000.0 as duration
  FROM      Track t
  JOIN      Album a ON t.AlbumId = a.AlbumId
  ORDER BY Milliseconds DESC
  LIMIT 5
') |> print()

# -----> 5. ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del
#           total? Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos
#           de todas las pistas.

dbGetQuery(con, '
    SELECT COUNT(TrackId) as `songs over $0.99` 
    FROM Track
    WHERE UnitPrice > 0.99
') |>  print()
# ANSWER: Hay 213 pistas con un UnitPrice mayor a $0.99

dbGetQuery(con, '
    SELECT (COUNT(TrackId) * 1.0 )/(SELECT COUNT(TrackId) FROM Track) as `percentage songs over $0.99` 
    FROM Track
    WHERE UnitPrice > 0.99
') |>  print()
# ANSWER: Las pistas con un UnitPrice mayor a $0.99 representa el 6%

dbGetQuery(con, "
  SELECT   AVG(Milliseconds) as `mean duration`, 
           MIN(Milliseconds) as `min duration`, 
           MAX(Milliseconds) as `max duration`,
           VARIANCE(Milliseconds) as `variance`
  FROM     Track
") |>  print()
# ==================================================================================================


# =========================== EJERCICIO 2: ANÁLISIS DESCRIPTIVO DE VENTAS ==========================
# Usando las tablas Invoice e InvoiceLine, responde:

# La tabla de Invoice lleva el resgistro de las facturas mientras que InvoiceLine lleva el registro
# de los detalles de cada factura (Por pista)
  
# ---> 1.¿Cuál es el total de ingresos generados por la tienda? ¿Y el promedio por factura?
dbGetQuery(con, '
  SELECT  SUM(Total) as sales, 
          AVG(Total) as `avg sales`
  FROM    Invoice')
# ANSWER: El total de ventas es USD 2328.5 mientras que el promedio de venta por factura es USD 5.6

# ----> 2. ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.
dbGetQuery(con, "
  SELECT    strftime('%Y', InvoiceDate) AS year,
            COUNT(InvoiceId) AS invoices
  FROM      Invoice
  GROUP BY  strftime('%Y', InvoiceDate) 
  ORDER BY year ASC
")

# ----> 3. Identifica los 5 países con más ingresos. Muestra: país, número de facturas, ingreso 
#          total y promedio por factura.

dbGetQuery(con, "
  SELECT    BillingCountry as country,
            COUNT(InvoiceId) AS invoices,
            SUM(Total) AS sales, 
            AVG(Total) AS `avg sales`
  FROM      Invoice
  GROUP BY  BillingCountry
  ORDER BY  sales DESC LIMIT 5
")


# ----> 4. ¿Cuál es la desviación estándar del total de facturas? Calcula primero la varianza en SQL
#          y luego obtén la raíz en R o Python.

dbGetQuery(con, "
  SELECT    VARIANCE(Total) as var
  FROM      Invoice
") |>  as.numeric() |> sqrt() |> cat()
# ANSWER: La desviacion estándar del total de facturas resulta en 4.7

# ----> 5. Encuentra los meses con mayor y menor ingreso promedio.
dbGetQuery(con, "
SELECT month, `avg sales`, 'max' AS tipo FROM (
  SELECT  strftime('%m', InvoiceDate) AS month,
            AVG(Total) AS `avg sales`
    FROM    Invoice
    GROUP BY strftime('%m', InvoiceDate)
    ORDER BY `avg sales` DESC LIMIT 1
)

UNION ALL

SELECT month, `avg sales`, 'min' AS tipo FROM (
    SELECT  strftime('%m', InvoiceDate) AS month,
            AVG(Total) AS `avg sales`
    FROM    Invoice
    GROUP BY strftime('%m', InvoiceDate)
    ORDER BY `avg sales` ASC LIMIT 1
)
")

# ==================================================================================================

dbDisconnect(con)

# EOF