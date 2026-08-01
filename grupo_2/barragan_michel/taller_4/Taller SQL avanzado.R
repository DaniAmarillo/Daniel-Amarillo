# --------------------------------- CLASE SQL. PARTE 2 ---------------------------------------------
# Autor: Michel Mendivenson Barragán Zabala mbarraganz@unal.edu.co
# Materia: Minería de datos. 2026-1 
# --------------------------------------------------------------------------------------------------

setwd(here::here())

# Conexión a bases de datos de ejemplo
library(DBI)
library(RSQLite)
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), "../_resources/chinook.db"); cat("Conexión establecida.\n")

dbListTables(con)

# ======================== EJERCICIO 1: ANÁLISIS DE VENTAS CON JOINS Y CTR =========================
# Usando las tablas Invoice, InvoiceLine, Track, Album, Artist y Customer de Chinook:

# ----> 1. Escribe una consulta con INNER JOIN que devuelva: nombre del cliente, país, nombre de la
#          pista comprada, género y precio unitario. Limita a 15 filas.

dbGetQuery(con, "
  SELECT  (cust.FirstName || ' ' || cust.LastName) AS `customer name`,
          inv.BillingCountry AS `invoice country`,
          track.Name AS track,
          genre.Name AS genre,
          line.UnitPrice AS price
  FROM         InvoiceLine line
  INNER JOIN   Track       track  ON line.TrackId    = track.TrackId
  INNER JOIN   Invoice     inv    ON line.InvoiceId  = inv.InvoiceId
  INNER JOIN   Customer    cust   ON inv.CustomerId  = cust.CustomerId
  INNER JOIN   Genre       genre  ON track.GenreId   = genre.GenreId
  LIMIT 15
")

# -----> 2. Usa un LEFT JOIN para encontrar pistas que nunca han sido vendidas (que no aparecen en
#           InvoiceLine).

dbGetQuery(con, "
  SELECT    t.Name AS track, 
            t.Composer AS composer,
            t.Milliseconds/60000.0 AS duration,
            t.UnitPrice AS price
  FROM      Track t 
  LEFT JOIN InvoiceLine AS inv ON t.TrackId = inv.TrackId
  WHERE InvoiceLineId IS NULL
  LIMIT 10")

# -----> 3. Con una CTE, calcula el total de ingresos por artista (suma de UnitPrice × Quantity de
#           sus pistas vendidas). Muestra los 10 artistas con más ingresos.

dbGetQuery(con, "
  WITH TrackTotal AS (
      ----- INGRESOS TOTALES POR PISTAS -----
      SELECT    TrackId,
                SUM(UnitPrice) AS total
      FROM      InvoiceLine
      GROUP BY  TrackId
  ), 
  
  Composer AS (
      ----- COMPOSER x TRACK -----
      SELECT    TrackId,
                Composer AS composer
      FROM      Track
  )
  
  SELECT    c.composer,
            SUM(t.total) AS `total revenue`
  FROM      TrackTotal t
  LEFT JOIN Composer c ON t.TrackId = c.TrackId
  GROUP BY  composer
  ORDER BY `total revenue` DESC
  LIMIT 10;
")

# -----> 4. Extiende la CTE anterior para agregar:
#           - La participación porcentual de cada artista sobre el total de ingresos.
#           - Un RANK() por ingresos.

dbGetQuery(con, "
  WITH TrackTotal AS (
      ----- INGRESOS TOTALES POR PISTAS -----
      SELECT    TrackId,
                SUM(UnitPrice) AS total
      FROM      InvoiceLine
      GROUP BY  TrackId
  ), 
  
  Composer AS (
      ----- COMPOSER x TRACK -----
      SELECT    TrackId,
                Composer AS composer
      FROM      Track
  ), 
  
  ComposerTotal AS (
      SELECT    c.composer,
                SUM(t.total) AS `total revenue`
      FROM      TrackTotal t
      LEFT JOIN Composer c ON t.TrackId = c.TrackId
      GROUP BY  composer
      ORDER BY `total revenue`
  )
  
  SELECT    *, 
            RANK()   OVER (ORDER BY `total revenue` DESC) AS ranking, 
            `total revenue`/SUM(`total revenue`) OVER () * 100.0 AS `% participation`
  FROM      ComposerTotal
  LIMIT 10
")

# ==================================================================================================



# ====================== EJERCICIO 2: ANÁLISIS TEMPORAL CON FUNCIONES DE VENTANA ===================
# Usando la tabla Invoice:

# -----> 1. Calcula el total de ventas por año y mes (SUBSTR(InvoiceDate, 1, 7)).
#        Ordena cronológicamente.

dbGetQuery(con, "
  SELECT    strftime('%Y', InvoiceDate) AS year, 
            strftime('%m', InvoiceDate) AS month,
            SUM(Total) AS total
  FROM      Invoice
  GROUP BY  year, month
  ORDER BY year, month
")

# -----> 2. Agrega a la consulta anterior:
#           - LAG: ventas del mes anterior.
#           - Variación absoluta respecto al mes anterior.
#           - Media móvil de 3 meses (ROWS BETWEEN 2 PRECEDING AND CURRENT ROW).

dbGetQuery(con, "
  WITH YearMonth AS (
      ---- Ventas por año y mes ordenadas de forma cronológica -----
      SELECT    strftime('%Y', InvoiceDate) AS year, 
                strftime('%m', InvoiceDate) AS month,
                SUM(Total) AS total
      FROM      Invoice
      GROUP BY  year, month
      ORDER BY year, month
  )
  
  SELECT    *, 
            LAG(total, 1) OVER (ORDER BY year, month) AS lag, 
            total - (LAG(total, 1) OVER (ORDER BY year, month)) AS variation, 
            ROUND(AVG(total) OVER (ORDER BY year, month
               ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) AS `moving average 3m`
  FROM      YearMonth
")

# -----> 3. Usando NTILE(4), clasifica cada mes en cuartiles según su nivel de ventas.  ¿Cuáles 
#           meses pertenecen al cuartil superior (Q4)?

dbGetQuery(con, "
  SELECT * FROM (
      SELECT    strftime('%Y', InvoiceDate) AS year, 
                strftime('%m', InvoiceDate) AS month,
                SUM(Total) AS total, 
                NTILE(4) OVER (ORDER BY SUM(Total)) AS quartile
      FROM      Invoice
      GROUP BY  year, month
  )
  WHERE quartile = 4
")

# -----> 4. Con RANK(), identifica el mes con mayores ventas de cada año. Usa una CTE para calcular
#           primero las ventas mensuales, luego aplica RANK() con PARTITION BY el año.

dbGetQuery(con, "
  WITH YearMonth AS (
      SELECT    strftime('%Y', InvoiceDate) AS year, 
                strftime('%m', InvoiceDate) AS month,
                SUM(Total) AS total,
                RANK() OVER (
                    PARTITION BY strftime('%Y', InvoiceDate) 
                    ORDER BY SUM(Total) DESC) 
                    AS monthRank
      FROM      Invoice
      GROUP BY  year, month
      ORDER BY  year, month
  )
  
  SELECT    *
  FROM      YearMonth
  WHERE MonthRank = 1
")

# Nota: Si se quisiera escoger solamente el primer (o último) mes que es de rango 1 debería usar 
#       ROW_NUMBER() en cambio de RANK()

# ==================================================================================================

dbDisconnect(con)

# EOF