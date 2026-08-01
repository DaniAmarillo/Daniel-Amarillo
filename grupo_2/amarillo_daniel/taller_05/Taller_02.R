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
dbListTables(con)
dbListFields(con, "INVOICELINE")

##EJERCICIO 1.##

##1. Escribe una consulta con INNER JOIN que devuelva: nombre del cliente, país, nombre de la pista comprada, género y precio unitario. Limita a 15 filas. ##
dbGetQuery(con, "
  SELECT CONCAT(cu.FirstName, ' ', cu.LastName) AS nombre_completo,
         cu.Country AS pais,
         il.UnitPrice AS precio_unitario,
         t.Name AS Titulo,
         g.Name AS Genero
  FROM   Invoice  i
  INNER JOIN Customer cu  ON i.CustomerId = cu.CustomerId
  INNER JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
  INNER JOIN Track t ON il.TrackId = t.TrackId
  INNER JOIN Genre g ON g.GenreId = t.GenreId
  LIMIT  15;
")

##2. Usa un LEFT JOIN para encontrar pistas que nunca han sido vendidas (que no aparecen en InvoiceLine).##
dbGetQuery(con,"SELECT 
           t.Name AS Pista_sin_ventas
           FROM Track t
           LEFT JOIN InvoiceLine il ON t.TrackId = il.TrackId 
           WHERE il.InvoiceLineId IS NULL
           LIMIT 15")


##3. Con una CTE, calcula el total de ingresos por artista (suma de UnitPrice × Quantity de sus pistas vendidas). Muestra los 10 artistas con más ingresos.##

dbGetQuery(con,"WITH ingresos_por_artista AS(
                  SELECT *,
                  art.Name AS Artista
                  FROM invoiceLine il
                  INNER JOIN Track t ON t.TrackId = il.TrackId
                  INNER JOIN Album a On t.AlbumId = a.AlbumId
                  INNER JOIN Artist art On a.ArtistId = art.ArtistId

)
                SELECT Artista,
                SUM(UnitPrice) AS Ingresos 
                FROM ingresos_por_artista
                GROUP BY Artista
                ORDER BY Ingresos DESC
                LIMIT 5;
")

dbGetQuery(con,"select * 
           from Track")
