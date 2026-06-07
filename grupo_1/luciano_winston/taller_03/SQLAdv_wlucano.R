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
dbListTables(con)

# Columnas de la tabla Track
dbListFields(con, "Track")
dbListFields(con, "Artist")
dbListFields(con, "Album")
dbListFields(con, "Invoice")
dbListFields(con, "InvoiceLine")
dbListFields(con, "Customer")

#############################################################################

resultado_compras <- dbGetQuery(con, "
                      SELECT 
                          c.FirstName || ' ' || c.LastName AS Cliente,
                          c.Country AS Pais,
                          t.Name AS Pista,
                          g.Name AS Genero,
                          il.UnitPrice AS Precio
                      FROM Customer c
                      INNER JOIN Invoice i     ON c.CustomerId = i.CustomerId
                      INNER JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
                      INNER JOIN Track t       ON il.TrackId = t.TrackId
                      INNER JOIN Genre g       ON t.GenreId = g.GenreId
                      LIMIT 15
")
resultado_compras

#############################################################################

pistas_no_vendidas <- dbGetQuery(con, "
                                 SELECT t.TrackId As N_Track,
                                        t.Name AS Pista_No_Vendida
                                 FROM Track t
                                 LEFT JOIN InvoiceLine il ON il.TrackId = t.TrackId
                                 WHERE il.InvoiceLineId IS NULL;
                                 ")
pistas_no_vendidas


# Para ver cuántas pistas nunca se han vendido
cat("Total de pistas sin ventas:", nrow(pistas_no_vendidas))

#############################################################################
CTE <- dbGetQuery(con, "
                  WITH IngresosA AS (
                  SELECT art.Name AS Artista,
                        (il.UnitPrice * il.Quantity) AS Subtotal
                  FROM InvoiceLine il
                  JOIN Track t ON il.TrackId = t.TrackId
                  JOIN Album a ON t.AlbumId = a.AlbumId
                  JOIN Artist art ON a.ArtistId = art.ArtistId
                  )
                  SELECT Artista, SUM(Subtotal) AS Total
                  FROM IngresosA
                  GROUP BY Artista
                  ORDER BY Total DESC
                  LIMIT 10
")
#############################################################################
CTE_exp <- dbGetQuery(con, "
                  WITH IngresosA AS (
                  SELECT art.Name AS Artista,
                        (il.UnitPrice * il.Quantity) AS Subtotal
                  FROM InvoiceLine il
                  JOIN Track t ON il.TrackId = t.TrackId
                  JOIN Album a ON t.AlbumId = a.AlbumId
                  JOIN Artist art ON a.ArtistId = art.ArtistId
                  ),
                  Total_Global AS (
                  SELECT SUM(Subtotal) AS Total_ingresos
                  FROM IngresosA
                  )
                  SELECT Artista, 
                         SUM(Subtotal) AS Total,
                         ROUND(t.Total_ingresos, 2) AS Total_global,
                         ROUND((SUM(Subtotal)/t.Total_ingresos)*100, 2) AS Porcentaje_participacion
                  FROM IngresosA
                  CROSS JOIN Total_Global t
                  GROUP BY Artista
                  ORDER BY Porcentaje_participacion DESC
                  LIMIT 10
                  
")
#############################################################################
CTE_exp_rank <- dbGetQuery(con, "
                    WITH IngresosDetalle AS (
                        SELECT art.Name AS Artista,
                               (il.UnitPrice * il.Quantity) AS Subtotal
                        FROM InvoiceLine il
                        JOIN Track t ON il.TrackId = t.TrackId
                        JOIN Album a ON t.AlbumId = a.AlbumId
                        JOIN Artist art ON a.ArtistId = art.ArtistId
                    ),
                    TotalesPorArtista AS (
                        SELECT Artista, 
                               SUM(Subtotal) AS Total_Artista
                        FROM IngresosDetalle
                        GROUP BY Artista
                    ),
                    MetricaGlobal AS (
                        SELECT SUM(Total_Artista) AS Gran_Total 
                        FROM TotalesPorArtista
                    )
                    SELECT t.Artista, 
                           ROUND(t.Total_Artista, 2) AS Ventas,
                           ROUND((t.Total_Artista / m.Gran_Total) * 100, 2) AS Porcentaje_Participacion,
                           RANK() OVER (ORDER BY t.Total_Artista DESC) AS Ranking_Posicion
                    FROM TotalesPorArtista t
                    CROSS JOIN MetricaGlobal m
                    ORDER BY Ranking_Posicion ASC
                    LIMIT 10
")
#############################################################################
#Ejercicio 2
#############################################################################
MyA <- dbGetQuery(con, "
                 SELECT (SUBSTR(InvoiceDate, 1, 7)) As Año,
                        Total
                 FROM Invoice
                 GROUP BY Año
                 ")


MA <- dbGetQuery(con, "
                 SELECT Mes,
                        Venta_Mes,
                        LAG(Venta_Mes) OVER (ORDER BY Mes) AS Ventas_Mes_Anterior,
                        ROUND(Venta_Mes - LAG(Venta_Mes) OVER (ORDER BY Mes), 2) AS Variacion_Absoluta,
                        ROUND(AVG(Venta_Mes) OVER (
                 ORDER BY Mes 
                   ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) AS Media_Movil_3Meses
                 FROM (
                 SELECT SUBSTR(InvoiceDate, 1, 7) AS Mes,
                 SUM(Total) AS Venta_Mes
                 FROM Invoice
                 GROUP BY Mes)
                ")

Mq <- dbGetQuery(con, "
                  WITH VentasPorMes AS (
                      SELECT SUBSTR(InvoiceDate, 1, 7) AS Mes,
                             SUM(Total) AS Venta_Mes
                      FROM Invoice
                      GROUP BY Mes
                  ),
                  Clasificacion AS (
                      SELECT Mes,
                             Venta_Mes,
                             NTILE(4) OVER (ORDER BY Venta_Mes ASC) AS Cuartil
                      FROM VentasPorMes
                  )
                  SELECT Mes, ROUND(Venta_Mes, 2) AS Ventas
                  FROM Clasificacion
                  WHERE Cuartil = 4
                  ORDER BY Ventas DESC
")



MejoresMeses <- dbGetQuery(con, "
                  WITH VentasMensuales AS (
                      SELECT 
                          SUBSTR(InvoiceDate, 1, 4) AS Año,
                          SUBSTR(InvoiceDate, 1, 7) AS Mes,
                          SUM(Total) AS Venta_Mes
                      FROM Invoice
                      GROUP BY Mes
                  ),
                  RankingPorAnio AS (
                      SELECT 
                          Año,
                          Mes,
                          Venta_Mes,
                          RANK() OVER (
                              PARTITION BY Año 
                              ORDER BY Venta_Mes DESC
                          ) AS Posicion
                      FROM VentasMensuales
                  )
                  SELECT Año, Mes, ROUND(Venta_Mes, 2) AS Ventas_Maximas
                  FROM RankingPorAnio
                  WHERE Posicion = 1
                  ORDER BY Año DESC
")
