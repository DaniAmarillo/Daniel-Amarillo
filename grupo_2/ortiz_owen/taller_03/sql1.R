#importamos las librerias
library(DBI)
library(RSQLite)
library(dplyr)
library(knitr)

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

# Listamos las tablas disponibles
dbListTables(con)

# Columnas de la tabla Track
dbListFields(con, "Track")

#ver la tabla
dbReadTable(con, "Album")

###Ejercicio 1###

#1.
total_pistas <- dbGetQuery(con, "SELECT COUNT(*) AS total_pistas FROM Track;")

View(total_pistas, title = "Conteos de la tabla Track")

#2 
generos_distintos <- dbGetQuery(con, "SELECT COUNT(DISTINCT GenreId) AS generos_distintos FROM Track;")

View(generos_distintos, title = "Conteos de la tabla Track")

#3
estadisticas_pistas <- dbGetQuery(con, "
  SELECT  
    g.Name                                AS genero,
    COUNT(t.TrackId)                      AS numero_de_pistas,
    ROUND(AVG(t.Milliseconds)/60000.0, 2) AS duracion_media_min
  FROM      Track t
  JOIN      Genre g ON t.GenreId = g.GenreId
  GROUP BY  g.Name
  ORDER BY  duracion_media_min DESC;
")
View(estadisticas_pistas, title = "Estadísticas de las pistas")

#4
pistas_largas <- dbGetQuery(con, "
  SELECT 
    t.Name AS cancion, 
    a.Title AS album, 
    ROUND(t.Milliseconds / 60000.0, 2) AS duracion_minutos
  FROM Track t
  JOIN Album a ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5;
")
View(pistas_largas)

#5
analisis_precios <- dbGetQuery(con, "
  SELECT 
    COUNT(*) AS cantidad_caras,
    (SELECT COUNT(*) FROM Track) AS total_pistas,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM Track), 2) AS porcentaje
  FROM Track
  WHERE UnitPrice > 0.99;
")
View(analisis_precios)

#6
resumen <- dbGetQuery(con, "
  SELECT
    g.Name                                    AS genero,
    COUNT(t.TrackId)                          AS n_pistas,
    ROUND(AVG(t.Milliseconds)/60000.0, 2)     AS duracion_media_min,
    ROUND(MIN(t.Milliseconds)/60000.0, 2)     AS duracion_min_min,
    ROUND(MAX(t.Milliseconds)/60000.0, 2)     AS duracion_max_min,
    ROUND(AVG(t.UnitPrice), 2)                AS precio_medio,
    ROUND(
      AVG(t.Milliseconds * t.Milliseconds) - 
      AVG(t.Milliseconds) * AVG(t.Milliseconds), 
    0)                                        AS varianza_ms
  FROM      Track t
  JOIN      Genre g ON t.GenreId = g.GenreId
  GROUP BY  g.Name
  ORDER BY  n_pistas DESC;
")
View(resumen)

###Ejercicio 2###

#1
ingresos <- dbGetQuery(con, "
  SELECT 
    SUM(Total) AS ingresos_totales,
    AVG(Total) AS promedio_por_factura,
    COUNT(InvoiceId) AS numero_total_facturas
  FROM Invoice;
")
print(ingresos)

#2
facturas_por_ano <- dbGetQuery(con, "
  SELECT 
    strftime('%Y', InvoiceDate) AS anio,
    COUNT(InvoiceId)            AS numero_de_facturas
  FROM   Invoice
  GROUP BY anio
  ORDER BY anio ASC;
")
View(facturas_por_ano)

#3
paises <- dbGetQuery(con, "
  SELECT 
    BillingCountry AS pais, 
    COUNT(InvoiceId) AS n_facturas, 
    ROUND(SUM(Total), 2) AS ingreso_total, 
    ROUND(AVG(Total), 2) AS promedio_por_factura
  FROM Invoice
  GROUP BY BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT 5;
")
View(paises)

#4
desviacion_facturas <- dbGetQuery(con, "
  SELECT 
    ROUND(
      SQRT(
        AVG(Total * Total) - AVG(Total) * AVG(Total)
      ), 2) AS desviacion_estandar_total
  FROM Invoice;
")
View(desviacion_facturas)

#5
meses <- dbGetQuery(con, "
  SELECT 
    strftime('%m', InvoiceDate) AS mes,
    ROUND(AVG(Total), 2)        AS ingreso_promedio,
    COUNT(InvoiceId)            AS total_facturas
  FROM   Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")
View(meses)





















