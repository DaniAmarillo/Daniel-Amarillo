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

# 1: Conteo total de pistas en la base de datos

conteo <- dbGetQuery(con, "
                     SELECT COUNT(*) AS Total_de_Pistas
                     FROM Track
                     ")
# Mostrar conteo
conteo

# 2: Conteo total de géneros distintos

# Verificar Géneros
dbGetQuery(con, "
           SELECT *
           FROM Genre
           ")

# Como son únicos en la tabla anterior podemos contar directamente como el ejemplo anterior
conteo_genero <- dbGetQuery(con,"
                            SELECT COUNT(*) AS Total_de_Géneros
                            FROM Genre
                            ")

conteo_genero

# 3: Nombre del género + cantidad de pistas y duración media en minutos

# Verificar columnas
dbListFields(con,"Track")
dbListFields(con,"Genre")

# Creación de la consulta
conteo_pistas_tiempo <- dbGetQuery(con,"
                            SELECT Genre.Name AS Genero,
                                   COUNT(Track.TrackId) AS Conteo_Pistas,
                                   ROUND(AVG(Track.Milliseconds)/60000,2) AS Duración_Promedio_Min
                            FROM Track
                            JOIN  Genre ON Track.GenreId = Genre.GenreId
                            GROUP BY Genre.Name
                            ORDER BY Duración_Promedio_Min DESC;
                            ")

conteo_pistas_tiempo

# 4: Top 5 pistas más largas

#Revisar contenidos de nuevo
dbListTables(con)
dbListFields(con,"Track")
dbListFields(con,"Album")

conteo_pistas_top5 <- dbGetQuery(con,"
                                   SELECT Track.Name AS Nombre,
                                          Album.Title AS Album,
                                          ROUND(Track.Milliseconds/60000,2) AS Duracion_Min
                                   FROM   Track
                                   JOIN   Album ON Album.AlbumId = Track.AlbumId
                                   ORDER BY Duracion_Min DESC
                                   LIMIT 5;
                                   ")
conteo_pistas_top5

# 5: ¿Cuántas pistas tienen un UnitPrice superior a $0.99?
#    ¿Qué porcentaje representan del total?

pistas_precio <- dbGetQuery(con, "
                            SELECT COUNT(*) AS Total_de_Pistas,
                                   SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) AS Pistas_Con_UnitPrice_Superior_0_99,
                                   ROUND(100.0 * SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) / COUNT(*), 2) AS Porcentaje_Sobre_Total
                            FROM Track;
                            ")

pistas_precio

# 6: Media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos

estadisticas_duracion <- dbGetQuery(con, "
                                  SELECT ROUND(AVG(Milliseconds), 2) AS Media_Milisegundos,
                                         MIN(Milliseconds) AS Minimo_Milisegundos,
                                         MAX(Milliseconds) AS Maximo_Milisegundos,
                                         ROUND(
                                           AVG(1.0 * Milliseconds * Milliseconds) -
                                           AVG(1.0 * Milliseconds) * AVG(1.0 * Milliseconds),
                                           2
                                         ) AS Varianza_Poblacional_Milisegundos
                                  FROM Track;
                                  ")

estadisticas_duracion

#--------------------------
#       EJERCICIO 2
#--------------------------

# 1: ¿Cuál es el total de ingresos generados por la tienda?
#    ¿Y el promedio por factura?

ingresos_tienda <- dbGetQuery(con, "
                             SELECT ROUND(SUM(Total), 2) AS Ingreso_Total,
                                    ROUND(AVG(Total), 2) AS Promedio_Por_Factura
                             FROM Invoice;
                             ")

ingresos_tienda

# 2: ¿Cuántas facturas se emitieron por año?
#    Orden cronológicamente.

facturas_por_anio <- dbGetQuery(con, "
                               SELECT strftime('%Y', InvoiceDate) AS Anio,
                                      COUNT(*) AS Numero_Facturas
                               FROM Invoice
                               GROUP BY strftime('%Y', InvoiceDate)
                               ORDER BY Anio;
                               ")

facturas_por_anio

# 3: Identifica los 5 países con más ingresos.
#    Muestra: país, número de facturas, ingreso total y promedio por factura.

# Revisar tabla
dbListFields(con,"Invoice")

paises_mas_ingresos <- dbGetQuery(con, "
                                 SELECT BillingCountry AS Pais,
                                        COUNT(*) AS Numero_Facturas,
                                        ROUND(SUM(Total), 2) AS Ingreso_Total,
                                        ROUND(AVG(Total), 2) AS Promedio_Por_Factura
                                 FROM Invoice
                                 GROUP BY BillingCountry
                                 ORDER BY Ingreso_Total DESC
                                 LIMIT 5;
                                 ")

paises_mas_ingresos

# 4: ¿Cuál es la desviación estándar del total de facturas?
#    Calcula primero la varianza en SQL y luego obtén la raíz en R.

varianza_facturas <- dbGetQuery(con, "
                               SELECT ROUND(
                                 AVG(1.0 * Total * Total) - AVG(1.0 * Total) * AVG(1.0 * Total),
                                 6
                               ) AS Varianza_Facturas # Var(X) = E(X^2) - E^2(X)
                               FROM Invoice;
                               ")

varianza_facturas

# Raíz cuadrada de la varianza
desviacion_estandar_facturas <- sqrt(varianza_facturas$Varianza_Facturas)

desviacion_estandar_facturas

# 5: Encuentra los meses con mayor y menor ingreso promedio.

ingreso_promedio_por_mes <- dbGetQuery(con, "
                                      SELECT CASE strftime('%m', InvoiceDate)
                                               WHEN '01' THEN 'Enero'
                                               WHEN '02' THEN 'Febrero'
                                               WHEN '03' THEN 'Marzo'
                                               WHEN '04' THEN 'Abril'
                                               WHEN '05' THEN 'Mayo'
                                               WHEN '06' THEN 'Junio'
                                               WHEN '07' THEN 'Julio'
                                               WHEN '08' THEN 'Agosto'
                                               WHEN '09' THEN 'Septiembre'
                                               WHEN '10' THEN 'Octubre'
                                               WHEN '11' THEN 'Noviembre'
                                               WHEN '12' THEN 'Diciembre'
                                             END AS Mes,
                                             ROUND(AVG(Total), 2) AS Ingreso_Promedio
                                      FROM Invoice
                                      GROUP BY strftime('%m', InvoiceDate)
                                      ORDER BY Ingreso_Promedio DESC;
                                      ")

ingreso_promedio_por_mes

# Mes con mayor ingreso promedio
mes_mayor_ingreso_promedio <- ingreso_promedio_por_mes[1, ]
mes_mayor_ingreso_promedio

# Mes con menor ingreso promedio
mes_menor_ingreso_promedio <- ingreso_promedio_por_mes[nrow(ingreso_promedio_por_mes), ]
mes_menor_ingreso_promedio

# Cierre de conexión

dbDisconnect(con)


