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
dbListFields(con, "Genre")
dbListFields(con, "Album")

##########################################################
resultado <- dbGetQuery(con, "
                        SELECT Name           AS Pista
                        FROM Track")

resultadoTotal <- dbGetQuery(con, "
                        SELECT Count(Name)    AS Pistas
                        From Track")

cat("Número de pistas:", resultadoTotal$Pistas)

##########################################################

resultado <- dbGetQuery(con, "
                          SELECT Name AS genero
                          FROM   Genre
                          ORDER  BY Name; ")

resultado <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT GenreId)     AS generos_distintos
         FROM Genre
         ")
cat("Numero de generos distintos:", resultado$generos_distintos)

##########################################################

resultado <- dbGetQuery(con, "
                        SELECT Name AS genero,
                        (
                            SELECT COUNT(*)
                            FROM Track t
                            WHERE t.GenreId = g.GenreId
                        ) AS numero_pistas,
                        (
                            SELECT ROUND(AVG(Milliseconds) / 60000.0, 2)
                            FROM Track t
                            WHERE t.GenreId = g.GenreId
                        ) AS duracion_promedio_minutos
                       FROM Genre g
                       ORDER BY numero_pistas DESC;
                              ")

resultado

#########################################################

resultado <- dbGetQuery(con, "
                        SELECT 
                        Name AS Pista,
                        (SELECT Title FROM Album a WHERE a.AlbumId = t.AlbumId) AS Album,
                        ROUND(Milliseconds / 60000.0, 2) AS duracion_min
                        FROM Track t
                        ORDER BY duracion_min DESC
                        LIMIT 5;
                        "
                        )
#########################################################

resultado <- dbGetQuery(con, "
                        SELECT Name AS Pista,
                               UnitPrice AS Precio
                        FROM TRACK
                        WHERE UnitPrice > 0.99;
                        ")

resultadoUnitPrice <- dbGetQuery(con, "
                        SELECT Count(Name) AS Pista,
                               UnitPrice AS Precio
                        FROM TRACK
                        WHERE UnitPrice > 0.99;
                        ")
cat("Proporción Pistas con Precio > 0.99:", round((resultadoUnitPrice$Pista/resultadoTotal$Pistas)*100, 2),"%")

#########################################################

resultado <- dbGetQuery(con, "
                        SELECT ROUND(AVG(Milliseconds),2) AS Media_en_ms,
                               ROUND(MIN(Milliseconds),2) AS Duración_min_en_ms,
                               ROUND(MAX(Milliseconds),2) AS Duración_max_en_ms,
                               ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds)*AVG(Milliseconds), 2) AS Varianza
                        FROM Track")

cat("Media en ms:", resultado$Media_en_ms)
cat("Minimo en ms:", resultado$Duración_min_en_ms)
cat("Maximo en ms:", resultado$Duración_max_en_ms)
cat("Varianza de Duración:", resultado$Varianza)

########################################################
dbListFields(con, "Invoice")
dbListFields(con, "InvoiceLine")

resultado <- dbGetQuery(con, "
                        SELECT SUM(Total) AS Gran_Total,
                               AVG(Total) AS Promedio_Factura
                               FROM Invoice
                             ")

facturas_por_año <- dbGetQuery(con, "
                              SELECT SUBSTR(InvoiceDate, 1, 4) AS Año,
                                     COUNT(InvoiceId) AS Total_Facturas
                              FROM Invoice
                              GROUP BY Año
                              ORDER BY Año DESC;
")

facturas_por_año
########################################################

facturas_por_pais <- dbGetQuery(con,"
                                SELECT BillingCountry As Pais,
                                       Count(InvoiceId) AS  T_Facturas_Pais,
                                       SUM(Total) AS  Ingreso_T,
                                       ROUND(AVG(Total) ,2) AS Promedio_Facturas
                                FROM Invoice 
                                GROUP BY Pais
                                ORDER BY Ingreso_T DESC
                                LIMIT 5;
                                ")
facturas_por_pais
########################################################

desviación <- dbGetQuery(con, "
                         SELECT ROUND((AVG(Total * Total) - AVG(Total)*AVG(Total)), 2) AS Varianza
                         FROM Invoice
                         ")
cat("Desviación_T_Facturas:", sqrt(desviación$Varianza))

########################################################
dbListFields(con, "Invoice")

meses <- dbGetQuery(con, "
                    SELECT SUBSTR(InvoiceDate, 6, 2) AS Mes, 
                    ROUND(AVG(Total), 2) AS Promedio
                    FROM Invoice
                    GROUP BY Mes
                    ORDER BY Promedio DESC
                         ")
cat("Mes con menor ingreso promedio:", min(meses$Mes), "con $", min(meses$Promedio))
cat("Mes con mayor ingreso promedio:", max(meses$Mes), "con $", max(meses$Promedio))

#######################################################