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
dbListFields(con, "track")

##EJERCICIO 1.##

##1. ¿Cuántas pistas hay en total en la base de datos?##
dbGetQuery(con, "
           SELECT COUNT(TrackId) AS total_pistas
           FROM track")

##2. ¿Cuántos géneros distintos existen?##

dbGetQuery(con, "
           SELECT COUNT(DISTINCT GenreId) AS GENEROS_DISTINTOS
           FROM track")

##3.  Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. Ordena de mayor a menor número de pistas.##

dbGetQuery(con, "
           SELECT  g.Name        AS genero,
           COUNT(t.TrackId) AS n_pistas,
           AVG(t.Milliseconds/60000) AS duracion_media
           FROM track t
           JOIN  Genre  g ON t.GenreId = g.GenreId
           GROUP BY g.Name
           ORDER BY n_pistas DESC;"
           )
##4. ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.##
dbGetQuery(con, "
           SELECT  t.name        AS Titulo,
           a.Title as Album,
           t.Milliseconds/60000 AS duracion
           FROM track t
           JOIN  Album  a ON t.AlbumId = a.AlbumId
           ORDER BY duracion DESC
           LIMIT 5;"
           )
##5. ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?##
dbGetQuery(con, "
           SELECT  SUM(UnitPrice > 0.99)        AS Precio_mayor,
           COUNT(TrackId)/SUM(UnitPrice > 0.99) AS porcentaje
           FROM track;"
)
##Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos de todas las pistas.##
dbGetQuery(con, "
           SELECT  AVG(Milliseconds/60000)        AS Media,
           MIN(Milliseconds/60000) AS Minimo,
           MAX(Milliseconds/60000) AS Maximo,
           VARIANCE(Milliseconds/60000) AS Varianza
           FROM track;")

##EJERCICIO 2##
dbListTables(con)
dbListFields(con, "Invoice")
##1. ¿Cuál es el total de ingresos generados por la tienda? ¿Y el promedio por factura?##
dbGetQuery(con, "
           SELECT  SUM(Total) AS Total_ingresos,
                   AVG(Total) AS Ingresos_promedio
           FROM Invoice"
)
##2. ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.##


