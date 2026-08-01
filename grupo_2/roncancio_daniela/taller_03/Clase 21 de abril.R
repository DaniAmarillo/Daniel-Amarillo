
# Sesion 4

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

## Ejercicio 1: Análisis descriptivo de la colección musical

#Con la base de datos Chinook, responde las siguientes preguntas usando 
#sentencias SQL.

# 1. ¿Cuántas pistas hay en total en la base de datos?

resultado1 <- dbGetQuery(con, "
  SELECT COUNT (*) As total_pistas
  FROM   Track
")

resultado1

# ¿Cuántos géneros distintos existen?

resultado2 <- dbGetQuery(con, "
  SELECT COUNT (*) As total_generos
  FROM Genre
")

resultado2

# 3. Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. 
# Ordena de mayor a menor número de pistas.

resultado3 <- dbGetQuery(con, "
  SELECT Genre.Name AS genero,
         COUNT(*) AS num_pistas,
         ROUND(AVG(Milliseconds) / 60000, 2) AS duracion_min
  FROM   Track
  JOIN   Genre ON Track.GenreId = Genre.GenreId
  GROUP BY Genre.Name
  ORDER BY num_pistas DESC;
")

resultado3

# 4. ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.

resultado4 <- dbGetQuery(con, "
  SELECT Track.Name AS nombre_pista,
         Album.Title AS album,
         ROUND(Milliseconds / 60000.0, 2) AS duracion_min
  FROM   Track
  JOIN   Album ON Track.AlbumId = Album.AlbumId
  ORDER BY Milliseconds DESC
  LIMIT 5;
")

resultado4

# ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?

# Pistas con precio mayor a $0.99

resultado5 <- dbGetQuery(con, "
  SELECT COUNT(*) AS num_pistas,
         ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM Track), 2) AS porcentaje
  FROM   Track
  WHERE  UnitPrice > 0.99;
")

resultado5

# Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos de todas las pistas.

resultado6 <- dbGetQuery(con, "
  SELECT COUNT(*) AS num_pistas,
         ROUND(AVG(Milliseconds), 2) AS duracion_media,
         MIN(Milliseconds) AS duracion_minima,
         MAX(Milliseconds) AS duracion_maxima,
         ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds)*AVG(Milliseconds), 2) AS varianza_pob
  FROM   Track
")

resultado6

# Ejericiio 2: Usando las tablas Invoice e InvoiceLine, responde:

# 1. ¿Cuál es el total de ingresos generados por la tienda? ¿Y el promedio por factura?
  
dbListFields(con, "Invoice")
dbListFields(con, "InvoiceLine")

resultado7 <- dbGetQuery(con,"
  SELECT ROUND(Sum(Total), 2) As Total_ingresos,
         ROUND(AVG(Total), 2) As promedio_factura
  FROM   Invoice
                         
")

resultado7

# 2. ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.

resultado8 <- dbGetQuery(con, "
  SELECT  InvoiceDate    AS fecha,
          COUNT(*)       AS totaL_facturas
  FROM    Invoice
  GROUP BY fecha
  ORDER BY fecha ASC
   LIMIT 20;
")

resultado8

#3.Identifica los 5 países con más ingresos. Muestra: país, número de facturas, ingreso total y promedio por factura.

resultado9 <- dbGetQuery(con, "
   SELECT BillingCountry AS pais,
         COUNT(*)       AS numero_facturas,
         ROUND(SUM(Total), 2) AS ingreso_total,
         ROUND(AVG(Total), 2) AS promedio_factura
   FROM  Invoice
   GROUP BY pais
   ORDER BY ingreso_total DESC
   LIMIT 5;
")

resultado9

# 4. ¿Cuál es la desviación estándar del total de facturas? Calcula primero la varianza en SQL y luego obtén la raíz en R o Python.

resultado10 <- dbGetQuery(con, "
  SELECT
        ROUND(AVG(Total * Total) - AVG(Total)*AVG(Total), 2) AS varianza_pob
  FROM Invoice;
")

resultado10

# Desviación estándar en R
sqrt(resultado10)


# 5. Encuentra los meses con mayor y menor ingreso promedio.

mayor <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 6, 2) AS mes,
         ROUND(AVG(Total), 2)      AS ingreso_promedio
  FROM   Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC
  LIMIT 5;
")

menor <- dbGetQuery(con, "
  SELECT SUBSTR(InvoiceDate, 6, 2) AS mes,
         ROUND(AVG(Total), 2)      AS ingreso_promedio
  FROM   Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio ASC
  LIMIT 5;
")

rbind(mayor, menor)


