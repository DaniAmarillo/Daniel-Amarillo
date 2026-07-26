library(bigrquery)
library(DBI)

# =============================================================================
# CREACIÓN DE BASE DE DATOS: Journal of Statistical Software en BigQuery
# Esquema relacional con las tablas generadas en el pipeline.
# =============================================================================

# Autenticación con cuenta de servicio
bq_auth(path = "keys/BigQuery.json")

# Crear dataset si no existe
bq_dataset_create(
  bq_dataset("jss-dashboard-498723", "JSS"),
  location = "US"
)

# Conectar a BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = "jss-dashboard-498723",
  dataset = "JSS",
  billing = "jss-dashboard-498723"
)

# Normalizar nombres de columnas
vol_db <- vol
names(vol_db)[names(vol_db) == 'order'] <- 'ord'

art_db <- art
names(art_db) <- names(art_db) |> stringr::str_replace_all('\\.', '_')

authors_db <- authors
names(authors_db) <- names(authors_db) |> stringr::str_replace_all('\\.', '_')

references_db <- references
names(references_db) <- names(references_db) |> stringr::str_replace_all('\\.', '_')

names(art.authors)    <- names(art.authors)    |> stringr::str_replace_all('\\.', '_')
names(art.references) <- names(art.references) |> stringr::str_replace_all('\\.', '_')

# Insertar tablas principales
dbWriteTable(con, 'volumes',            vol_db,        overwrite = TRUE)
dbWriteTable(con, 'articles',           art_db,        overwrite = TRUE)
dbWriteTable(con, 'authors',            authors_db,    overwrite = TRUE)
dbWriteTable(con, 'refs',               references_db, overwrite = TRUE)

# Insertar tablas de asociación
dbWriteTable(con, 'article_authors',    art.authors,    overwrite = TRUE)
dbWriteTable(con, 'article_references', art.references, overwrite = TRUE)

dbDisconnect(con)
cat('Database uploaded to BigQuery successfully.\n')
