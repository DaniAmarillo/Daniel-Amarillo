# ==================================================================================================
#                         CONSTRUCCIÓN DE LA BASE DE DATOS SQLITE - JSS
# ==================================================================================================
# Se construye una base de datos en SQLite a partir de las tablas generadas en el
# proceso de scrapping. Las tablas no tienen restricciones de integridad referencial.
#
#   vol          articles          authorship          authors
#   references
#
# Para correr este script es necesario haber corrido previamente scrapping.R o tener en el
# entorno las tablas: vol, articles, authors, authorship y references.
#
# Autor           : Michel Mendivenson Barragán Zabala
# Materia         : Minería de Datos
# ==================================================================================================

library(DBI)
library(RSQLite)
library(dplyr)

setwd(getSrcDirectory(function() {}))

cat('

------------ JSS Database Construction ------------
This process requires the following tables to be
available in the environment:

  * vol
  * articles
  * authors
  * authorship
  * references

Upon completion, a jss.sqlite file will be created
in the current working directory\n')

cat('* STATUS: Building ...\r')

con <- dbConnect(RSQLite::SQLite(), "JSS.sqlite")

# ------------------------------------- CREACIÓN DE TABLAS -----------------------------------------
# Se eliminan las tablas si ya existen para garantizar una inserción limpia en cada ejecución

dbExecute(con, "DROP TABLE IF EXISTS vol")
dbExecute(con, "
  CREATE TABLE vol (
    id     INTEGER,
    ord    INTEGER,
    year   INTEGER,
    url    TEXT
  )
")

# 'order' es una palabra reservada en SQL, por eso se renombra a 'ord'

dbExecute(con, "DROP TABLE IF EXISTS articles")
dbExecute(con, "
  CREATE TABLE articles (
    DOI                   TEXT,
    vol_id                INTEGER,
    title                 TEXT,
    issue                 INTEGER,
    url                   TEXT,
    abstract              TEXT,
    date                  TEXT,
    n_citations           INTEGER,
    n_references          INTEGER,
    influential_citations INTEGER,
    topic                 TEXT
  )
")

dbExecute(con, "DROP TABLE IF EXISTS authors")
dbExecute(con, "
  CREATE TABLE authors (
    ORCID  TEXT,
    author TEXT
  )
")

dbExecute(con, "DROP TABLE IF EXISTS authorship")
dbExecute(con, "
  CREATE TABLE authorship (
    ORCID TEXT,
    DOI   TEXT
  )
")

dbExecute(con, "DROP TABLE IF EXISTS ref")
dbExecute(con, "
  CREATE TABLE ref (
    DOI_origin    TEXT,
    DOI_reference TEXT,
    paperId       TEXT
  )
")

# --------------------------------- PREPARACIÓN DE LOS DATOS ---------------------------------------
# Antes de insertar se hacen los ajustes necesarios para que los datos sean coherentes
# con los nombres de columna definidos en cada tabla

vol_db <- vol |>
  rename(ord = order)

articles_db <- articles |>
  rename(
    vol_id                = vol.id,
    influential_citations = `influential citations`,
    n_references          = references,
    n_citations           = citations
  ) |>
  select(DOI, vol_id, title, issue, url, abstract, date,
         n_citations, n_references, influential_citations, topic)

authors_db <- authors |>
  filter(ORCID != "")

authorship_db <- authorship

references_db <- references |>
  rename(
    DOI_origin    = DOI.origin,
    DOI_reference = DOI.reference
  )

# --------------------------------------- INSERCIÓN ------------------------------------------------

dbAppendTable(con, "vol",        vol_db)
dbAppendTable(con, "articles",   articles_db)
dbAppendTable(con, "authors",    authors_db)
dbAppendTable(con, "authorship", authorship_db)
dbAppendTable(con, "ref",        references_db)

dbDisconnect(con)

cat('* STATUS: DONE         \n')

# EOF
