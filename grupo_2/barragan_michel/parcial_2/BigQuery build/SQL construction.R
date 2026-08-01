library(DBI)
library(RSQLite)

# =============================================================================
# CREACIÓN DE BASE DE DATOS: Journal of Statistical Software
# Esquema relacional en SQLite con las tablas generadas en el pipeline.
# =============================================================================

con <- dbConnect(RSQLite::SQLite(), 'data/JSS no updated.sqlite')

# Tablas principales
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS volumes (
    no    INTEGER PRIMARY KEY,
    year  INTEGER,
    ord   INTEGER,
    url   TEXT
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS articles (
    DOI           TEXT PRIMARY KEY,
    title         TEXT,
    issue         TEXT,
    url           TEXT,
    vol_no        INTEGER,
    abstract      TEXT,
    date          TEXT,
    no_authors    INTEGER,
    no_references INTEGER,
    no_citations  INTEGER,
    importance_ratio     REAL,
    topic         TEXT,
    FOREIGN KEY (vol_no) REFERENCES volumes(no)
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS authors (
    authorId      TEXT PRIMARY KEY,
    name          TEXT,
    name_norm     TEXT,
    GoogleScholar TEXT,
    ORCID         TEXT,
    no_papers     INTEGER,
    no_citations  INTEGER
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS refs (
    ref_url  TEXT PRIMARY KEY,
    title    TEXT,
    doi      TEXT,
    year     INTEGER,
    cited_by INTEGER
  )
")

# Tablas de asociación
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS article_authors (
    DOI      TEXT,
    authorId TEXT,
    PRIMARY KEY (DOI, authorId),
    FOREIGN KEY (DOI)      REFERENCES articles(DOI),
    FOREIGN KEY (authorId) REFERENCES authors(authorId)
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS article_references (
    DOI     TEXT,
    ref_url TEXT,
    PRIMARY KEY (DOI, ref_url),
    FOREIGN KEY (DOI)     REFERENCES articles(DOI),
    FOREIGN KEY (ref_url) REFERENCES refs(ref_url)
  )
")

vol_db <- vol
names(vol_db)[names(vol_db) == 'order'] <- 'ord'

art_db <- art
names(art_db) <- names(art_db) |> stringr::str_replace_all('\\.', '_')

authors_db <- authors
names(authors_db) <- names(authors_db) |> stringr::str_replace_all('\\.', '_')

references_db <- references
names(references_db) <- names(references_db) |> stringr::str_replace_all('\\.', '_')

names(art.references) <- names(art.references) |> stringr::str_replace_all('\\.', '_')

# Insertar datos
dbWriteTable(con, 'volumes',            vol_db,            append = TRUE)
dbWriteTable(con, 'articles',           art_db,            append = TRUE)
dbWriteTable(con, 'authors',            authors_db,        append = TRUE)
dbWriteTable(con, 'refs',               references_db,     append = TRUE)
dbWriteTable(con, 'article_authors',    art.authors,    append = TRUE)
dbWriteTable(con, 'article_references', art.references, append = TRUE)

dbDisconnect(con)
