get_conn <- function(DB_PATH){
    dbConnect(RSQLite::SQLite(), DB_PATH)
}

load_papers <- function(DB_PATH) {
  conn <- get_conn(DB_PATH)
  on.exit(dbDisconnect(conn))
  tryCatch(
    dbReadTable(conn, "papers"),
    error = function(e) {
      # tabla vacía de ejemplo si no existe aún
      data.frame(
        paper_id = character(), journal_name = character(), title = character(),
        publication_date = character(), year = integer(), doi = character(),
        url = character(), abstract = character(), authors = character(),
        n_authors = integer(), citations = integer(), downloads = integer(),
        references = character(), n_references = integer(), topic_label = character(),
        stringsAsFactors = FALSE
      )
    }
  )
}

load_updated_links <- function(DB_PATH){
  conn <- get_conn(DB_PATH)
  on.exit(dbDisconnect(conn))
  tryCatch(
    {
      notificacion <- dbReadTable(conn, "notification")
      respuesta <- notificacion$agregados
      respuesta
    },
    error = function(e) {
      0
    }
  )
}