update_BD <- function(vol.ids, art.dois, con){
  
  library(tictoc)
  library(DBI)
  library(dplyr)
  
  lapply(
    list('funct/JSS scrapping.R', 
         'funct/Semantic Scholar scrapping.R',
         'funct/OpenAlex scrapping.R', 
         'funct/Groq classification.R'), 
    source
  )
  
  tic()
  
  vol.ids <- vol.ids[-which.max(vol.ids)]
  # Información de volumenes nuevos 
  vol <- JSS.Vol()
  vol <- vol[!(vol$no %in% vol.ids),]
  
  if (nrow(vol) == 0){
    return(
      list(
        message = 'Not new volumes found',
        duration = toc(quiet=T)$callback_msg))
  } 
  
  # Ubicación nuevos artículos 
  art <- apply(vol[, c("no", "url")],
                          MARGIN = 1,
                          FUN = (\(x) {
                            resultado        <- JSS.Art(x["url"])
                            resultado$vol.no <- x["no"]
                            resultado
                          })) |> 
    do.call(what = rbind)
  
  # Información artículos nuevos
  art.info <- lapply(art$url,
                                FUN = (\(x){
                                  JSS.Art.Info(x)
                                })) |>
    do.call(what = rbind)
  art <- art |> left_join(art.info, by = 'url')
  art <- art[!(art$DOI %in% art.dois), ]
  
  # Cálculo de métricas
  semantic <- lapply(art$DOI, \(doi){
    Sys.sleep(1)
    Semantic.Paper(doi)
  })
  
  semantic <- Filter(Negate(is.null), semantic)
  
  art.metrics <- bind_rows(lapply(semantic, \(x) x$metrics))
  art.authors <- bind_rows(lapply(semantic, \(x) x$authors))
  
  art <- art |> left_join(art.metrics, by = "DOI")
  
  # Completando información no encontrada en Semantic Scholar
  doi.missing <- art$DOI[is.na(art$no.references)]
  if (length(doi.missing) > 0){
    
    oa.metrics <- lapply(doi.missing, OA.Metrics) |>
      do.call(what = rbind)
    
    # Reemplazar directamente las filas con NAs
    art[is.na(art$no.references), c('no.authors', 'no.references', 'no.citations')] <-
      oa.metrics[, c('no.authors', 'no.references', 'no.citations')]
    
  }
  
  # Info detallada de autores únicos desde Semantic Scholar.
  cat('Retrieving author info...\n')
  
  authors <- bind_rows(
    Filter(
      Negate(is.null),
      lapply(unique(art.authors$authorId), \(id){
        Sys.sleep(1)
        Semantic.AuthorInfo(id)
      })
    )
  )
  
  # Referencias de cada artículo desde OpenAlex
  art.references <- OA.ArticleReferences.Batch(art$DOI)
  if (nrow(art.references) > 0) {
    art.references[,"DOI"] <- art.references[,"DOI"] |> stringr::str_replace_all('https://doi.org/', '')
    # Info detallada de referencias únicas
    references <- OA.ReferenceInfo.Batch(art.references$ref.url)
    
  }
  
  # Clasificando los artículos
  art.class <- apply(art[, c("DOI", "abstract")],
                                MARGIN = 1,
                                FUN = (\(x){
                                  Sys.sleep(3)
                                  c('DOI'   = x["DOI"],
                                    'topic' = classify_abstract(x["abstract"]))
                                }))
  
  art.class <- as.data.frame(t(art.class))
  colnames(art.class) <- c('DOI', 'topic')
  art.class$topic[is.na(art.class$topic)] <- 'Other'
  
  art <- art |> left_join(art.class, by = 'DOI')
  
  
  # ===========================================================================
  # SUBIR INFORMACIÓN A LA BASE DE DATOS
  # ===========================================================================
  
  vol_db <- vol
  names(vol_db)[names(vol_db) == "order"] <- "ord"
  
  art_db <- art
  names(art_db) <- stringr::str_replace_all(names(art_db), "\\.", "_")
  
  authors_db <- authors
  names(authors_db) <- stringr::str_replace_all(names(authors_db), "\\.", "_")
  
  if (exists("references") && nrow(references) > 0) {
    references_db <- references
    names(references_db) <- stringr::str_replace_all(names(references_db), "\\.", "_")
  }
  
  names(art.authors) <- stringr::str_replace_all(names(art.authors), "\\.", "_")
  
  if (exists("art.references") && nrow(art.references) > 0) {
    names(art.references) <- stringr::str_replace_all(names(art.references), "\\.", "_")
  }
  
  DBI::dbBegin(con)
  
  tryCatch({
    
    # -------------------------------------------------------------------------
    # Volúmenes (solo insertar nuevos)
    # -------------------------------------------------------------------------
    DBI::dbWriteTable(con, "tmp_volumes", vol_db, overwrite = TRUE)
    
    DBI::dbExecute(con, "
      INSERT OR IGNORE INTO volumes
      SELECT * FROM tmp_volumes
    ")
    
    # -------------------------------------------------------------------------
    # Artículos (solo insertar nuevos)
    # -------------------------------------------------------------------------
    DBI::dbWriteTable(con, "tmp_articles", art_db, overwrite = TRUE)
    
    DBI::dbExecute(con, "
      INSERT OR IGNORE INTO articles
      SELECT * FROM tmp_articles
    ")
    
    # -------------------------------------------------------------------------
    # Autores (UPSERT)
    # -------------------------------------------------------------------------
    DBI::dbWriteTable(con, "tmp_authors", authors_db, overwrite = TRUE)
    
    DBI::dbExecute(con, "
      INSERT INTO authors
      SELECT * FROM tmp_authors
      ON CONFLICT(authorId) DO UPDATE SET
        name          = excluded.name,
        name_norm     = excluded.name_norm,
        GoogleScholar = excluded.GoogleScholar,
        ORCID         = excluded.ORCID,
        no_papers     = excluded.no_papers,
        no_citations  = excluded.no_citations
    ")
    
    # -------------------------------------------------------------------------
    # Referencias (UPSERT)
    # -------------------------------------------------------------------------
    if (exists("references_db") && nrow(references_db) > 0) {
      
      DBI::dbWriteTable(con, "tmp_refs", references_db, overwrite = TRUE)
      
      DBI::dbExecute(con, "
        INSERT INTO refs
        SELECT * FROM tmp_refs
        ON CONFLICT(ref_url) DO UPDATE SET
          title    = excluded.title,
          doi      = excluded.doi,
          year     = excluded.year,
          cited_by = excluded.cited_by
      ")
    }
    
    # -------------------------------------------------------------------------
    # Relaciones artículo-autor
    # -------------------------------------------------------------------------
    DBI::dbWriteTable(
      con,
      "tmp_article_authors",
      art.authors,
      overwrite = TRUE
    )
    
    DBI::dbExecute(con, "
      INSERT OR IGNORE INTO article_authors
      SELECT * FROM tmp_article_authors
    ")
    
    # -------------------------------------------------------------------------
    # Relaciones artículo-referencia
    # -------------------------------------------------------------------------
    if (exists("art.references") && nrow(art.references) > 0) {
      
      DBI::dbWriteTable(
        con,
        "tmp_article_references",
        art.references,
        overwrite = TRUE
      )
      
      DBI::dbExecute(con, "
        INSERT OR IGNORE INTO article_references
        SELECT * FROM tmp_article_references
      ")
    }
    
    # Limpieza
    DBI::dbRemoveTable(con, "tmp_volumes")
    DBI::dbRemoveTable(con, "tmp_articles")
    DBI::dbRemoveTable(con, "tmp_authors")
    DBI::dbRemoveTable(con, "tmp_article_authors")
    
    if ("tmp_refs" %in% DBI::dbListTables(con))
      DBI::dbRemoveTable(con, "tmp_refs")
    
    if ("tmp_article_references" %in% DBI::dbListTables(con))
      DBI::dbRemoveTable(con, "tmp_article_references")
    
    DBI::dbCommit(con)
    
  }, error = \(e){
    
    DBI::dbRollback(con)
    stop(e)
    
  })
  
  list(
    volumes_added  = nrow(vol_db),
    articles_added = nrow(art_db),
    authors_found  = nrow(authors_db),
    duration       = toc(quiet = TRUE)$callback_msg
  )
}
