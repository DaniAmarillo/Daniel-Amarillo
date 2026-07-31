library(openalexR)

# =============================================================================
# OA.Metrics()
# Consulta la API de OpenAlex para obtener métricas de impacto de un
# artículo dado su DOI.
# Retorna un data.frame con: DOI, no.authors, no.references y no.citations.
# Si el paper no está indexado retorna NAs.
# =============================================================================
OA.Metrics <- function(doi){
  
  tryCatch({
    
    response <- oa_fetch(entity = "works", doi = doi, verbose = FALSE)
    
    data.frame(
      DOI           = doi,
      no.authors    = nrow(response$authorships[[1]]),
      no.references = max(response$referenced_works_count) %||% NA,
      no.citations  = sum(response$cited_by_count)         %||% NA
    )
    
  }, error = \(e){
    message(sprintf('\nPaper no encontrado en OpenAlex: %s', doi))
    data.frame(DOI = doi, no.authors = NA, no.references = NA, no.citations = NA)
  })
}

# =============================================================================
# OA.ArticleReferences.Batch()
# Consulta la API de OpenAlex para obtener las referencias de múltiples
# artículos dado sus DOIs. Divide automáticamente en lotes de 50.
# Retorna un data.frame con: DOI y ref.url (URL de OpenAlex de cada
# referencia).
# =============================================================================
OA.ArticleReferences.Batch <- function(dois){
  
  # Eliminar duplicados y NAs
  dois    <- unique(dois[!is.na(dois)])
  
  # Dividir en lotes de 50 (límite de OpenAlex para filtros por DOI)
  batches <- split(dois, ceiling(seq_along(dois) / 50))
  
  bind_rows(lapply(batches, \(batch){
    
    tryCatch({
      
      response <- request("https://api.openalex.org/works") |>
        req_url_query(
          filter    = paste0("doi:", paste(batch, collapse = "|")),
          select    = "doi,referenced_works",
          per_page  = 50
        ) |>
        req_timeout(30) |>
        req_perform() |>
        resp_body_json()
      
      Sys.sleep(1)
      
      bind_rows(lapply(response$results, \(x){
        refs <- unlist(x$referenced_works)
        if (length(refs) == 0) return(NULL)
        data.frame(
          DOI     = x$doi,
          ref.url = refs
        )
      }))
      
    }, error = \(e){
      message(sprintf('\nError en batch: %s', conditionMessage(e)))
      NULL
    })
  }))
}


# =============================================================================
# OA.ReferenceInfo.Batch()
# Consulta la API de OpenAlex para obtener información de múltiples
# referencias dado sus URLs de OpenAlex. Divide automáticamente en lotes de 50.
# Retorna un data.frame con: ref.url, title, doi, year y cited_by_count.
# =============================================================================
OA.ReferenceInfo.Batch <- function(urls){
  
  # Eliminar duplicados y NAs
  urls    <- unique(urls[!is.na(urls)])
  
  # Extraer los IDs de OpenAlex (parte final de la URL)
  ids     <- basename(urls)
  
  # Dividir en lotes de 50
  batches <- split(ids, ceiling(seq_along(ids) / 50))
  
  bind_rows(lapply(batches, \(batch){
    
    tryCatch({
      
      response <- request("https://api.openalex.org/works") |>
        req_url_query(
          filter   = paste0("openalex_id:", paste(batch, collapse = "|")),
          select   = "id,display_name,doi,publication_year,cited_by_count",
          per_page = 50
        ) |>
        req_timeout(30) |>
        req_perform() |>
        resp_body_json()
      
      Sys.sleep(1)
      
      bind_rows(lapply(response$results, \(x){
        data.frame(
          ref.url  = x$id                   %||% NA,
          title    = x$display_name         %||% NA,
          doi      = x$doi                  %||% NA,
          year     = x$publication_year     %||% NA,
          cited_by = x$cited_by_count       %||% NA
        )
      }))
      
    }, error = \(e){
      message(sprintf('\nError en batch: %s', conditionMessage(e)))
      NULL
    })
  }))
}