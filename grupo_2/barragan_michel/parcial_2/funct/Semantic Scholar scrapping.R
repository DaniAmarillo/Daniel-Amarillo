library(httr2)
library(dplyr)

# =============================================================================
# Semantic.Papers.Batch()
# Consulta la API de Semantic Scholar para obtener métricas y autores de
# múltiples artículos en una sola solicitud. Divide automáticamente en
# lotes de 500.
# Retorna una lista con dos data.frames:
#   - $metrics : DOI, no.authors, no.references, no.citations e importance.ratio
#   - $authors : DOI y authorId
# =============================================================================
Semantic.Papers.Batch <- function(dois){
  
  # Eliminar duplicados y NAs
  dois    <- unique(dois[!is.na(dois)])
  
  # Dividir en lotes de 500 (máximo permitido por la API)
  batches <- split(dois, ceiling(seq_along(dois) / 500))
  
  results <- lapply(batches, \(batch){
    
    tryCatch({
      
      Sys.sleep(10)
      
      detail <- request("https://api.semanticscholar.org/graph/v1/paper/batch") |>
        req_url_query(fields = "externalIds,authors,referenceCount,citationCount,influentialCitationCount") |>
        req_body_json(list(ids = as.list(paste0("DOI:", batch)))) |>
        req_timeout(30) |>
        req_headers("User-Agent" = "mbarraganz@unal.edu.co") |>
        req_perform() |>
        resp_body_json()

      # Extraer métricas
      metrics <- bind_rows(lapply(detail, \(x){
        data.frame(
          DOI              = x$externalIds$DOI              %||% NA,
          no.authors       = length(x$authors),
          no.references    = x$referenceCount               %||% NA,
          no.citations     = x$citationCount                %||% NA,
          importance.ratio = if (!is.null(x$citationCount) && x$citationCount > 0)
            x$influentialCitationCount / x$citationCount
          else NA
        )
      }))
      
      # Extraer asociación DOI-autor
      authors <- bind_rows(lapply(detail, \(x){
        doi      <- x$externalIds$DOI %||% NA
        author_ids <- sapply(x$authors, \(a) a$authorId %||% NA)
        
        if (length(author_ids) == 0) return(NULL)
        
        data.frame(DOI = doi, authorId = author_ids)
      }))
      
      list(metrics = metrics, authors = authors)
      
    }, error = \(e){
      message(sprintf('\nError en batch: %s', conditionMessage(e)))
      NULL
    })
  })
  
  # Combinar resultados de todos los lotes
  return(list(
    metrics = bind_rows(lapply(results, \(x) x$metrics)),
    authors = bind_rows(lapply(results, \(x) x$authors))
  )
  )
}

# =============================================================================
# Semantic.AuthorInfo.Batch()
# Consulta la API de Semantic Scholar para obtener info de múltiples autores
# en una sola solicitud. Divide automáticamente en lotes de 500.
# Retorna un data.frame con: authorId, name, name.norm, no.papers,
# no.citations, ORCID y GoogleScholar ID.
# =============================================================================
Semantic.AuthorInfo.Batch <- function(ids){
  
  # Eliminar duplicados y NAs
  ids     <- unique(ids[!is.na(ids)])
  
  # Dividir en lotes de 500 (máximo permitido por la API)
  batches <- split(ids, ceiling(seq_along(ids) / 500))
  
  bind_rows(lapply(batches, \(batch){
    
    tryCatch({
      
      detail <- request("https://api.semanticscholar.org/graph/v1/author/batch") |>
        req_url_query(fields = "name,paperCount,citationCount,externalIds") |>
        req_body_json(list(ids = as.list(batch))) |>
        req_timeout(30) |>
        req_headers("User-Agent" = "mbarraganz@unal.edu.co") |>
        req_perform() |>
        resp_body_json()
      
      Sys.sleep(1)
      
      bind_rows(lapply(detail, \(x){
        data.frame(
          authorId      = x$authorId,
          name          = x$name,
          # name.norm     = iconv(x$name, from = 'UTF-8', to = 'ASCII//TRANSLIT') |>
          #   stringr::str_to_lower() |>
          #   stringr::str_squish(),
          GoogleScholar = x$externalIds$GoogleScholar %||% NA,
          ORCID         = x$externalIds$ORCID         %||% NA,
          no.papers     = x$paperCount                %||% NA,
          no.citations  = x$citationCount             %||% NA
        )
      }))
      
    }, error = \(e){
      message(sprintf('\nError en batch: %s', conditionMessage(e)))
      NULL
    })
  }))
}

# =============================================================================
# Semantic.Paper()
# Consulta la API de Semantic Scholar para obtener métricas y autores de
# un único artículo a partir de su DOI.
# Retorna una lista con:
#   - $metrics : DOI, no.authors, no.references, no.citations e importance.ratio
#   - $authors : DOI y authorId
# =============================================================================
Semantic.Paper <- function(doi){
  
  tryCatch({
    
    detail <- request(sprintf(
      "https://api.semanticscholar.org/graph/v1/paper/DOI:%s",
      URLencode(doi, reserved = TRUE)
    )) |>
      req_url_query(fields = "externalIds,authors,referenceCount,citationCount,influentialCitationCount") |>
      req_timeout(30) |>
      req_headers("User-Agent" = "mbarraganz@unal.edu.co") |>
      req_perform() |>
      resp_body_json()
    
    # Extraer métricas del artículo
    metrics <- data.frame(
      DOI              = detail$externalIds$DOI %||% doi,
      no.authors       = length(detail$authors),
      no.references    = detail$referenceCount %||% NA,
      no.citations     = detail$citationCount %||% NA,
      importance.ratio = if (!is.null(detail$citationCount) && detail$citationCount > 0)
        detail$influentialCitationCount / detail$citationCount
      else NA
    )
    
    # Extraer asociación DOI-autor
    author_ids <- sapply(detail$authors, \(a) a$authorId %||% NA)
    
    authors <- if (length(author_ids) == 0) {
      NULL
    } else {
      data.frame(
        DOI = detail$externalIds$DOI %||% doi,
        authorId = author_ids
      )
    }
    
    list(metrics = metrics, authors = authors)
    
  }, error = \(e){
    message(sprintf('\nError DOI %s: %s', doi, conditionMessage(e)))
    NULL
  })
}

# =============================================================================
# Semantic.AuthorInfo()
# Consulta la API de Semantic Scholar para obtener información de un único
# autor a partir de su authorId.
# Retorna un data.frame con: authorId, name, no.papers, no.citations,
# ORCID y GoogleScholar ID.
# =============================================================================
Semantic.AuthorInfo <- function(id){
  
  tryCatch({
    
    detail <- request(sprintf(
      "https://api.semanticscholar.org/graph/v1/author/%s",
      id
    )) |>
      req_url_query(fields = "name,paperCount,citationCount,externalIds") |>
      req_timeout(30) |>
      req_headers("User-Agent" = "mbarraganz@unal.edu.co") |>
      req_perform() |>
      resp_body_json()
    
    data.frame(
      authorId      = detail$authorId,
      name          = detail$name,
      GoogleScholar = detail$externalIds$GoogleScholar %||% NA,
      ORCID         = detail$externalIds$ORCID %||% NA,
      no.papers     = detail$paperCount %||% NA,
      no.citations  = detail$citationCount %||% NA
    )
    
  }, error = \(e){
    message(sprintf('\nError authorId %s: %s', id, conditionMessage(e)))
    NULL
  })
}
