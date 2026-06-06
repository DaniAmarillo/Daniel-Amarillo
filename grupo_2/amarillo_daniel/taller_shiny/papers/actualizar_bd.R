paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr","openalexR",
  "purrr", "tibble", "janitor", "readr", "knitr", "tidyr","RSQLite"
)

# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)

actualizar_tabla <- function(fecha = "2026-06-01", authors, paper_authors,
                             references, paper_references) {
  
  source_info    <- oa_fetch(entity = "sources", issn = "2063-5303")
  source_id_short <- gsub("https://openalex.org/", "", source_info$id[1])
  
  resp <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter   = paste0("primary_location.source.id:", source_id_short,
                        ",from_publication_date:", fecha),
      sort     = "publication_date:desc",
      per_page = 25
    ) |>
    req_perform()
  
  result <- resp_body_json(resp)
  message("Total works encontrados: ", result$meta$count)
  
  # ── papers_recientes ─────────────────────────────────────────────────────────
  papers_recientes <- map_dfr(result$results, function(w) {
    tibble(
      titulo            = pluck(w, "display_name",                          .default = NA_character_),
      doi               = pluck(w, "doi",                                   .default = NA_character_),
      issue             = pluck(w, "biblio", "issue",                       .default = NA_character_),
      journal_name      = pluck(w, "primary_location", "source", "display_name", .default = NA_character_),
      url               = pluck(w, "primary_location", "landing_page_url",  .default = NA_character_),
      fecha_publicacion = pluck(w, "publication_date",                      .default = NA_character_),
      nro_de_citas      = pluck(w, "cited_by_count",                        .default = NA_integer_),
      fwci              = pluck(w, "fwci",                                  .default = NA_real_),
      n_autores         = length(w$authorships),
      n_referencia      = pluck(w, "referenced_works_count",                .default = NA_integer_),
      topic_label       = pluck(w, "primary_topic", "display_name",         .default = NA_character_),
      year              = pluck(w, "publication_year",                      .default = NA_integer_)
    )
  })
  
  # ── abstracts ────────────────────────────────────────────────────────────────
  reconstruir_abstract <- function(inv_index) {
    if (is.null(inv_index) || length(inv_index) == 0) return(NA_character_)
    palabras <- unlist(lapply(names(inv_index), function(palabra) {
      posiciones <- unlist(inv_index[[palabra]])
      setNames(rep(palabra, length(posiciones)), posiciones)
    }))
    paste(palabras[order(as.integer(names(palabras)))], collapse = " ")
  }
  
  extraer_seccion <- function(texto, inicio, fin_vec) {
    if (is.na(texto)) return(NA_character_)
    patron_inicio <- paste0("(?i)", inicio, "\\s*:?\\s*")
    patron_fin    <- paste0("(?i)(", paste(fin_vec, collapse = "|"), ")\\s*:?\\s*")
    if (!grepl(patron_inicio, texto, perl = TRUE)) return(NA_character_)
    parte <- sub(paste0(".*", patron_inicio), "", texto, perl = TRUE)
    if (grepl(patron_fin, parte, perl = TRUE))
      parte <- sub(paste0(patron_fin, ".*"), "", parte, perl = TRUE)
    trimws(parte)
  }
  
  abstracts_tabla <- map_dfr(result$results, function(w) {
    abstract_completo <- reconstruir_abstract(w$abstract_inverted_index)
    tibble(
      doi        = pluck(w, "doi", .default = NA_character_),
      resumen    = abstract_completo,
      Background = extraer_seccion(abstract_completo, "Background|Introduction|Aims",
                                   c("Method", "Methods", "Results", "Conclusion", "Conclusions")),
      Metodos    = extraer_seccion(abstract_completo, "Method|Methods|Procedure",
                                   c("Results", "Findings", "Conclusion", "Conclusions")),
      Resultados = extraer_seccion(abstract_completo, "Results|Findings",
                                   c("Conclusion", "Conclusions", "Discussion")),
      Conclusion = extraer_seccion(abstract_completo, "Conclusion|Conclusions|Discussion", c("$")),
      topic_label = pluck(w, "primary_topic", "display_name", .default = NA_character_)
    )
  })
  
  # ── autores ──────────────────────────────────────────────────────────────────
  autores_nuevos_raw <- map_dfr(result$results, function(w) {
    map_dfr(w$authorships, function(a) {
      tibble(
        doi    = pluck(w, "doi",                        .default = NA_character_),
        nombre = pluck(a, "author", "display_name",     .default = NA_character_)
      )
    })
  })
  
  nombres_nuevos <- setdiff(autores_nuevos_raw$nombre, authors$autores)
  
  if (length(nombres_nuevos) > 0) {
    id_maximo  <- max(authors$id_autor)
    authors    <- bind_rows(authors, tibble(
      id_autor = seq(id_maximo + 1, id_maximo + length(nombres_nuevos)),
      autores  = nombres_nuevos
    ))
  }
  
  paper_authors <- bind_rows(
    paper_authors,
    autores_nuevos_raw |>
      left_join(authors, by = c("nombre" = "autores")) |>
      select(doi, id_autor)
  ) |> distinct()
  
  message("Autores totales: ", nrow(authors))
  message("Filas doi-autor: ", nrow(paper_authors))
  
  # ── referencias ──────────────────────────────────────────────────────────────
  refs_nuevas_raw <- map_dfr(result$results, function(w) {
    if (length(w$referenced_works) == 0) return(NULL)
    tibble(
      doi         = pluck(w, "doi", .default = NA_character_),
      openalex_id = unlist(w$referenced_works)
    )
  })
  
  ids_unicos <- unique(refs_nuevas_raw$openalex_id)
  message("Referencias únicas a consultar: ", length(ids_unicos))
  
  obtener_metadatos_refs <- function(ids) {
    ids_cortos <- gsub("https://openalex.org/", "", ids)
    lotes      <- split(ids_cortos, ceiling(seq_along(ids_cortos) / 50))
    
    map_dfr(lotes, function(lote) {
      Sys.sleep(0.3)
      
      tryCatch({
        resp_refs <- request("https://api.openalex.org/works") |>
          req_url_query(
            filter   = paste0("openalex_id:", paste(lote, collapse = "|")),
            per_page = 50,
            select   = "id,doi,display_name,publication_year,authorships,biblio,primary_location"
          ) |>
          req_perform()
        
        data_refs <- resp_body_json(resp_refs)
        
        map_dfr(data_refs$results, function(r) {
          
          # ── autores de la cita ── protegido contra authorships vacío o NULL
          raw_authors <- pluck(r, "authorships", .default = list())
          autores_cita <- if (length(raw_authors) == 0) {
            "Sin autor"
          } else {
            nms <- map_chr(head(raw_authors, 6), function(a)
              pluck(a, "author", "display_name", .default = "?"))
            if (length(raw_authors) > 6) c(nms, "et al.") else nms
          }
          
          # ── campos biblio ── nunca usar $ encadenado sobre posibles NULL
          vol   <- pluck(r, "biblio", "volume", .default = NULL)
          issue <- pluck(r, "biblio", "issue",  .default = NULL)
          doi_r <- pluck(r, "doi",              .default = NULL)
          fuente <- pluck(r, "primary_location", "source", "display_name", .default = "")
          
          tibble(
            openalex_id = pluck(r, "id", .default = NA_character_),
            referencia  = paste0(
              paste(autores_cita, collapse = ", "), " ",
              "(", pluck(r, "publication_year", .default = "s.f."), "). ",
              pluck(r, "display_name",          .default = "Sin título"), ". ",
              fuente,
              if (!is.null(vol))   paste0(", ", vol)        else "",
              if (!is.null(issue)) paste0("(", issue, ")")  else "",
              if (!is.null(doi_r)) paste0(". ", doi_r)      else ""
            )
          )
        })
        
      }, error = function(e) {
        message("ERROR en lote (primeros IDs): ", paste(head(lote, 3), collapse = ", "))
        message("Mensaje: ", conditionMessage(e))
        tibble(openalex_id = NA_character_, referencia = NA_character_)
      })
    })
  }
  
  metadatos_refs <- obtener_metadatos_refs(ids_unicos)
  
  nuevas_refs_df <- metadatos_refs |>
    filter(!is.na(referencia), !referencia %in% references$referencia)
  
  if (nrow(nuevas_refs_df) > 0) {
    id_maximo_ref  <- max(references$id_referencia)
    nuevas_refs_df <- nuevas_refs_df |>
      mutate(id_referencia = seq(id_maximo_ref + 1,
                                 id_maximo_ref + nrow(nuevas_refs_df))) |>
      select(id_referencia, referencia)
    references <- bind_rows(references, nuevas_refs_df)
  }
  
  paper_references <- bind_rows(
    paper_references,
    refs_nuevas_raw |>
      left_join(metadatos_refs, by = "openalex_id") |>
      left_join(references,     by = "referencia")  |>
      select(doi, id_referencia) |>
      filter(!is.na(id_referencia))
  ) |> distinct()
  
  # ── etiquetado tópicos ───────────────────────────────────────────────────────
  abstracts_tabla <- abstracts_tabla |>
    mutate(topic_label = case_when(
      if_any(-doi, ~ str_detect(replace_na(., ""),
                                "artificial intelligence| llm |gpt|nlp|agents|neuronal networks|chatbot|deep learning|transformers| ai ")) ~ "AI",
      if_any(-doi, ~ str_detect(replace_na(., ""),
                                "machine learning|clustering|knn|overfitting")) ~ "Machine Learning",
      if_any(-doi, ~ str_detect(replace_na(., ""),
                                "regression|inference|variance|covariance|statistic|sample|survey|random")) ~ "Statistic",
      TRUE ~ "Other"
    ))
  
  papers_recientes$topic_label <- abstracts_tabla$topic_label
  
  message("Referencias totales: ",    nrow(references))
  message("Filas doi-referencia: ",   nrow(paper_references))
  
  return(list(
    papers_recientes = papers_recientes,
    paper_references = paper_references,
    references       = references,
    abstracts_tabla  = abstracts_tabla,
    paper_authors    = paper_authors,
    authors          = authors
  ))
}
