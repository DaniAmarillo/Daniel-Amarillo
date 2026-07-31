
COLUMNAS_KEYWORDS_CANDIDATAS <- c(
  "palabras_clave", "palabras_claves", "keywords", "keywords_str", "kw"
)

detectar_columna_keywords <- function(df) {
  encontrada <- intersect(COLUMNAS_KEYWORDS_CANDIDATAS, names(df))
  if (length(encontrada) == 0) return(NA_character_)
  encontrada[1]
}

construir_corpus <- function(con) {
  df <- leer_papers(con)

  if (nrow(df) == 0) {
    return(list(
      corpus = tibble::tibble(
        doc_id = character(0), titulo = character(0),
        resumen = character(0), keywords = character(0), texto = character(0)
      ),
      stats = list(n_total = 0L, n_incluidos = 0L, n_sin_resumen = 0L,
                   n_sin_keywords = 0L, columna_keywords = NA_character_,
                   idiomas = character(0), anios = integer(0))
    ))
  }

  col_kw <- detectar_columna_keywords(df)

  df <- df %>%
    dplyr::mutate(
      titulo   = as.character(.data$titulo),
      resumen  = as.character(.data$resumen),
      keywords = if (!is.na(col_kw)) as.character(.data[[col_kw]]) else NA_character_
    )

  incluidos <- df %>%
    dplyr::filter(!is.na(.data$titulo), stringr::str_trim(.data$titulo) != "")

  n_sin_resumen  <- sum(is.na(incluidos$resumen)  | stringr::str_trim(incluidos$resumen)  == "")
  n_sin_keywords <- sum(is.na(incluidos$keywords) | stringr::str_trim(incluidos$keywords) == "")

  corpus <- incluidos %>%
    dplyr::mutate(
      doc_id = as.character(dplyr::row_number()),

      texto = purrr::pmap_chr(
        list(.data$titulo, .data$resumen, .data$keywords),
        function(t, r, k) {
          partes <- c(t, r, k)
          partes <- partes[!is.na(partes) & stringr::str_trim(partes) != ""]
          paste(partes, collapse = ". ")
        }
      )
    ) %>%
    dplyr::select(doc_id, titulo, resumen, keywords, texto, dplyr::everything())

  anios <- suppressWarnings(
    as.integer(stringr::str_extract(as.character(incluidos$fecha_publicacion), "\\d{4}"))
  )

  stats <- list(
    n_total          = nrow(df),
    n_incluidos      = nrow(incluidos),
    n_excluidos      = nrow(df) - nrow(incluidos),
    n_sin_resumen    = n_sin_resumen,
    n_sin_keywords   = n_sin_keywords,
    columna_keywords = col_kw,
    idiomas          = "en (resúmenes en inglés; interfaz en español)",
    anios            = sort(unique(anios[!is.na(anios)])),
    criterio_inclusion = "titulo no vacío y no NA; resumen/keywords no inventados si faltan"
  )

  list(corpus = corpus, stats = stats)
}

