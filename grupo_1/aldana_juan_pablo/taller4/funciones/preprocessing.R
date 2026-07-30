
STOPWORDS_EN <- tidytext::stop_words %>%
  dplyr::filter(.data$lexicon == "SMART") %>%
  dplyr::pull(.data$word)

normalizar_texto <- function(texto) {
  texto %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    stringr::str_replace_all("[\u2018\u2019\u201c\u201d]", "'") %>%
    stringr::str_squish()
}

tokenizar_unigramas <- function(corpus_df) {
  corpus_df %>%
    dplyr::mutate(texto = normalizar_texto(.data$texto)) %>%
    tidytext::unnest_tokens(output = "term", input = "texto", token = "words") %>%
    dplyr::filter(
      !.data$term %in% STOPWORDS_EN,
      !stringr::str_detect(.data$term, "^\\d+$"),
      stringr::str_length(.data$term) > 2
    ) %>%
    dplyr::mutate(term = SnowballC::wordStem(.data$term, language = "english"))
}

tokenizar_bigramas <- function(corpus_df) {
  corpus_df %>%
    dplyr::mutate(texto = normalizar_texto(.data$texto)) %>%
    tidytext::unnest_tokens(
      output = "bigrama", input = "texto", token = "ngrams", n = 2
    ) %>%
    tidyr::separate(.data$bigrama, into = c("w1", "w2"), sep = " ", remove = TRUE) %>%
    dplyr::filter(
      !.data$w1 %in% STOPWORDS_EN, !.data$w2 %in% STOPWORDS_EN,
      !stringr::str_detect(.data$w1, "^\\d+$"),
      !stringr::str_detect(.data$w2, "^\\d+$"),
      stringr::str_length(.data$w1) > 2, stringr::str_length(.data$w2) > 2
    ) %>%
    dplyr::mutate(
      w1 = SnowballC::wordStem(.data$w1, language = "english"),
      w2 = SnowballC::wordStem(.data$w2, language = "english"),
      term = paste(.data$w1, .data$w2, sep = "_")
    ) %>%
    dplyr::select(doc_id, term)
}

preprocesar_corpus <- function(corpus_df) {
  uni <- tokenizar_unigramas(corpus_df) %>% dplyr::select(doc_id, term)
  bi  <- tokenizar_bigramas(corpus_df)
  dplyr::bind_rows(uni, bi)
}

