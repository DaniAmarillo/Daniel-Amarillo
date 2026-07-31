
rankear <- function(scores, doc_ids, meta, top_n = 10) {
  tibble::tibble(doc_id = doc_ids, score = as.numeric(scores)) %>%
    dplyr::filter(!is.na(.data$score), .data$score > 0) %>%
    dplyr::arrange(dplyr::desc(.data$score), .data$doc_id) %>%
    dplyr::left_join(meta, by = "doc_id") %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(posicion = dplyr::row_number()) %>%
    dplyr::select(posicion, dplyr::everything())
}

