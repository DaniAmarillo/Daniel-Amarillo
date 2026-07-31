# search_functions.R


library(stringr)
library(Matrix)
library(dplyr)

#  Preprocesamiento de la consulta 
limpiar_texto_query <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z\\s]", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

tokenizar_query <- function(query, idx) {
  q <- limpiar_texto_query(query)
  tokens <- str_split(q, " ")[[1]]
  tokens <- tokens[nchar(tokens) >= 3]
  stopw <- tm::stopwords("english")
  tokens <- tokens[!tokens %in% stopw]
  
  tokens[tokens %in% idx$vocab]
}

# --- Estrategia 1: BM25 
search_bm25 <- function(query, idx, n = 10, k1 = 1.5, b = 0.75) {
  tokens <- tokenizar_query(query, idx)
  if (length(tokens) == 0) {
    return(list(ok = FALSE, msg = "Ningún término de la consulta aparece en el vocabulario.",
                results = NULL))
  }

  scores <- numeric(idx$n_docs)
  for (t in unique(tokens)) {
    tf   <- idx$dtm_raw[, t]
    idf  <- idx$idf_bm25[t]
    denom <- tf + k1 * (1 - b + b * idx$dl / idx$avgdl)
    contrib <- idf * (tf * (k1 + 1)) / denom
    contrib[tf == 0] <- 0
    scores <- scores + contrib
  }

  ord <- order(scores, decreasing = TRUE)
  ord <- ord[scores[ord] > 0]
  n_disp <- min(n, length(ord))
  ord <- ord[seq_len(n_disp)]

  list(
    ok = TRUE,
    tokens_usados = tokens,
    results = idx$metadatos[ord, ] %>% dplyr::mutate(score = round(scores[ord], 4),
                                                       posicion = dplyr::row_number())
  )
}

# --- Estrategia 2: LSA 
cosine_sim <- function(q, M) {
  num <- as.numeric(M %*% q)
  den <- sqrt(rowSums(M^2)) * sqrt(sum(q^2))
  den[den == 0] <- NA
  num / den
}

search_lsa <- function(query, idx, n = 10) {
  tokens <- tokenizar_query(query, idx)
  if (length(tokens) == 0) {
    return(list(ok = FALSE, msg = "Ningún término de la consulta aparece en el vocabulario.",
                results = NULL))
  }

  tf_q <- table(tokens)
  q_vec <- numeric(length(idx$vocab))
  names(q_vec) <- idx$vocab
  q_vec[names(tf_q)] <- as.numeric(tf_q) / length(tokens)  # tf normalizado
  q_vec <- q_vec * idx$idf_tfidf                            # tf-idf de la consulta

  q_latente <- as.numeric(q_vec %*% idx$V_k)   # MISMA proyección que los documentos

  scores <- cosine_sim(q_latente, idx$doc_vectors_lsa)
  scores[is.na(scores)] <- -1

  ord <- order(scores, decreasing = TRUE)
  n_disp <- min(n, sum(scores > -1))
  ord <- ord[seq_len(n_disp)]

  list(
    ok = TRUE,
    tokens_usados = tokens,
    results = idx$metadatos[ord, ] %>% dplyr::mutate(score = round(scores[ord], 4),
                                                       posicion = dplyr::row_number())
  )
}

#
buscar <- function(query, idx, metodo = c("bm25", "lsa", "ambas"), n = 10) {
  metodo <- match.arg(metodo)
  if (metodo == "bm25") return(list(bm25 = search_bm25(query, idx, n)))
  if (metodo == "lsa")  return(list(lsa  = search_lsa(query, idx, n)))
  list(bm25 = search_bm25(query, idx, n), lsa = search_lsa(query, idx, n))
}


precision_at_k <- function(relevantes, k = 5) {
  relevantes <- relevantes[seq_len(min(k, length(relevantes)))]
  sum(relevantes, na.rm = TRUE) / k
}
