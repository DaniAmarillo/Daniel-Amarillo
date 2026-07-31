normalize_text_ir <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = " ")
  x <- tolower(x)
  x <- gsub("https?://\\S+|doi:\\S+", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ir_stopwords <- c(
  "the","and","for","with","from","that","this","are","was","were","has","have","had",
  "into","its","their","than","then","such","using","used","use","based","between",
  "through","these","those","our","can","may","also","more","most","within","without",
  "study","paper","article","results","method","methods","model","models","approach",
  "analysis","data","system","systems","proposed","show","shows","new","different",
  "una","uno","unos","unas","para","con","del","los","las","por","que","como","sobre",
  "este","esta","estos","estas","entre","desde","hacia","tambien","puede","pueden"
)

tokenize_ir <- function(x) {
  x <- normalize_text_ir(x)
  tokens <- unlist(strsplit(x, "\\s+"), use.names = FALSE)
  tokens <- tokens[nchar(tokens) >= 3]
  tokens <- tokens[!tokens %in% ir_stopwords]
  tokens <- tokens[!grepl("^[0-9]+$", tokens)]
  tokens
}

build_search_index <- function(db_path, max_terms = 1800L, svd_k = 80L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  papers <- DBI::dbGetQuery(con, "SELECT * FROM papers")
  papers$paper_id <- as.integer(papers$paper_id)
  papers$title <- ifelse(is.na(papers$title), "", papers$title)
  papers$abstract <- ifelse(is.na(papers$abstract), "", papers$abstract)
  papers$authors_raw <- ifelse(is.na(papers$authors_raw), "", papers$authors_raw)
  papers$doi <- ifelse(is.na(papers$doi), "", papers$doi)
  papers$topic_label <- ifelse(is.na(papers$topic_label) | papers$topic_label == "", "Otros", papers$topic_label)

  corpus <- paste(papers$title, papers$title, papers$abstract, papers$topic_label)
  tokens_by_doc <- lapply(corpus, tokenize_ir)
  valid <- lengths(tokens_by_doc) > 0
  papers <- papers[valid, , drop = FALSE]
  tokens_by_doc <- tokens_by_doc[valid]

  n_docs <- length(tokens_by_doc)
  term_freq <- sort(table(unlist(tokens_by_doc, use.names = FALSE)), decreasing = TRUE)
  doc_freq <- sort(table(unlist(lapply(tokens_by_doc, unique), use.names = FALSE)), decreasing = TRUE)
  candidates <- names(term_freq)
  candidates <- candidates[doc_freq[candidates] >= 2 & doc_freq[candidates] <= ceiling(0.85 * n_docs)]
  vocab <- head(candidates, max_terms)

  term_id <- seq_along(vocab)
  names(term_id) <- vocab
  dtm <- matrix(0, nrow = n_docs, ncol = length(vocab), dimnames = list(NULL, vocab))
  for (i in seq_along(tokens_by_doc)) {
    ids <- term_id[tokens_by_doc[[i]]]
    ids <- ids[!is.na(ids)]
    if (length(ids) > 0) {
      counts <- table(ids)
      dtm[i, as.integer(names(counts))] <- as.numeric(counts)
    }
  }

  df <- colSums(dtm > 0)
  idf <- log((1 + n_docs) / (1 + df)) + 1
  tf <- dtm / pmax(rowSums(dtm), 1)
  tfidf <- sweep(tf, 2, idf, "*")
  tfidf_norm <- normalize_rows(tfidf)

  k <- min(as.integer(svd_k), nrow(tfidf) - 1L, ncol(tfidf) - 1L)
  k <- max(2L, k)
  sv <- svd(tfidf, nu = k, nv = k)
  lsa_docs <- sv$u[, seq_len(k), drop = FALSE] %*% diag(sv$d[seq_len(k)], nrow = k)
  lsa_docs_norm <- normalize_rows(lsa_docs)

  list(
    papers = papers,
    vocab = vocab,
    idf = idf,
    tfidf_norm = tfidf_norm,
    svd_v = sv$v[, seq_len(k), drop = FALSE],
    svd_d = sv$d[seq_len(k)],
    lsa_docs_norm = lsa_docs_norm,
    meta = list(
      n_docs = n_docs,
      original_dim = length(vocab),
      reduced_dim = k,
      max_terms = max_terms,
      min_df = 2,
      max_df_share = 0.85,
      method = "TF-IDF + Truncated SVD/LSA",
      built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
}

normalize_rows <- function(x) {
  norms <- sqrt(rowSums(x * x))
  norms[norms == 0] <- 1
  x / norms
}

vectorize_query <- function(query, index) {
  toks <- tokenize_ir(query)
  q <- numeric(length(index$vocab))
  names(q) <- index$vocab
  ids <- match(toks, index$vocab)
  ids <- ids[!is.na(ids)]
  if (length(ids) > 0) {
    counts <- table(ids)
    q[as.integer(names(counts))] <- as.numeric(counts)
  }
  q <- q / max(sum(q), 1)
  q * index$idf
}

query_lsa <- function(q, index) {
  dims <- seq_along(index$svd_d)
  q_lsa <- as.numeric(q %*% index$svd_v[, dims, drop = FALSE])
  q_lsa <- q_lsa / pmax(index$svd_d[dims], .Machine$double.eps)
  n <- sqrt(sum(q_lsa * q_lsa))
  if (n == 0) q_lsa else q_lsa / n
}

make_fragment <- function(text, query, width = 320L) {
  text <- gsub("\\s+", " ", ifelse(is.na(text), "", text))
  if (!nzchar(text)) return("Sin resumen disponible.")
  toks <- tokenize_ir(query)
  pos <- integer(0)
  for (tok in toks) {
    hit <- regexpr(tok, normalize_text_ir(text), fixed = TRUE)[1]
    if (!is.na(hit) && hit > 0) {
      pos <- hit
      break
    }
  }
  start <- if (length(pos) == 0) 1L else max(1L, pos - 80L)
  frag <- substr(text, start, start + width)
  if (start > 1L) frag <- paste0("...", frag)
  if (nchar(text) > start + width) frag <- paste0(frag, "...")
  frag
}

search_articles <- function(query, index, strategy = "tfidf", top_n = 10L) {
  query <- trimws(query %||% "")
  if (!nzchar(query)) return(data.frame())
  top_n <- as.integer(top_n)
  q <- vectorize_query(query, index)
  if (sum(abs(q)) == 0) return(data.frame())

  if (strategy == "lsa") {
    qn <- query_lsa(q, index)
    scores <- as.numeric(index$lsa_docs_norm %*% qn)
    label <- "LSA/SVD + coseno"
  } else {
    qn <- q / sqrt(sum(q * q))
    scores <- as.numeric(index$tfidf_norm %*% qn)
    label <- "TF-IDF + coseno"
  }

  ord <- order(scores, decreasing = TRUE, na.last = NA)
  ord <- ord[scores[ord] > 0]
  ord <- head(ord, top_n)
  if (length(ord) == 0) return(data.frame())

  out <- index$papers[ord, c("paper_id","title","authors_raw","publication_date","topic_label","doi","url","abstract"), drop = FALSE]
  out$rank <- seq_along(ord)
  out$strategy <- label
  out$score <- round(scores[ord], 4)
  out$fragment <- vapply(out$abstract, make_fragment, character(1), query = query)
  out[, c("strategy","rank","score","title","authors_raw","publication_date","topic_label","doi","url","fragment","paper_id")]
}

load_or_build_search_index <- function(db_path, index_path = "search_index.rds", force = FALSE) {
  stale <- !file.exists(index_path)
  if (!stale && file.exists(db_path)) {
    stale <- file.info(index_path)$mtime < file.info(db_path)$mtime
  }
  if (isTRUE(force) || stale) {
    idx <- build_search_index(db_path)
    saveRDS(idx, index_path)
    return(idx)
  }
  readRDS(index_path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
