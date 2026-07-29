# =============================================================================
# retrieval.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
# Tres estrategias de recuperación sobre el corpus JMLR:
#   E1  BM25            (léxica, matriz dispersa, sin reducción)
#   E2  LSA             (semántica latente: TF-IDF + Truncated SVD vía irlba)
#   E3  Híbrida RRF     (fusión de rankings de E1 y E2)
# =============================================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(irlba)
})


# =============================================================================
# E1 · BM25
# =============================================================================

#' Precalcula la matriz de pesos BM25.
#'
#' BM25 puntúa un documento d frente a una consulta q como
#'
#'   score(q,d) = SUM_{t in q}  qtf(t) * IDF(t) * ---------------------------
#'                                                 tf(t,d) * (k1 + 1)
#'                                                 --------------------------
#'                                                 tf(t,d) + k1*(1-b+b*|d|/L)
#'
#' El factor que depende de d NO depende de la consulta, así que se precalcula
#' una sola vez y la consulta se resuelve con un producto matriz-vector disperso.
#' Esto es lo que permite que la app no recalcule nada por búsqueda.
#'
#' IDF usa la variante suavizada de Lucene, log(1 + (N-df+0.5)/(df+0.5)), que
#' es siempre positiva y evita los pesos negativos del BM25 clásico para
#' términos presentes en más de la mitad del corpus.
construir_bm25 <- function(dtm, k1 = 1.2, b = 0.75) {
  N   <- nrow(dtm)
  dl  <- Matrix::rowSums(dtm)
  avgdl <- mean(dl)
  df  <- Matrix::colSums(dtm > 0)
  idf <- log(1 + (N - df + 0.5) / (df + 0.5))

  W <- as(dtm, "TsparseMatrix")           # triplete: i, j, x (0-indexado)
  tf <- W@x
  norm_d <- k1 * (1 - b + b * dl[W@i + 1L] / avgdl)
  W@x <- idf[W@j + 1L] * tf * (k1 + 1) / (tf + norm_d)

  list(
    W = as(W, "CsparseMatrix"), idf = idf, dl = dl, avgdl = avgdl,
    df = df, k1 = k1, b = b
  )
}

#' Puntajes BM25 de todos los documentos frente a una consulta vectorizada.
buscar_bm25 <- function(modelo, q_vec) {
  q <- as.numeric(q_vec)                  # sparseVector -> denso (|V| ~ 1e4)
  as.numeric(modelo$W %*% q)
}


# =============================================================================
# E2 · LSA  (TF-IDF + Truncated SVD)
# =============================================================================

#' TF-IDF con tf sublineal y normalización L2 por fila.
#'
#' tf sublineal (1 + log tf) porque en abstracts de ~180 palabras la
#' repetición de un término aporta evidencia decreciente. La normalización L2
#' hace que ||X||_F^2 = n_docs exactamente, lo que da una lectura limpia de la
#' "energía" retenida por el SVD truncado.
construir_tfidf <- function(dtm) {
  N  <- nrow(dtm)
  df <- Matrix::colSums(dtm > 0)
  idf <- log((1 + N) / (1 + df)) + 1

  X <- as(dtm, "TsparseMatrix")
  X@x <- (1 + log(X@x)) * idf[X@j + 1L]
  X <- as(X, "CsparseMatrix")

  nrm <- sqrt(Matrix::rowSums(X^2))
  nrm[nrm == 0] <- 1
  X <- Matrix::Diagonal(x = 1 / nrm) %*% X

  list(X = as(X, "CsparseMatrix"), idf = idf, df = df)
}

#' Truncated SVD sobre la matriz TF-IDF dispersa.
#'
#' X ~= U D V'  =>  U = X V D^{-1}
#'
#' Los documentos viven en las filas de U; una consulta nueva se proyecta con
#' la misma fórmula (fold-in de Deerwester et al., 1990):
#'
#'   q_k = q' V D^{-1}
#'
#' irlba calcula solo los k primeros vectores singulares sin densificar X,
#' que es justo la recomendación del enunciado para matrices dispersas de texto.
construir_lsa <- function(X, k = 150L, semilla = 2026L) {
  set.seed(semilla)
  t0 <- Sys.time()
  sv <- irlba::irlba(X, nv = k, nu = k)
  t_fit <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  energia_total <- sum(X@x^2)                      # = n_docs si X esta L2-norm
  energia_ret   <- cumsum(sv$d^2) / energia_total

  U <- sv$u
  nrm <- sqrt(rowSums(U^2)); nrm[nrm == 0] <- 1
  U_norm <- U / nrm                                # filas unitarias -> coseno

  list(
    U = U, U_norm = U_norm, d = sv$d, V = sv$v, k = k,
    energia_retenida = energia_ret, energia_total = energia_total,
    tiempo_ajuste_s = t_fit
  )
}

#' Proyecta una consulta al espacio latente y devuelve similitud coseno.
buscar_lsa <- function(lsa, tfidf, q_vec) {
  q <- as.numeric(q_vec)
  if (sum(q) == 0) return(rep(0, nrow(lsa$U)))
  q <- (1 + log(pmax(q, 1e-12))) * (q > 0) * tfidf$idf   # mismo esquema tf-idf
  n <- sqrt(sum(q^2)); if (n > 0) q <- q / n

  q_k <- as.numeric(crossprod(lsa$V, q) / lsa$d)         # q' V D^-1
  nq <- sqrt(sum(q_k^2))
  if (nq == 0) return(rep(0, nrow(lsa$U)))
  as.numeric(lsa$U_norm %*% (q_k / nq))
}


# =============================================================================
# E3 · Híbrida (Reciprocal Rank Fusion)
# =============================================================================

#' Fusión por rangos recíprocos.
#'
#' Los puntajes de BM25 (no acotado, >= 0) y de LSA (coseno en [-1,1]) están en
#' escalas incomparables, así que la fusión se hace sobre RANGOS, no sobre
#' puntajes:  RRF(d) = SUM_i 1 / (K + rank_i(d)).  K=60 es el valor estándar
#' (Cormack et al., 2009); amortigua el peso de las primeras posiciones.
fusion_rrf <- function(lista_scores, K = 60) {
  n <- length(lista_scores[[1]])
  acc <- numeric(n)
  for (s in lista_scores) {
    r <- rank(-s, ties.method = "min")
    acc <- acc + 1 / (K + r)
  }
  acc
}


# =============================================================================
# Ranking y presentación
# =============================================================================

#' Ordena y corta el top-n aplicando desempate determinista.
#'
#' Empates: se resuelven por año descendente y, si persisten, por paper_num
#' ascendente (orden de publicación en el volumen). Nunca se deja el desempate
#' al orden arbitrario de la tabla: eso haría el ranking no reproducible.
rankear <- function(scores, meta, n = 10L, umbral = 1e-12) {
  ord <- order(-scores, -meta$year, meta$paper_num)
  ord <- ord[scores[ord] > umbral]
  if (length(ord) == 0) return(integer(0))
  utils::head(ord, n)
}

#' Extrae el fragmento del abstract con mayor densidad de términos de consulta.
extraer_fragmento <- function(abstract, q_stems, max_chars = 320L) {
  if (is.na(abstract) || !nzchar(abstract)) return("")
  fr <- unlist(strsplit(abstract, "(?<=[.!?])\\s+", perl = TRUE))
  if (length(fr) == 0) return(substr(abstract, 1, max_chars))
  punt <- vapply(fr, function(s) {
    tk <- tokenizar(normalizar_texto(s))
    if (!length(tk)) return(0)
    sum(tk %in% q_stems)
  }, numeric(1))
  i <- which.max(punt)
  if (punt[i] == 0) i <- 1L
  txt <- fr[i]
  if (i < length(fr) && nchar(txt) < max_chars * 0.6)
    txt <- paste(txt, fr[i + 1L])
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars), "...")
  trimws(txt)
}

#' Ejecuta una estrategia y devuelve el data.frame de resultados listo para UI.
#'
#' @param estrategia "bm25" | "lsa" | "hibrido"
ejecutar_busqueda <- function(idx, consulta, estrategia = "bm25", n = 10L) {
  qv <- vectorizar_consulta(consulta, idx$vocab)
  q_stems <- setdiff(tokenizar(normalizar_texto(consulta)), character(0))

  # Cada puntaje se calcula solo si la estrategia lo necesita: así el tiempo
  # de respuesta medido corresponde realmente a la estrategia seleccionada.
  scores <- switch(
    estrategia,
    bm25    = buscar_bm25(idx$bm25, qv$vec),
    lsa     = buscar_lsa(idx$lsa, idx$tfidf, qv$vec),
    hibrido = fusion_rrf(list(buscar_bm25(idx$bm25, qv$vec),
                              buscar_lsa(idx$lsa, idx$tfidf, qv$vec))),
    stop("Estrategia desconocida: ", estrategia)
  )

  pos <- rankear(scores, idx$meta, n = n)
  if (length(pos) == 0)
    return(list(resultados = idx$meta[0, ], info = qv, scores = scores))

  res <- idx$meta[pos, , drop = FALSE]
  res$puntaje <- scores[pos]
  res$posicion <- seq_along(pos)
  res$fragmento <- vapply(res$abstract, extraer_fragmento, character(1),
                          q_stems = q_stems, USE.NAMES = FALSE)
  rownames(res) <- NULL
  list(resultados = res, info = qv, scores = scores)
}
