#### Taller 4 - Minería de Datos ####
#### Parte 3: Funciones de consulta (para usar dentro de app.R) ####

# 02_corpus_representacion.R y define funciones para transformar una consulta
# de texto libre y compararla contra el corpus con dos estrategias:
#   1) TF-IDF + similitud coseno            (recuperación léxica)
#   2) LSA / Truncated SVD + similitud coseno (recuperación en espacio reducido)

library(Matrix)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)

corpus      <- readRDS("cache/corpus.rds")
dtm_tfidf   <- readRDS("cache/dtm_tfidf.rds")
vocabulario <- readRDS("cache/vocabulario.rds")
idf         <- readRDS("cache/idf.rds")
lsa_svd     <- readRDS("cache/lsa_svd.rds")

# Vectores de documentos ya proyectados en el espacio LSA (docs x k),
# se calculan una sola vez aquí, no en cada búsqueda.
doc_vectores_lsa <- lsa_svd$u %*% diag(lsa_svd$d)
rownames(doc_vectores_lsa) <- lsa_svd$doi

#### Preprocesamiento de la consulta (idéntico al del corpus) ####
preprocesar_texto <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

tokenizar_stem <- function(texto) {
  texto <- preprocesar_texto(texto)
  tokens <- unlist(str_split(texto, "\\s+"))
  tokens <- tokens[!(tokens %in% stopwords("en"))]
  tokens <- tokens[nchar(tokens) >= 3]
  wordStem(tokens, language = "en")
}

#### Convierte una consulta en un vector TF-IDF alineado con el vocabulario del corpus ####
consulta_a_tfidf <- function(consulta) {
  tokens <- tokenizar_stem(consulta)
  tf <- table(tokens)
  
  vec <- numeric(length(vocabulario))
  names(vec) <- vocabulario
  
  terminos_comunes <- intersect(names(tf), vocabulario)
  # Términos de la consulta que no están en el vocabulario del corpus se
  # ignoran (no se puede calcular idf de un término nunca visto).
  vec[terminos_comunes] <- as.numeric(tf[terminos_comunes]) * idf[terminos_comunes]
  
  vec
}

#### Similitud coseno genérica ####
similitud_coseno <- function(a, b) {
  # a: vector; b: matriz (docs x dims), o vector
  if (is.matrix(b) || is(b, "Matrix")) {
    num <- as.numeric(b %*% a)
    norm_b <- sqrt(Matrix::rowSums(b^2))
    norm_a <- sqrt(sum(a^2))
    ifelse(norm_a == 0 | norm_b == 0, 0, num / (norm_b * norm_a))
  } else {
    sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  }
}

#### Estrategia 1: TF-IDF + coseno (léxica) ####
buscar_tfidf <- function(consulta, n_resultados = 10) {
  q_vec <- consulta_a_tfidf(consulta)
  
  sim <- similitud_coseno(q_vec, dtm_tfidf)
  names(sim) <- rownames(dtm_tfidf)
  
  ranking <- sort(sim, decreasing = TRUE)[seq_len(min(n_resultados, length(sim)))]
  
  data.frame(doi = names(ranking), puntaje = as.numeric(ranking)) |>
    left_join(corpus, by = "doi") |>
    mutate(posicion = row_number()) |>
    select(posicion, doi, titulo, topic_label, year, puntaje,texto_completo)
}

#### Estrategia 2: LSA / Truncated SVD + coseno (espacio reducido) ####
buscar_lsa <- function(consulta, n_resultados = 10) {
  q_tfidf <- consulta_a_tfidf(consulta)
  
  # Proyección (folding-in) de la consulta al espacio de k componentes:
  #   q_k = q^T %*% V %*% Sigma^-1
  V <- matrix(lsa_svd$v, ncol = length(lsa_svd$d))
  q_lsa <- as.numeric(q_tfidf %*% V) / lsa_svd$d
  
  sim <- similitud_coseno(q_lsa, doc_vectores_lsa)
  names(sim) <- rownames(doc_vectores_lsa)
  
  ranking <- sort(sim, decreasing = TRUE)[seq_len(min(n_resultados, length(sim)))]
  
  data.frame(doi = names(ranking), puntaje = as.numeric(ranking)) |>
    left_join(corpus, by = "doi") |>
    mutate(posicion = row_number()) |>
    select(posicion, doi, titulo, topic_label, year, puntaje,texto_completo)
}
