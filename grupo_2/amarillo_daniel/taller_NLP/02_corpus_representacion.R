#### Taller 4 - Minería de Datos ####
#### Parte 1: Construcción del corpus + Parte 2: Representación vectorial ####
#
# Salidas (todas en la carpeta ./cache):
#   - corpus.rds            data frame doi | titulo | topic_label | texto_completo
#   - dtm_tfidf.rds          matriz dispersa (docs x términos) ponderada TF-IDF
#   - vocabulario.rds        vector de términos (columnas de la DTM), en el mismo orden
#   - idf.rds                vector de idf por término (para poder ponderar la consulta igual)
#   - lsa_svd.rds            objeto irlba (u, d, v) -> LSA / Truncated SVD
#   - preprocesar_texto()    función reutilizable (se guarda como función fuente, ver 03_funciones_busqueda.R)

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(SnowballC)
library(Matrix)
library(irlba)

dir.create("cache", showWarnings = FALSE)

#### 1. Conexión y extracción desde SQLite ####

con <- dbConnect(RSQLite::SQLite(), "JBA_25_26.sqlite")

papers   <- dbReadTable(con, "papers")
abstract <- dbReadTable(con, "abstract")

dbDisconnect(con)

#### 2. Construcción del corpus ####
# Decisión documentada: el texto de cada artículo se construye concatenando
# título + las 4 secciones del abstract (Backround, Metodos, Resultados,
# Conclusion). No existe un campo de "palabras clave" en la base, por lo que
# NO se incluyen keywords en el texto indexado; topic_label se conserva
# únicamente como metadato para filtros, no se mete al texto libre porque no
# es una palabra clave del autor sino una categoría asignada por el pipeline
# de carga.

abstract <- abstract |>
  mutate(across(c(Backround, Metodos, Resultados, Conclusion),
                ~ replace_na(.x, "")))

colnames(papers)

corpus <- papers |>
  select(doi, titulo, year) |>
  left_join(abstract, by = "doi") |>
  mutate(
    across(c(Backround, Metodos, Resultados, Conclusion), ~ replace_na(.x, "")),
    texto_completo = str_squish(paste(titulo, Backround, Metodos, Resultados, Conclusion,resumen
                                      ))
  ) |>
  select(doi, titulo, topic_label, year, texto_completo)

#### 2.1 Reporte de cobertura (para documentar en el .Rmd/.qmd) ####

n_total       <- nrow(corpus)
n_sin_titulo  <- sum(is.na(papers$titulo) | trimws(papers$titulo) == "")
n_sin_abstract <- sum(corpus$texto_completo == "" |
                       str_squish(corpus$texto_completo) == str_squish(corpus$titulo))
n_vacios      <- sum(nchar(corpus$texto_completo) == 0)

cat("Cobertura del corpus\n")
cat("  Total de artículos:            ", n_total, "\n")
cat("  Artículos sin título:           ", n_sin_titulo, "\n")
cat("  Artículos sin ninguna sección de abstract:", n_sin_abstract, "\n")
cat("  Artículos con texto vacío (excluidos de facto de la DTM):", n_vacios, "\n")

# Criterio de exclusión: un artículo sin NINGÚN texto (ni título ni abstract)
# no puede representarse vectorialmente y se excluye del corpus indexado.
# No se inventa contenido para reemplazar textos faltantes (requisito 4.1).
corpus <- corpus |> filter(nchar(texto_completo) > 0)

saveRDS(corpus, "cache/corpus.rds")

preprocesar_texto <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

corpus_txt <- Corpus(VectorSource(preprocesar_texto(corpus$texto_completo)))
corpus_txt <- tm_map(corpus_txt, removeWords, stopwords("en"))
corpus_txt <- tm_map(corpus_txt, stemDocument, language = "en")
corpus_txt <- tm_map(corpus_txt, stripWhitespace)

#### 4. Representación vectorial: TF-IDF ####
# Se limita el vocabulario para controlar memoria (requisito sección 6):
#   - se descartan términos que aparecen en menos de 2 documentos (ruido/erratas)
#   - se descartan términos que aparecen en más del 90% de los documentos
#     (demasiado comunes para discriminar relevancia)

dtm <- DocumentTermMatrix(
  corpus_txt,
  control = list(
    bounds  = list(global = c(2, floor(0.9 * length(corpus_txt)))),
    weighting = weightTfIdf
  )
)

dtm_sparse <- Matrix::sparseMatrix(
  i = dtm$i, j = dtm$j, x = dtm$v,
  dims = c(dtm$nrow, dtm$ncol),
  dimnames = list(corpus$doi, colnames(dtm))
)

vocabulario <- colnames(dtm_sparse)

cat("\nRepresentación TF-IDF\n")
cat("  Documentos:            ", nrow(dtm_sparse), "\n")
cat("  Tamaño del vocabulario:", ncol(dtm_sparse), "\n")
cat("  Densidad de la matriz: ", round(100 * Matrix::nnzero(dtm_sparse) / length(dtm_sparse), 3), "%\n")

# idf por término: se recalcula aparte porque la consulta del usuario debe
# ponderarse con el MISMO idf del corpus (no con un idf propio de la consulta)
n_docs <- nrow(dtm_sparse)
df_termino <- Matrix::colSums(dtm_sparse > 0)
idf <- log(n_docs / df_termino)

saveRDS(dtm_sparse, "cache/dtm_tfidf.rds")
saveRDS(vocabulario, "cache/vocabulario.rds")
saveRDS(idf, "cache/idf.rds")

#### 5. Reducción dimensional: Truncated SVD (LSA) ####
# Método: Truncated SVD vía irlba, aplicado directamente sobre la matriz
# dispersa TF-IDF (no se densifica en ningún momento).
#
# Selección de k (número de componentes):
#   Se inspecciona la caída de varianza explicada por los primeros singulares
#   y se elige el k donde la varianza acumulada supera ~80%, con un tope
#   práctico para no perder la ventaja de dispersión/tiempo de cómputo.

k_max <- min(200, ncol(dtm_sparse) - 1, nrow(dtm_sparse) - 1)
svd_probe <- irlba(dtm_sparse, nv = k_max)

var_explicada <- svd_probe$d^2 / sum(svd_probe$d^2)
var_acum <- cumsum(var_explicada)
k_final <- which(var_acum >= 0.80)[1]
if (is.na(k_final)) k_final <- k_max

cat("\nReducción dimensional (LSA / Truncated SVD)\n")
cat("  Dimensión original (vocabulario):", ncol(dtm_sparse), "\n")
cat("  Dimensión reducida (k):          ", k_final, "\n")
cat("  Varianza acumulada retenida:      ", round(var_acum[k_final] * 100, 1), "%\n")

lsa_svd <- irlba(dtm_sparse, nv = k_final)
lsa_svd$dimnames_terminos <- vocabulario   # para poder alinear con una consulta nueva
lsa_svd$doi <- corpus$doi                  # para poder recuperar metadatos tras el ranking

saveRDS(lsa_svd, "cache/lsa_svd.rds")

cat("\nListo. Objetos guardados en ./cache para uso directo en app.R\n")

