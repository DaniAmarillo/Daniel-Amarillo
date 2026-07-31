# Construcción de la representación TF-IDF
# Taller 4 - Minería de Datos
# Paso 3: procesamiento del texto y representación vectorial

library(dplyr)
library(stringr)
library(text2vec)
library(stopwords)
library(Matrix)
library(cld3)

# 1. Rutas del proyecto

ruta_corpus <- file.path(
  "objetos_busqueda",
  "corpus_articulos.rds"
)

carpeta_objetos <- "objetos_busqueda"

if (!file.exists(ruta_corpus)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      ruta_corpus,
      ". Ejecute primero construir_corpus.R."
    )
  )
}

if (!dir.exists(carpeta_objetos)) {
  dir.create(carpeta_objetos)
}

# 2. Carga del corpus

corpus_articulos <- readRDS(
  ruta_corpus
)

cat("\n1. Corpus cargado correctamente\n")
cat("Documentos:", nrow(corpus_articulos), "\n")

columnas_necesarias <- c(
  "paper_id",
  "title_limpio",
  "abstract_limpio",
  "texto_original",
  "texto_procesado"
)

columnas_faltantes <- base::setdiff(
  columnas_necesarias,
  names(corpus_articulos)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste0(
      "Faltan las siguientes columnas en el corpus: ",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

# 3. Validación automática del idioma

cat("\n2. Detección automática del idioma\n")

corpus_articulos <- corpus_articulos %>%
  mutate(
    idioma_detectado = cld3::detect_language(
      texto_original
    ),
    idioma_detectado = ifelse(
      is.na(idioma_detectado) |
        idioma_detectado == "",
      "no_identificado",
      idioma_detectado
    )
  )

distribucion_idiomas <- corpus_articulos %>%
  count(
    idioma_detectado,
    sort = TRUE
  ) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    )
  )

print(distribucion_idiomas)

# 4. Stopwords

stopwords_ingles <- stopwords::stopwords(
  language = "en",
  source = "snowball"
)

stopwords_adicionales <- c(
  "article",
  "paper",
  "study",
  "research",
  "results",
  "result",
  "method",
  "methods",
  "using",
  "used",
  "use",
  "based",
  "proposed",
  "approach",
  "provide",
  "provides",
  "show",
  "shows",
  "also"
)

stopwords_finales <- unique(
  c(
    stopwords_ingles,
    stopwords_adicionales
  )
)

cat("\n3. Configuración de stopwords\n")
cat(
  "Stopwords utilizadas:",
  length(stopwords_finales),
  "\n"
)

# 5. Función de tokenización

tokenizar_textos <- function(textos) {
  tokens <- text2vec::word_tokenizer(
    textos
  )
  
  lapply(
    tokens,
    function(tokens_documento) {
      tokens_documento <- stringr::str_to_lower(
        tokens_documento
      )
      
      tokens_documento <- tokens_documento[
        stringr::str_length(tokens_documento) >= 2
      ]
      
      tokens_documento <- tokens_documento[
        !tokens_documento %in% stopwords_finales
      ]
      
      tokens_documento <- tokens_documento[
        !stringr::str_detect(
          tokens_documento,
          "^[0-9]+$"
        )
      ]
      
      tokens_documento <- tokens_documento[
        stringr::str_detect(
          tokens_documento,
          "[a-z]"
        )
      ]
      
      tokens_documento
    }
  )
}

# 6. Creación del iterador de documentos

ids_documentos <- as.character(
  corpus_articulos$paper_id
)

iterador_vocabulario <- text2vec::itoken(
  corpus_articulos$texto_procesado,
  ids = ids_documentos,
  tokenizer = tokenizar_textos,
  progressbar = FALSE
)

# 7. Construcción del vocabulario inicial

vocabulario_inicial <- text2vec::create_vocabulary(
  iterador_vocabulario,
  ngram = c(1L, 2L)
)

cat("\n4. Vocabulario inicial\n")
cat(
  "Términos iniciales:",
  nrow(vocabulario_inicial),
  "\n"
)

# 8. Depuración del vocabulario

vocabulario_final <- text2vec::prune_vocabulary(
  vocabulario_inicial,
  term_count_min = 2,
  doc_proportion_min = 0.002,
  doc_proportion_max = 0.95
)

if (nrow(vocabulario_final) == 0) {
  stop(
    "El vocabulario quedó vacío después de aplicar los filtros."
  )
}

cat("\n5. Vocabulario depurado\n")
cat(
  "Términos conservados:",
  nrow(vocabulario_final),
  "\n"
)

cat(
  "Términos eliminados:",
  nrow(vocabulario_inicial) -
    nrow(vocabulario_final),
  "\n"
)

# 9. Vectorizador

vectorizador <- text2vec::vocab_vectorizer(
  vocabulario_final
)

# 10. Matriz documento-término de frecuencias

iterador_dtm <- text2vec::itoken(
  corpus_articulos$texto_procesado,
  ids = ids_documentos,
  tokenizer = tokenizar_textos,
  progressbar = FALSE
)

matriz_frecuencias <- text2vec::create_dtm(
  iterador_dtm,
  vectorizador
)

cat("\n6. Matriz de frecuencias\n")
cat(
  "Filas:",
  nrow(matriz_frecuencias),
  "\n"
)
cat(
  "Columnas:",
  ncol(matriz_frecuencias),
  "\n"
)

# 11. Construcción del modelo TF-IDF

modelo_tfidf <- text2vec::TfIdf$new(
  norm = "l2",
  sublinear_tf = TRUE
)

matriz_tfidf <- modelo_tfidf$fit_transform(
  matriz_frecuencias
)

if (!inherits(matriz_tfidf, "sparseMatrix")) {
  matriz_tfidf <- methods::as(
    matriz_tfidf,
    "dgCMatrix"
  )
}

# 12. Validación de la matriz TF-IDF

if (nrow(matriz_tfidf) != nrow(corpus_articulos)) {
  stop(
    "La cantidad de filas de TF-IDF no coincide con la cantidad de artículos."
  )
}

if (ncol(matriz_tfidf) != nrow(vocabulario_final)) {
  stop(
    "La cantidad de columnas de TF-IDF no coincide con el vocabulario."
  )
}

valores_no_cero <- Matrix::nnzero(
  matriz_tfidf
)

total_celdas <- as.numeric(
  nrow(matriz_tfidf)
) * as.numeric(
  ncol(matriz_tfidf)
)

porcentaje_ceros <- round(
  (
    1 -
      valores_no_cero / total_celdas
  ) * 100,
  4
)

memoria_matriz_mb <- round(
  as.numeric(
    object.size(matriz_tfidf)
  ) / 1024^2,
  4
)

cat("\n7. Representación TF-IDF\n")
cat(
  "Dimensión:",
  nrow(matriz_tfidf),
  "x",
  ncol(matriz_tfidf),
  "\n"
)
cat(
  "Valores diferentes de cero:",
  valores_no_cero,
  "\n"
)
cat(
  "Porcentaje de ceros:",
  porcentaje_ceros,
  "%\n"
)
cat(
  "Memoria ocupada:",
  memoria_matriz_mb,
  "MB\n"
)
cat(
  "Tipo de matriz:",
  class(matriz_tfidf)[1],
  "\n"
)

# 13. Frecuencia documental de los términos

frecuencia_documental <- Matrix::colSums(
  matriz_frecuencias > 0
)

tabla_terminos <- data.frame(
  termino = names(frecuencia_documental),
  documentos = as.numeric(
    frecuencia_documental
  ),
  stringsAsFactors = FALSE
) %>%
  arrange(
    desc(documentos),
    termino
  )

cat("\n8. Términos presentes en más documentos\n")

print(
  tabla_terminos %>%
    slice_head(n = 20)
)

# 14. Términos con mayor peso TF-IDF promedio

peso_promedio_tfidf <- Matrix::colMeans(
  matriz_tfidf
)

tabla_pesos_tfidf <- data.frame(
  termino = names(peso_promedio_tfidf),
  peso_promedio = as.numeric(
    peso_promedio_tfidf
  ),
  stringsAsFactors = FALSE
) %>%
  arrange(
    desc(peso_promedio)
  )

cat("\n9. Términos con mayor peso TF-IDF promedio\n")

print(
  tabla_pesos_tfidf %>%
    slice_head(n = 20)
)

# 15. Resumen de la representación vectorial

resumen_tfidf <- data.frame(
  indicador = c(
    "Número de documentos",
    "Campos textuales",
    "Tipo de representación",
    "Tipo de recuperación",
    "Tokenización",
    "Stopwords",
    "N-gramas",
    "Frecuencia mínima",
    "Proporción documental mínima",
    "Proporción documental máxima",
    "Vocabulario inicial",
    "Vocabulario final",
    "Filas de la matriz",
    "Columnas de la matriz",
    "Valores no cero",
    "Porcentaje de ceros",
    "Memoria de la matriz en MB",
    "Formato de la matriz"
  ),
  valor = c(
    as.character(
      nrow(corpus_articulos)
    ),
    "Título y resumen",
    "TF-IDF",
    "Léxica",
    "Palabras",
    paste0(
      length(stopwords_finales),
      " términos en inglés"
    ),
    "Unigramas y bigramas",
    "2 apariciones en el corpus",
    "0.002",
    "0.95",
    as.character(
      nrow(vocabulario_inicial)
    ),
    as.character(
      nrow(vocabulario_final)
    ),
    as.character(
      nrow(matriz_tfidf)
    ),
    as.character(
      ncol(matriz_tfidf)
    ),
    as.character(
      valores_no_cero
    ),
    paste0(
      porcentaje_ceros,
      "%"
    ),
    as.character(
      memoria_matriz_mb
    ),
    class(matriz_tfidf)[1]
  ),
  stringsAsFactors = FALSE
)

cat("\n10. Resumen de la representación TF-IDF\n")
print(resumen_tfidf)

# 16. Configuración del procesamiento

configuracion_texto <- list(
  campos_textuales = c(
    "title",
    "abstract"
  ),
  idioma_stopwords = "en",
  stopwords = stopwords_finales,
  longitud_minima_token = 2L,
  eliminar_numeros_aislados = TRUE,
  conservar_terminos_alfanumericos = TRUE,
  ngram_min = 1L,
  ngram_max = 2L,
  term_count_min = 2L,
  doc_proportion_min = 0.002,
  doc_proportion_max = 0.95,
  normalizacion_tfidf = "l2",
  sublinear_tf = TRUE
)

# 17. Objeto completo del modelo léxico

objeto_tfidf <- list(
  corpus = corpus_articulos,
  vocabulario = vocabulario_final,
  vectorizador = vectorizador,
  modelo_tfidf = modelo_tfidf,
  matriz_frecuencias = matriz_frecuencias,
  matriz_tfidf = matriz_tfidf,
  configuracion = configuracion_texto,
  idiomas = distribucion_idiomas
)

# 18. Guardado de resultados

saveRDS(
  objeto_tfidf,
  file.path(
    carpeta_objetos,
    "modelo_tfidf_completo.rds"
  )
)

saveRDS(
  matriz_tfidf,
  file.path(
    carpeta_objetos,
    "matriz_tfidf.rds"
  )
)

saveRDS(
  vocabulario_final,
  file.path(
    carpeta_objetos,
    "vocabulario_tfidf.rds"
  )
)

saveRDS(
  configuracion_texto,
  file.path(
    carpeta_objetos,
    "configuracion_texto.rds"
  )
)

write.csv(
  distribucion_idiomas,
  "distribucion_idiomas.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  tabla_terminos,
  "frecuencia_documental_terminos.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  tabla_pesos_tfidf,
  "pesos_promedio_tfidf.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_tfidf,
  "resumen_representacion_tfidf.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n11. Construcción de TF-IDF finalizada correctamente\n")
cat("Objetos generados:\n")
cat("1. objetos_busqueda/modelo_tfidf_completo.rds\n")
cat("2. objetos_busqueda/matriz_tfidf.rds\n")
cat("3. objetos_busqueda/vocabulario_tfidf.rds\n")
cat("4. objetos_busqueda/configuracion_texto.rds\n")
cat("5. distribucion_idiomas.csv\n")
cat("6. frecuencia_documental_terminos.csv\n")
cat("7. pesos_promedio_tfidf.csv\n")
cat("8. resumen_representacion_tfidf.csv\n")