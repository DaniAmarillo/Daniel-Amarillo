# Construcción y limpieza del corpus
# Taller 4 - Minería de Datos
# Paso 2: preparación de los textos para recuperación de información

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)

# 1. Rutas del proyecto

db_path <- "bdcc_2025.sqlite"
carpeta_objetos <- "objetos_busqueda"

if (!file.exists(db_path)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      db_path,
      ". Verifique que esté en la carpeta del proyecto."
    )
  )
}

if (!dir.exists(carpeta_objetos)) {
  dir.create(carpeta_objetos)
}

# 2. Conexión y lectura de la base

con_corpus <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = db_path
)

if (!DBI::dbIsValid(con_corpus)) {
  stop("No fue posible establecer una conexión válida con SQLite.")
}

papers <- DBI::dbReadTable(
  con_corpus,
  "papers"
)

DBI::dbDisconnect(con_corpus)

cat("\n1. Artículos cargados desde SQLite\n")
cat("Total:", nrow(papers), "\n")

# 3. Función para limpiar etiquetas y caracteres especiales

limpiar_texto_base <- function(texto) {
  texto <- ifelse(
    is.na(texto),
    "",
    as.character(texto)
  )
  
  texto %>%
    stringr::str_replace_all("<[^>]+>", " ") %>%
    stringr::str_replace_all("&nbsp;", " ") %>%
    stringr::str_replace_all("&amp;", " and ") %>%
    stringr::str_replace_all("&lt;", " ") %>%
    stringr::str_replace_all("&gt;", " ") %>%
    stringr::str_replace_all("&#?[A-Za-z0-9]+;", " ") %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish()
}

# 4. Limpieza independiente de título y resumen

corpus_articulos <- papers %>%
  mutate(
    title_original = as.character(title),
    abstract_original = as.character(abstract),
    
    title_limpio = limpiar_texto_base(title),
    abstract_limpio = limpiar_texto_base(abstract),
    
    publication_date = suppressWarnings(
      as.Date(publication_date)
    ),
    
    citations = suppressWarnings(
      as.numeric(citations)
    ),
    
    downloads = suppressWarnings(
      as.numeric(downloads)
    ),
    
    n_references = suppressWarnings(
      as.numeric(n_references)
    )
  )

# 5. Construcción del documento textual

corpus_articulos <- corpus_articulos %>%
  mutate(
    texto_original = stringr::str_squish(
      paste(
        title_limpio,
        abstract_limpio
      )
    )
  )

# 6. Normalización para procesamiento vectorial

normalizar_texto <- function(texto) {
  texto %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      "[^[:alnum:]\\- ]",
      " "
    ) %>%
    stringr::str_replace_all(
      "(?<![[:alnum:]])[0-9]+(?![[:alnum:]])",
      " "
    ) %>%
    stringr::str_squish()
}

corpus_articulos <- corpus_articulos %>%
  mutate(
    texto_procesado = normalizar_texto(
      texto_original
    )
  )

# 7. Validación del corpus

corpus_articulos <- corpus_articulos %>%
  mutate(
    caracteres_titulo = stringr::str_length(
      title_limpio
    ),
    
    caracteres_resumen = stringr::str_length(
      abstract_limpio
    ),
    
    caracteres_documento = stringr::str_length(
      texto_procesado
    ),
    
    palabras_documento = stringr::str_count(
      texto_procesado,
      "\\S+"
    )
  )

documentos_vacios <- corpus_articulos %>%
  filter(
    is.na(texto_procesado) |
      texto_procesado == ""
  )

cat("\n2. Validación del corpus\n")
cat("Documentos construidos:", nrow(corpus_articulos), "\n")
cat("Documentos vacíos:", nrow(documentos_vacios), "\n")

if (nrow(documentos_vacios) > 0) {
  stop(
    paste0(
      "Se encontraron ",
      nrow(documentos_vacios),
      " documentos vacíos después de la limpieza."
    )
  )
}

# 8. Estadísticas de longitud

resumen_longitud_corpus <- data.frame(
  estadistica = c(
    "Mínimo",
    "Primer cuartil",
    "Mediana",
    "Media",
    "Tercer cuartil",
    "Máximo"
  ),
  
  caracteres = c(
    min(corpus_articulos$caracteres_documento),
    unname(
      quantile(
        corpus_articulos$caracteres_documento,
        0.25
      )
    ),
    median(corpus_articulos$caracteres_documento),
    mean(corpus_articulos$caracteres_documento),
    unname(
      quantile(
        corpus_articulos$caracteres_documento,
        0.75
      )
    ),
    max(corpus_articulos$caracteres_documento)
  ),
  
  palabras = c(
    min(corpus_articulos$palabras_documento),
    unname(
      quantile(
        corpus_articulos$palabras_documento,
        0.25
      )
    ),
    median(corpus_articulos$palabras_documento),
    mean(corpus_articulos$palabras_documento),
    unname(
      quantile(
        corpus_articulos$palabras_documento,
        0.75
      )
    ),
    max(corpus_articulos$palabras_documento)
  )
)

cat("\n3. Longitud de los documentos\n")
print(resumen_longitud_corpus)

# 9. Revisión de posibles etiquetas residuales

etiquetas_residuales <- corpus_articulos %>%
  filter(
    stringr::str_detect(
      texto_original,
      "<[^>]+>"
    )
  )

cat("\n4. Etiquetas XML o HTML residuales\n")
cat("Documentos con etiquetas residuales:", nrow(etiquetas_residuales), "\n")

# 10. Conservación de metadatos necesarios para el ranking

metadata_articulos <- corpus_articulos %>%
  select(
    paper_id,
    title = title_original,
    title_limpio,
    authors_raw,
    publication_date,
    year,
    topic_label,
    doi,
    url,
    citations,
    downloads,
    n_references,
    abstract = abstract_original,
    abstract_limpio,
    texto_original,
    texto_procesado,
    caracteres_documento,
    palabras_documento
  )

# 11. Muestra del corpus limpio

cat("\n5. Ejemplo de textos procesados\n")

muestra_corpus <- metadata_articulos %>%
  transmute(
    paper_id,
    titulo = stringr::str_trunc(
      title_limpio,
      width = 80,
      ellipsis = "..."
    ),
    texto = stringr::str_trunc(
      texto_procesado,
      width = 250,
      ellipsis = "..."
    ),
    palabras = palabras_documento
  ) %>%
  slice_head(n = 5)

print(
  tibble::as_tibble(muestra_corpus),
  n = 5
)

# 12. Resumen metodológico del corpus

resumen_corpus <- data.frame(
  indicador = c(
    "Artículos originales",
    "Artículos incluidos",
    "Artículos excluidos",
    "Documentos vacíos",
    "Campos textuales utilizados",
    "Palabras clave disponibles",
    "Etiquetas residuales",
    "Idioma esperado del corpus"
  ),
  
  valor = c(
    as.character(nrow(papers)),
    as.character(nrow(metadata_articulos)),
    as.character(
      nrow(papers) - nrow(metadata_articulos)
    ),
    as.character(nrow(documentos_vacios)),
    "Título y resumen",
    "No disponibles en SQLite",
    as.character(nrow(etiquetas_residuales)),
    "Principalmente inglés; pendiente de validación automática"
  ),
  
  stringsAsFactors = FALSE
)

cat("\n6. Resumen metodológico\n")
print(resumen_corpus)

# 13. Guardado de objetos

saveRDS(
  metadata_articulos,
  file.path(
    carpeta_objetos,
    "corpus_articulos.rds"
  )
)

write.csv(
  resumen_corpus,
  "resumen_construccion_corpus.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_longitud_corpus,
  "resumen_longitud_corpus.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  muestra_corpus,
  "muestra_corpus_procesado.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n7. Construcción del corpus finalizada correctamente\n")
cat("Objetos generados:\n")
cat("1. objetos_busqueda/corpus_articulos.rds\n")
cat("2. resumen_construccion_corpus.csv\n")
cat("3. resumen_longitud_corpus.csv\n")
cat("4. muestra_corpus_procesado.csv\n")