# Preparación del buscador de artículos
# Taller 4 - Minería de Datos
# Paso 1: inspección de la base y diagnóstico del corpus

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)

# 1. Ruta de la base de datos

db_path <- "bdcc_2025.sqlite"

if (!file.exists(db_path)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      db_path,
      ". Verifique que esté en la misma carpeta del proyecto."
    )
  )
}

# 2. Conexión con SQLite

con_diagnostico <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = db_path
)

if (!DBI::dbIsValid(con_diagnostico)) {
  stop("No fue posible establecer una conexión válida con SQLite.")
}

cat("\n1. Diagnóstico inicial de la base SQLite\n")

# 3. Tablas disponibles

tablas <- DBI::dbListTables(con_diagnostico)

cat("\n2. Tablas disponibles\n")
print(tablas)

if (!"papers" %in% tablas) {
  DBI::dbDisconnect(con_diagnostico)
  stop("La base SQLite no contiene una tabla llamada 'papers'.")
}

# 4. Carga de la tabla papers

papers <- DBI::dbReadTable(
  con_diagnostico,
  "papers"
)

cat("\n3. Dimensión de la tabla papers\n")
cat("Filas:", nrow(papers), "\n")
cat("Columnas:", ncol(papers), "\n")

cat("\n4. Nombres de las columnas\n")
print(names(papers))

cat("\n5. Estructura de la tabla\n")
str(papers)

# 5. Función para contar textos faltantes

contar_faltantes_texto <- function(data, columna) {
  if (!columna %in% names(data)) {
    return(NA_integer_)
  }
  
  valores <- as.character(data[[columna]])
  
  sum(
    is.na(valores) |
      stringr::str_trim(valores) == ""
  )
}

# 6. Identificación de columnas relevantes

columnas_candidatas <- c(
  "title",
  "abstract",
  "keywords",
  "keyword",
  "authors_raw",
  "publication_date",
  "year",
  "topic_label",
  "doi",
  "citations",
  "downloads",
  "n_references"
)

columnas_disponibles <- base::intersect(
  columnas_candidatas,
  names(papers)
)

columnas_no_disponibles <- base::setdiff(
  columnas_candidatas,
  names(papers)
)

cat("\n6. Columnas relevantes disponibles\n")
print(columnas_disponibles)

cat("\n7. Columnas candidatas no disponibles\n")
print(columnas_no_disponibles)

# 7. Resumen de valores faltantes

resumen_faltantes <- data.frame(
  campo = c(
    "title",
    "abstract",
    "keywords",
    "keyword",
    "authors_raw",
    "publication_date",
    "topic_label",
    "doi"
  ),
  faltantes = c(
    contar_faltantes_texto(papers, "title"),
    contar_faltantes_texto(papers, "abstract"),
    contar_faltantes_texto(papers, "keywords"),
    contar_faltantes_texto(papers, "keyword"),
    contar_faltantes_texto(papers, "authors_raw"),
    contar_faltantes_texto(papers, "publication_date"),
    contar_faltantes_texto(papers, "topic_label"),
    contar_faltantes_texto(papers, "doi")
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(
    porcentaje = ifelse(
      is.na(faltantes),
      NA_real_,
      round(faltantes / nrow(papers) * 100, 2)
    )
  )

cat("\n8. Resumen de valores faltantes\n")
print(resumen_faltantes)

# 8. Artículos sin título ni resumen

if ("title" %in% names(papers)) {
  titulo_vacio <- is.na(papers$title) |
    stringr::str_trim(as.character(papers$title)) == ""
} else {
  titulo_vacio <- rep(TRUE, nrow(papers))
}

if ("abstract" %in% names(papers)) {
  resumen_vacio <- is.na(papers$abstract) |
    stringr::str_trim(as.character(papers$abstract)) == ""
} else {
  resumen_vacio <- rep(TRUE, nrow(papers))
}

sin_titulo_ni_resumen <- sum(
  titulo_vacio & resumen_vacio
)

cat("\n9. Artículos sin título y sin resumen\n")
print(sin_titulo_ni_resumen)

# 9. Control de DOI

cantidad_doi_no_vacios <- NA_integer_
cantidad_doi_unicos <- NA_integer_
cantidad_doi_duplicados <- NA_integer_

if ("doi" %in% names(papers)) {
  doi_limpio <- papers %>%
    transmute(
      doi_limpio = stringr::str_to_lower(
        stringr::str_trim(as.character(doi))
      )
    ) %>%
    filter(
      !is.na(doi_limpio),
      doi_limpio != ""
    )
  
  cantidad_doi_no_vacios <- nrow(doi_limpio)
  
  cantidad_doi_unicos <- dplyr::n_distinct(
    doi_limpio$doi_limpio
  )
  
  cantidad_doi_duplicados <- cantidad_doi_no_vacios -
    cantidad_doi_unicos
  
  cat("\n10. Control de DOI\n")
  cat("DOI no vacíos:", cantidad_doi_no_vacios, "\n")
  cat("DOI únicos:", cantidad_doi_unicos, "\n")
  cat("DOI duplicados:", cantidad_doi_duplicados, "\n")
} else {
  cat("\n10. Control de DOI\n")
  cat("La tabla no contiene la columna doi.\n")
}

# 10. Distribución por categoría

if ("topic_label" %in% names(papers)) {
  distribucion_temas <- papers %>%
    mutate(
      topic_label = ifelse(
        is.na(topic_label) |
          stringr::str_trim(as.character(topic_label)) == "",
        "Sin categoría",
        as.character(topic_label)
      )
    ) %>%
    count(topic_label, sort = TRUE)
  
  cat("\n11. Distribución temática\n")
  print(distribucion_temas)
} else {
  distribucion_temas <- NULL
  
  cat("\n11. Distribución temática\n")
  cat("La tabla no contiene la columna topic_label.\n")
}

# 11. Cobertura temporal

fecha_minima <- NA_character_
fecha_maxima <- NA_character_

if ("publication_date" %in% names(papers)) {
  fechas <- suppressWarnings(
    as.Date(papers$publication_date)
  )
  
  fechas_validas <- fechas[!is.na(fechas)]
  
  cat("\n12. Cobertura temporal\n")
  
  if (length(fechas_validas) > 0) {
    fecha_minima <- as.character(min(fechas_validas))
    fecha_maxima <- as.character(max(fechas_validas))
    
    cat("Fecha mínima:", fecha_minima, "\n")
    cat("Fecha máxima:", fecha_maxima, "\n")
  } else {
    cat("No se encontraron fechas válidas.\n")
  }
} else {
  cat("\n12. Cobertura temporal\n")
  cat("La tabla no contiene la columna publication_date.\n")
}

# 12. Longitud de los títulos

if ("title" %in% names(papers)) {
  longitud_titulos <- stringr::str_length(
    ifelse(
      is.na(papers$title),
      "",
      as.character(papers$title)
    )
  )
  
  cat("\n13. Longitud de los títulos en caracteres\n")
  print(summary(longitud_titulos))
} else {
  cat("\n13. Longitud de los títulos en caracteres\n")
  cat("La tabla no contiene la columna title.\n")
}

# 13. Longitud de los resúmenes

if ("abstract" %in% names(papers)) {
  longitud_resumenes <- stringr::str_length(
    ifelse(
      is.na(papers$abstract),
      "",
      as.character(papers$abstract)
    )
  )
  
  cat("\n14. Longitud de los resúmenes en caracteres\n")
  print(summary(longitud_resumenes))
} else {
  cat("\n14. Longitud de los resúmenes en caracteres\n")
  cat("La tabla no contiene la columna abstract.\n")
}

# 14. Ejemplo de registros

campos_para_mostrar <- base::intersect(
  c(
    "title",
    "abstract",
    "keywords",
    "keyword",
    "authors_raw",
    "publication_date",
    "topic_label",
    "doi"
  ),
  names(papers)
)

cat("\n15. Ejemplo de cinco registros\n")

if (length(campos_para_mostrar) > 0) {
  muestra_articulos <- papers %>%
    select(all_of(campos_para_mostrar)) %>%
    slice_head(n = 5) %>%
    mutate(
      abstract = if (
        "abstract" %in% names(.)
      ) {
        stringr::str_trunc(
          stringr::str_squish(
            stringr::str_remove_all(
              abstract,
              "<[^>]+>"
            )
          ),
          width = 200,
          side = "right",
          ellipsis = "..."
        )
      } else {
        NULL
      }
    )
  
  print(
    tibble::as_tibble(muestra_articulos),
    n = 5
  )
} else {
  cat("No se encontraron columnas relevantes para mostrar.\n")
}

# 15. Resumen general

resumen_general <- data.frame(
  indicador = c(
    "Total de artículos",
    "Total de columnas",
    "Artículos sin título",
    "Artículos sin resumen",
    "Artículos sin título ni resumen",
    "DOI no vacíos",
    "DOI únicos",
    "DOI duplicados"
  ),
  valor = c(
    nrow(papers),
    ncol(papers),
    contar_faltantes_texto(papers, "title"),
    contar_faltantes_texto(papers, "abstract"),
    sin_titulo_ni_resumen,
    cantidad_doi_no_vacios,
    cantidad_doi_unicos,
    cantidad_doi_duplicados
  ),
  stringsAsFactors = FALSE
)

cat("\n16. Resumen general\n")
print(resumen_general)

# 16. Exportación del diagnóstico

write.csv(
  resumen_faltantes,
  "diagnostico_faltantes_corpus.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_general,
  "diagnostico_general_corpus.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (!is.null(distribucion_temas)) {
  write.csv(
    distribucion_temas,
    "diagnostico_distribucion_temas.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# 17. Cierre de la conexión

if (DBI::dbIsValid(con_diagnostico)) {
  DBI::dbDisconnect(con_diagnostico)
}

cat("\n17. Diagnóstico finalizado correctamente\n")
cat("Archivos generados:\n")
cat("1. diagnostico_faltantes_corpus.csv\n")
cat("2. diagnostico_general_corpus.csv\n")

if (!is.null(distribucion_temas)) {
  cat("3. diagnostico_distribucion_temas.csv\n")
}