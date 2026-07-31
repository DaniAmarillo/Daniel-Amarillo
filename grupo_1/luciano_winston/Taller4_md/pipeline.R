# ==============================================================================
# pipeline.R -- PIPELINE COMPLETO OFFLINE (Taller 4)
# ==============================================================================

suppressPackageStartupMessages({
  library(RSQLite)
  library(Matrix)
  library(RSpectra)
})

source("global.R")

# ==============================================================================
# PARAMETROS
# ==============================================================================

# ------------------------------------------------------------------------------
# PARAMETROS  (todos justificables en el documento)
# ------------------------------------------------------------------------------
DB              <- "Springer_Visual_Miner.sqlite"
SALIDA          <- "search_index.rds"

MIN_DF          <- 3L     # termino debe aparecer en >= 3 documentos
MAX_DF_PROP     <- 0.50   # y en <= 50% del corpus
PESO_TITULO     <- 3L
PESO_KEYWORDS   <- 2L

BM25_K1         <- 1.2
BM25_B          <- 0.75

K_MAX           <- 300L   # componentes a calcular con svds
ENERGIA_OBJETIVO<- 0.80   # umbral de energia deseado (puede ser inalcanzable)
K_MIN           <- 50L
K_FORZADO       <- 200L   # elegido por el barrido de Precision@5 (ver evaluar())
                          # automatico. Se usa para pinchar el k elegido por el
                          # barrido de Precision@5 del documento.



K_LSA       <- 200L



ARCH_JUICIOS   <- "juicios.csv"
ARCH_PLANTILLA <- "juicios_plantilla.csv"
ARCH_RESULT    <- "evaluacion_resultados.csv"
ARCH_METRICAS  <- "evaluacion_metricas.csv"
ARCH_BARRIDO   <- "barrido_k.csv"

PROFUNDIDAD_POOL <- 10L
ESTRATEGIAS      <- c("bm25", "tfidf", "lsa", "hibrido")
REJILLA_K        <- c(25L, 50L, 100L, 150L, 200L, 250L, 300L)

# ==============================================================================
# LAS CINCO CONSULTAS
# ==============================================================================

CONSULTAS <- data.frame(
  id = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
  texto = c(
    "large language models",
    "systems that automatically create new text and images",
    "artificial intelligence applications",
    "federated learning privacy preservation",
    "aprendizaje profundo para el diagnostico de enfermedades",
    "cars that drive themselves without a human",
    "protecting patient data when training models across hospitals",
    "quantum computing for combinatorial optimization",
    "deep learning",
    "explainability methods that are not based on gradients"
  ),
  tipo = c(
    "terminos literales",
    "sinonimos / parafrasis generica",
    "general",
    "especifica",
    "adversa (espanol + dominio poco representado)",
    "sinonimos / desajuste de vocabulario",
    "parafrasis de consulta especifica",
    "especifica de nicho",
    "general extrema",
    "adversa (negacion)"
  ),
  justificacion = c(
    "Los terminos aparecen tal cual en titulos y resumenes. Favorece a la recuperacion lexica.",
    "Describe modelos generativos SIN usar la palabra 'generative'. La parafrasis usa terminos genericos (system, create, text, image) de baja capacidad discriminante.",
    "Consulta amplia y ambigua: casi todo el corpus es 'aplicaciones de IA'. Mide si el ranking discrimina cuando el termino no discrimina.",
    "Interseccion de tres conceptos concretos. Exige precision, no cobertura.",
    "Consulta en espanol, traducida termino a termino, sobre un dominio medico poco representado en la revista.",
    "Desajuste de vocabulario puro: la consulta dice 'cars' y 'drive', el corpus dice 'autonomous vehicles'. Es el escenario donde el espacio latente deberia superar al emparejamiento exacto.",
    "Misma intencion que Q4 pero sin usar ninguno de sus terminos tecnicos. Comparar Q4 con Q7 aisla el efecto del vocabulario manteniendo fija la necesidad de informacion.",
    "Tema estrecho con pocos articulos en el corpus. Penaliza a los metodos que devuelven resultados genericos por defecto.",
    "Dos palabras omnipresentes. Caso limite de saturacion: si todo es relevante, ninguna metrica discrimina.",
    "El sistema es una bolsa de palabras: no modela la negacion. Se espera que recupere justamente lo que la consulta excluye."
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# LECTURA ROBUSTA DE CSV
# ==============================================================================
.leer_csv <- function(f) {
  d <- utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                       check.names = FALSE)
  names(d) <- sub("^\ufeff", "", names(d))          
  names(d) <- sub("^X\\.U\\.FEFF\\.", "", names(d))  
  names(d) <- trimws(names(d))
  d
}

# ==============================================================================
# ETAPA 1 -- DEPENDENCIAS
# ==============================================================================
instalar_dependencias <- function() {
  PAQUETES <- list(

    # --- Aplicacion Shiny -------------------------------------------------------
    aplicacion = c(
      "shiny",        # framework
      "bslib",        # temas
      "shinyjs",      # JS desde R (navegacion entre paginas)
      "DT",           # tablas interactivas
      "highcharter"   # visualizaciones del Taller 2
    ),

    # --- Datos ------------------------------------------------------------------
    datos = c(
      "RSQLite",      # base de datos del corpus
      "DBI",          # capa de acceso (permite migrar a Postgres sin tocar codigo)
      "dplyr",        # manipulacion
      "tidyr",        # unnest de autores y referencias
      "purrr",        # map sobre listas JSON
      "jsonlite",     # autores y referencias serializados
      "stringr"       # manipulacion de texto
    ),

    # --- Extraccion (sin navegador headless) ------------------------------------
    scraping = c(
      "httr2",        # cliente HTTP
      "rvest",        # parseo de HTML
      "xml2",         # backend de rvest
      "tibble"        # data frames del scraper
    ),

    # --- Recuperacion de informacion --------------------------------------------
    recuperacion = c(
      "Matrix",       # matrices dispersas
      "SnowballC",    # stemming Porter
      "stringi"       # normalizacion de acentos a ASCII
    ),

    # --- Solo para construir el indice y evaluar (NO para desplegar) ------------
    construccion = c(
      "RSpectra"      
    ),

    # --- Solo para tejer el documento -------------------------------------------
    documento = c(
      "rmarkdown",
      "knitr",
      "ggplot2",
      "scales"
    )
  )

  todos <- unique(unlist(PAQUETES))
  faltan <- setdiff(todos, rownames(installed.packages()))

  if (length(faltan) == 0) {
    cat("Todos los paquetes ya estan instalados.\n")
  } else {
    cat("Faltan", length(faltan), "paquetes:\n  ",
        paste(faltan, collapse = ", "), "\n\nInstalando...\n")
    install.packages(faltan)
  }

  cat("\n--- Versiones instaladas ---\n")
  for (grupo in names(PAQUETES)) {
    cat("\n", toupper(grupo), "\n", sep = "")
    for (p in PAQUETES[[grupo]]) {
      v <- tryCatch(as.character(packageVersion(p)), error = function(e) "NO INSTALADO")
      cat(sprintf("  %-14s %s\n", p, v))
    }
  }

  cat(sprintf("\nR: %s\n", getRversion()))
  cat("\nNota: RSpectra solo se necesita para build_index.R y evaluacion.R.\n")
  cat("La aplicacion desplegada no lo requiere.\n\n")
  invisible(TRUE)
}


# ==============================================================================
# ETAPA 2 -- CONSTRUCCION DEL INDICE
# ==============================================================================
construir_indice <- function() {
  t_inicio <- Sys.time()
  cat("\n=== CONSTRUCCION DEL INDICE ===\n\n")

  # ==============================================================================
  # 1. CORPUS
  # ==============================================================================
  con <- dbConnect(RSQLite::SQLite(), DB)
  stopifnot(dbExistsTable(con, "papers"))

  campos_papers <- dbListFields(con, "papers")
  tiene_keywords <- "keywords" %in% campos_papers

  papers <- dbGetQuery(con, "
    SELECT paper_id, doi, url, title, abstract, authors_raw, publication_date,
           year, topic_label, citations, downloads, n_authors
    FROM papers")

  papers$keywords <- if (tiene_keywords) {
    dbGetQuery(con, "SELECT keywords FROM papers")$keywords
  } else {
    NA_character_
  }
  dbDisconnect(con)

  N <- nrow(papers)
  cat(sprintf("Articulos leidos de SQLite: %d\n", N))

  # --- Diagnostico de completitud -------------
  vacio <- function(x) is.na(x) | trimws(as.character(x)) %in% c("", "NA")

  diag_corpus <- list(
    n_articulos        = N,
    sin_titulo         = sum(vacio(papers$title)),
    sin_resumen        = sum(vacio(papers$abstract)),
    sin_keywords       = sum(vacio(papers$keywords)),
    keywords_en_esquema= tiene_keywords,
    sin_autores        = sum(vacio(papers$authors_raw)),
    sin_fecha          = sum(vacio(papers$publication_date))
  )

  cat(sprintf("  sin titulo   : %d\n", diag_corpus$sin_titulo))
  cat(sprintf("  sin resumen  : %d\n", diag_corpus$sin_resumen))
  cat(sprintf("  sin keywords : %d%s\n", diag_corpus$sin_keywords,
              if (!tiene_keywords) "  (la columna no existe en el esquema)" else ""))

  # --- Idioma: heuristica por palabras funcionales ------------------------------
  # El corpus deberia ser enteramente ingles; se verifica en vez de suponerlo.
  marcadores_es <- c(" el ", " la ", " los ", " las ", " de ", " que ", " para ",
                     " con ", " una ", " del ", " por ")
  marcadores_en <- c(" the ", " of ", " and ", " to ", " in ", " for ",
                     " with ", " that ", " is ", " are ")

  contar <- function(txt, marcas) {
    txt <- paste0(" ", tolower(ifelse(is.na(txt), "", txt)), " ")
    vapply(txt, function(s) sum(vapply(marcas, function(m)
      lengths(regmatches(s, gregexpr(m, s, fixed = TRUE))), numeric(1))), numeric(1))
  }

  base_idioma <- paste(ifelse(is.na(papers$title), "", papers$title),
                       ifelse(is.na(papers$abstract), "", papers$abstract))
  es_score <- contar(base_idioma, marcadores_es)
  en_score <- contar(base_idioma, marcadores_en)
  idioma   <- ifelse(en_score >= es_score, "en", "es")
  diag_corpus$idiomas <- table(idioma)
  cat("  idiomas      : "); print(diag_corpus$idiomas)

  # --- Criterio de inclusion ----------------------------------------------------
  usable <- !(vacio(papers$title) & vacio(papers$abstract))
  diag_corpus$excluidos_sin_texto <- sum(!usable)
  papers <- papers[usable, , drop = FALSE]
  N <- nrow(papers)
  cat(sprintf("  excluidos    : %d (sin titulo ni resumen)\n", diag_corpus$excluidos_sin_texto))
  cat(sprintf("Corpus final : %d articulos\n\n", N))

  # ==============================================================================
  # 2. TOKENIZACION
  # ==============================================================================
  cat("Tokenizando...\n")
  t0 <- Sys.time()

  textos <- construir_texto(papers$title, papers$abstract, papers$keywords,
                            peso_titulo = PESO_TITULO, peso_keywords = PESO_KEYWORDS)
  tokens <- tokenizar(textos, bigramas = TRUE)

  tokens_sup <- tokenizar(textos, bigramas = FALSE, solo_superficie = TRUE)
  df_sup     <- table(unlist(lapply(tokens_sup, unique), use.names = FALSE))
  df_sup     <- df_sup[df_sup >= MIN_DF]          
  superficie    <- names(df_sup)
  superficie_df <- stats::setNames(as.integer(df_sup), superficie)
  rm(tokens_sup); invisible(gc(verbose = FALSE))

  t_token <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %.1f s | tokens totales: %s | promedio por doc: %.0f\n",
              t_token, format(sum(lengths(tokens)), big.mark = ","),
              mean(lengths(tokens))))
  cat(sprintf("  diccionario de superficie (para corregir consultas): %s formas\n",
              format(length(superficie), big.mark = ",")))

  # ==============================================================================
  # 3. MATRIZ DOCUMENTO-TERMINO
  # ==============================================================================
  doc_id    <- rep(seq_len(N), lengths(tokens))
  plano     <- unlist(tokens, use.names = FALSE)
  vocab_ini <- unique(plano)

  M <- sparseMatrix(i = doc_id, j = match(plano, vocab_ini),
                    x = rep(1, length(plano)),
                    dims = c(N, length(vocab_ini)))   

  df_ini <- Matrix::colSums(M > 0)
  cat(sprintf("\nVocabulario inicial : %s terminos\n",
              format(length(vocab_ini), big.mark = ",")))

  # --- Poda: terminos demasiado raros o demasiado frecuentes --------------------

  mantener  <- which(df_ini >= MIN_DF & df_ini <= MAX_DF_PROP * N)
  M         <- M[, mantener, drop = FALSE]
  vocabulario <- vocab_ini[mantener]
  df        <- df_ini[mantener]
  V_dim     <- length(vocabulario)

  cat(sprintf("Vocabulario podado  : %s terminos (min_df=%d, max_df=%.0f%%)\n",
              format(V_dim, big.mark = ","), MIN_DF, 100 * MAX_DF_PROP))
  cat(sprintf("  unigramas: %s | bigramas: %s\n",
              format(sum(!grepl("_", vocabulario)), big.mark = ","),
              format(sum(grepl("_", vocabulario)), big.mark = ",")))
  cat(sprintf("Densidad de la matriz: %.4f%% (%s celdas no nulas)\n",
              100 * length(M@x) / (as.numeric(N) * V_dim),
              format(length(M@x), big.mark = ",")))

  docs_vacios <- sum(Matrix::rowSums(M) == 0)
  if (docs_vacios > 0) cat(sprintf("  AVISO: %d documentos quedaron sin terminos tras la poda\n", docs_vacios))


  Mt <- as(M, "TsparseMatrix")
  fi <- Mt@x                
  di <- Mt@i + 1L           
  ti <- Mt@j + 1L           

  # ==============================================================================
  # 4. TF-IDF  (representacion dispersa, base de LSA)
  # ==============================================================================
  idf <- log(N / df)                      
  x_tfidf <- (1 + log(fi)) * idf[ti]       

  X <- sparseMatrix(i = di, j = ti, x = x_tfidf, dims = c(N, V_dim))
  normas <- sqrt(Matrix::rowSums(X^2)); normas[normas == 0] <- 1
  X <- Matrix::Diagonal(x = 1 / normas) %*% X
  X <- as(X, "CsparseMatrix")

  # ==============================================================================
  # 5. PESOS BM25 PRECALCULADOS
  # ==============================================================================

  longitud <- Matrix::rowSums(M)
  avgdl    <- mean(longitud[longitud > 0])
  idf_bm25 <- log(1 + (N - df + 0.5) / (df + 0.5))

  denominador <- fi + BM25_K1 * (1 - BM25_B + BM25_B * longitud[di] / avgdl)
  x_bm25 <- idf_bm25[ti] * fi * (BM25_K1 + 1) / denominador

  BM <- sparseMatrix(i = di, j = ti, x = x_bm25, dims = c(N, V_dim))
  BM <- as(BM, "CsparseMatrix")

  cat(sprintf("\nBM25 listo (k1=%.1f, b=%.2f, avgdl=%.0f)\n", BM25_K1, BM25_B, avgdl))

  # ==============================================================================
  # 6. REDUCCION DIMENSIONAL -- TRUNCATED SVD (LSA)
  # ==============================================================================

  k_calc <- min(K_MAX, N - 2L, V_dim - 2L)
  cat(sprintf("\nCalculando SVD truncado con k=%d...\n", k_calc))

  t0 <- Sys.time()
  sv <- RSpectra::svds(X, k = k_calc)
  t_svd <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  energia_total <- sum(X@x^2)
  energia_acum  <- cumsum(sv$d^2) / energia_total

  # --- Tabla de energia ----
  cat(sprintf("  SVD calculado en %.1f s\n\n", t_svd))
  cat("  Energia acumulada por numero de componentes:\n")
  rejilla <- c(25L, 50L, 100L, 150L, 200L, 250L, 300L)
  rejilla <- rejilla[rejilla <= k_calc]
  tabla_energia <- data.frame(
    k       = rejilla,
    energia = round(100 * energia_acum[rejilla], 1)
  )
  for (r in seq_len(nrow(tabla_energia))) {
    cat(sprintf("    k=%-4d %5.1f%%\n", tabla_energia$k[r], tabla_energia$energia[r]))
  }

  # --- Seleccion de k ----------------------------------------------------------
  criterio <- NA_character_

  if (!is.null(K_FORZADO)) {
    k <- min(as.integer(K_FORZADO), k_calc)
    criterio <- sprintf("k fijado manualmente en %d (elegido por barrido de Precision@5)", k)
  } else {
    alcanza <- which(energia_acum >= ENERGIA_OBJETIVO)[1]
    if (!is.na(alcanza)) {
      k <- max(min(alcanza, k_calc), K_MIN)
      criterio <- sprintf("primer k con energia >= %.0f%%", 100 * ENERGIA_OBJETIVO)
    } else {
      k <- k_calc
      criterio <- sprintf("umbral de %.0f%% INALCANZABLE con k<=%d; se usa el maximo calculado",
                          100 * ENERGIA_OBJETIVO, k_calc)
      cat(sprintf("\n  AVISO: la energia no llega al %.0f%% ni con k=%d (maximo %.1f%%).\n",
                  100 * ENERGIA_OBJETIVO, k_calc, 100 * max(energia_acum)))
      cat("         El umbral de varianza NO es un buen criterio para LSA sobre texto.\n")
      cat("         Elige k por rendimiento de recuperacion y fijalo en K_FORZADO.\n")
    }
  }

  cat(sprintf("\n  k elegido: %d  |  energia: %.1f%%  |  criterio: %s\n",
              k, 100 * energia_acum[k], criterio))

  V_mat    <- sv$v[, seq_len(k), drop = FALSE]     
  docs_lsa <- as.matrix(X %*% V_mat)                

  nl <- sqrt(rowSums(docs_lsa^2)); nl[nl == 0] <- 1
  docs_lsa <- docs_lsa / nl                         

  # ==============================================================================
  # 7. EMPAQUETADO
  # ==============================================================================
  meta <- papers[, c("paper_id", "doi", "url", "title", "abstract", "authors_raw",
                     "publication_date", "year", "topic_label", "citations",
                     "downloads")]

  indice <- list(
    vocabulario = vocabulario,
    superficie    = superficie,      
    superficie_df = superficie_df,   
    idf         = idf,
    dtm         = M,        
    tfidf       = X,        
    bm25        = BM,
    V           = V_mat,
    docs_lsa    = docs_lsa,
    meta        = meta,

    parametros = list(
      min_df = MIN_DF, max_df_prop = MAX_DF_PROP,
      peso_titulo = PESO_TITULO, peso_keywords = PESO_KEYWORDS,
      bm25_k1 = BM25_K1, bm25_b = BM25_B, avgdl = avgdl,
      k_calculado = k_calc, k_final = k, energia_objetivo = ENERGIA_OBJETIVO,
      criterio_k = criterio
    ),

    tabla_energia = tabla_energia,

    diagnostico = c(diag_corpus, list(
      vocabulario_inicial = length(vocab_ini),
      vocabulario_final   = V_dim,
      unigramas           = sum(!grepl("_", vocabulario)),
      bigramas            = sum(grepl("_", vocabulario)),
      nnz                 = length(M@x),
      densidad            = length(M@x) / (as.numeric(N) * V_dim),
      docs_vacios         = docs_vacios,
      energia_k           = energia_acum[k],
      segundos_tokenizar  = t_token,
      segundos_svd        = t_svd
    )),


    valores_singulares = sv$d,
    energia_acumulada  = energia_acum,

    construido = Sys.time()
  )

  # --- Tamanos en memoria  ----------
  tam <- function(o) as.numeric(object.size(o)) / 1024^2
  indice$diagnostico$mb_tfidf_disperso <- tam(X)
  indice$diagnostico$mb_bm25_disperso  <- tam(BM)
  indice$diagnostico$mb_lsa_denso      <- tam(docs_lsa)
  indice$diagnostico$mb_matriz_V       <- tam(V_mat)
  indice$diagnostico$mb_dtm            <- tam(M)
  indice$diagnostico$mb_tfidf_denso_teorico <- as.numeric(N) * V_dim * 8 / 1024^2

  saveRDS(indice, SALIDA, compress = "xz")

  # ==============================================================================
  # 8. VERIFICACION
  # ==============================================================================
  cat("\n=== VERIFICACION ===\n")
  cat(sprintf("Dimension original (vocabulario) : %s\n", format(V_dim, big.mark = ",")))
  cat(sprintf("Dimension reducida (componentes) : %d\n", k))
  cat(sprintf("Reduccion                        : %.1fx\n", V_dim / k))
  cat(sprintf("TF-IDF disperso                  : %.2f MB\n", indice$diagnostico$mb_tfidf_disperso))
  cat(sprintf("  (denso equivaldria a           : %.0f MB)\n", indice$diagnostico$mb_tfidf_denso_teorico))
  cat(sprintf("LSA docs (denso)                 : %.2f MB\n", indice$diagnostico$mb_lsa_denso))
  cat(sprintf("Matriz de proyeccion V           : %.2f MB  <- domina el tamano del indice\n",
              indice$diagnostico$mb_matriz_V))
  cat(sprintf("Archivo                          : %s (%.2f MB)\n",
              SALIDA, file.size(SALIDA) / 1024^2))

  idx <- cargar_indice(SALIDA)
  prueba <- "generative artificial intelligence for medical diagnosis"

  cat("\nPrueba del corrector (formas mal escritas):\n")
  for (mal in c("macine learnin", "Dianostico ml", "nueral netwoks", "generativ ai")) {
    d <- diagnosticar_consulta(mal, idx)
    cat(sprintf("  \"%s\"  ->  %s\n", mal, d$mensaje))
  }

  cat(sprintf("\nConsulta de prueba: \"%s\"\n", prueba))

  for (w in c("bm25", "tfidf", "lsa")) {
    invisible(buscar_articulos(prueba, idx, estrategia = w, n = 3))
  }

  for (est in c("bm25", "tfidf", "lsa", "hibrido")) {
    reps <- 10L
    t0 <- Sys.time()
    for (z in seq_len(reps)) r <- buscar_articulos(prueba, idx, estrategia = est, n = 3)
    ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000 / reps
    cat(sprintf("\n  [%s]  %.1f ms (mediana de %d corridas, en caliente)\n",
                toupper(est), ms, reps))
    if (nrow(r) == 0) {
      cat("    (sin resultados)\n")
    } else {
      for (i in seq_len(nrow(r)))
        cat(sprintf("    %d. (%.4f) %s\n", i, r$puntaje[i], substr(r$titulo[i], 1, 68)))
    }
  }

  cat(sprintf("\nTiempo total: %.1f s\n\n",
              as.numeric(difftime(Sys.time(), t_inicio, units = "secs"))))
  invisible(SALIDA)
}


# ==============================================================================
# ETAPA 3 -- ANATOMIA DEL VOCABULARIO Y ABLACION DE BIGRAMAS
# ==============================================================================
analizar_vocabulario <- function() {
  cat("\n=== ANATOMIA DEL VOCABULARIO ===\n\n")

  # ==============================================================================
  # 1. CORPUS Y TOKENIZACION
  # ==============================================================================
  con <- dbConnect(RSQLite::SQLite(), DB)
  papers <- dbGetQuery(con, "
    SELECT paper_id, doi, url, title, abstract, keywords, authors_raw,
           publication_date, year, topic_label, citations, downloads
    FROM papers")
  dbDisconnect(con)

  N <- nrow(papers)
  textos <- construir_texto(papers$title, papers$abstract, papers$keywords)

  tok_uni  <- tokenizar(textos, bigramas = FALSE)
  tok_ambos <- tokenizar(textos, bigramas = TRUE)

  n_tok_uni  <- sum(lengths(tok_uni))
  n_tok_ambos <- sum(lengths(tok_ambos))

  cat(sprintf("Documentos                  : %d\n", N))
  cat(sprintf("Tokens (solo unigramas)     : %s\n", format(n_tok_uni, big.mark = ",")))
  cat(sprintf("Tokens (unigramas+bigramas) : %s\n", format(n_tok_ambos, big.mark = ",")))

  # ==============================================================================
  # 2. DE DONDE SALE EL RECORTE
  # ==============================================================================
  df_de <- function(tokens) {
    pares <- unique(data.frame(
      doc = rep(seq_along(tokens), lengths(tokens)),
      tok = unlist(tokens, use.names = FALSE),
      stringsAsFactors = FALSE
    ))
    table(pares$tok)
  }

  df_todos <- df_de(tok_ambos)
  es_bigrama <- grepl("_", names(df_todos), fixed = TRUE)

  cat(sprintf("\nTipos unicos totales        : %s\n", format(length(df_todos), big.mark = ",")))
  cat(sprintf("  unigramas                 : %s\n", format(sum(!es_bigrama), big.mark = ",")))
  cat(sprintf("  bigramas                  : %s\n", format(sum(es_bigrama), big.mark = ",")))

  cat("\n--- Distribucion de la frecuencia documental (ley de Zipf) ---\n\n")
  tabla_df <- data.frame(
    df          = c("1 (hapax)", "2", "3-5", "6-20", "21-100", ">100"),
    unigramas   = c(sum(df_todos[!es_bigrama] == 1),
                    sum(df_todos[!es_bigrama] == 2),
                    sum(df_todos[!es_bigrama] %in% 3:5),
                    sum(df_todos[!es_bigrama] %in% 6:20),
                    sum(df_todos[!es_bigrama] %in% 21:100),
                    sum(df_todos[!es_bigrama] > 100)),
    bigramas    = c(sum(df_todos[es_bigrama] == 1),
                    sum(df_todos[es_bigrama] == 2),
                    sum(df_todos[es_bigrama] %in% 3:5),
                    sum(df_todos[es_bigrama] %in% 6:20),
                    sum(df_todos[es_bigrama] %in% 21:100),
                    sum(df_todos[es_bigrama] > 100))
  )
  tabla_df$total <- tabla_df$unigramas + tabla_df$bigramas
  tabla_df$pct   <- round(100 * tabla_df$total / length(df_todos), 1)
  print(tabla_df, row.names = FALSE)
  write.csv(tabla_df, "vocab_zipf.csv", row.names = FALSE, fileEncoding = "UTF-8")

  # --- Cuanto elimina cada filtro POR SEPARADO ---------------------------------
  corta_min <- df_todos < MIN_DF
  corta_max <- df_todos > MAX_DF_PROP * N

  cat("\n--- Aporte de cada filtro (medidos por separado) ---\n\n")
  cat(sprintf("min_df < %d          elimina : %s terminos (%.1f%%)\n",
              MIN_DF, format(sum(corta_min), big.mark = ","),
              100 * mean(corta_min)))
  cat(sprintf("max_df > %.0f%% del corpus elimina : %s terminos (%.2f%%)\n",
              100 * MAX_DF_PROP, format(sum(corta_max), big.mark = ","),
              100 * mean(corta_max)))
  cat(sprintf("ambos a la vez                    : %s terminos\n",
              format(sum(corta_min & corta_max), big.mark = ",")))
  cat(sprintf("sobreviven                        : %s terminos\n",
              format(sum(!corta_min & !corta_max), big.mark = ",")))

  write.csv(data.frame(
    tipos_iniciales   = length(df_todos),
    unigramas_ini     = sum(!es_bigrama),
    bigramas_ini      = sum(es_bigrama),
    elimina_min_df    = sum(corta_min),
    elimina_max_df    = sum(corta_max),
    elimina_ambos     = sum(corta_min & corta_max),
    sobreviven        = sum(!corta_min & !corta_max),
    min_df            = MIN_DF,
    max_df_prop       = MAX_DF_PROP,
    n_documentos      = N
  ), "vocab_filtros.csv", row.names = FALSE, fileEncoding = "UTF-8")

  if (sum(corta_max) > 0) {
    write.csv(data.frame(
      termino = names(sort(df_todos[corta_max], decreasing = TRUE)),
      df      = as.integer(sort(df_todos[corta_max], decreasing = TRUE)),
      pct_corpus = round(100 * as.integer(sort(df_todos[corta_max], decreasing = TRUE)) / N, 1)
    ), "vocab_eliminados_maxdf.csv", row.names = FALSE, fileEncoding = "UTF-8")
  }

  if (sum(corta_max) > 0) {
    cat(sprintf("\nTerminos eliminados por max_df (los que aparecen en >%d documentos):\n",
                round(MAX_DF_PROP * N)))
    altos <- sort(df_todos[corta_max], decreasing = TRUE)
    for (i in seq_len(min(20, length(altos)))) {
      cat(sprintf("   %-28s df=%d (%.0f%% del corpus)\n",
                  names(altos)[i], altos[i], 100 * altos[i] / N))
    }
  } else {
    cat("\nmax_df NO elimino ningun termino: ningun tipo supera el umbral.\n")
    cat("El filtro esta declarado pero no hace trabajo en este corpus.\n")
  }

  cat("\n--- Terminos mas frecuentes que SI sobreviven ---\n\n")
  sobrev <- sort(df_todos[!corta_min & !corta_max], decreasing = TRUE)
  for (i in seq_len(min(15, length(sobrev)))) {
    cat(sprintf("   %-28s df=%d (%.0f%%)\n", names(sobrev)[i], sobrev[i],
                100 * sobrev[i] / N))
  }

  # ==============================================================================
  # 3. ABLACION: LOS BIGRAMAS, VALEN LO QUE CUESTAN?
  # ==============================================================================


  .indice_temporal <- function(tokens, meta, k = K_LSA) {
    doc_id <- rep(seq_along(tokens), lengths(tokens))
    plano  <- unlist(tokens, use.names = FALSE)
    vocab0 <- unique(plano)

    M <- sparseMatrix(i = doc_id, j = match(plano, vocab0),
                      x = rep(1, length(plano)),
                      dims = c(length(tokens), length(vocab0)))

    dfi <- Matrix::colSums(M > 0)
    keep <- which(dfi >= MIN_DF & dfi <= MAX_DF_PROP * length(tokens))
    M <- M[, keep, drop = FALSE]
    vocabulario <- vocab0[keep]
    dfi <- dfi[keep]
    n <- length(tokens); V <- length(vocabulario)

    Mt <- as(M, "TsparseMatrix")
    fi <- Mt@x; di <- Mt@i + 1L; ti <- Mt@j + 1L

    idf <- log(n / dfi)
    X <- sparseMatrix(i = di, j = ti, x = (1 + log(fi)) * idf[ti], dims = c(n, V))
    nr <- sqrt(Matrix::rowSums(X^2)); nr[nr == 0] <- 1
    X <- as(Matrix::Diagonal(x = 1 / nr) %*% X, "CsparseMatrix")

    longitud <- Matrix::rowSums(M)
    avgdl <- mean(longitud[longitud > 0])
    idfb <- log(1 + (n - dfi + 0.5) / (dfi + 0.5))
    den <- fi + BM25_K1 * (1 - BM25_B + BM25_B * longitud[di] / avgdl)
    BM <- as(sparseMatrix(i = di, j = ti, x = idfb[ti] * fi * (BM25_K1 + 1) / den,
                          dims = c(n, V)), "CsparseMatrix")

    kk <- min(k, n - 2L, V - 2L)
    sv <- RSpectra::svds(X, k = kk)
    Vm <- sv$v
    D  <- as.matrix(X %*% Vm)
    nl <- sqrt(rowSums(D^2)); nl[nl == 0] <- 1

    list(vocabulario = vocabulario, idf = idf, tfidf = X, bm25 = BM,
         V = Vm, docs_lsa = D / nl, meta = meta,
         parametros = list(k_final = kk, bm25_k1 = BM25_K1, bm25_b = BM25_B))
  }

  meta <- papers[, c("paper_id", "doi", "url", "title", "abstract", "authors_raw",
                     "publication_date", "year", "topic_label", "citations", "downloads")]

  if (!file.exists("juicios.csv") || !file.exists("evaluacion_resultados.csv")) {
    cat("\n\n(Sin juicios.csv o evaluacion_resultados.csv: se omite la ablacion.)\n\n")
  } else {

    juicios <- .leer_csv("juicios.csv")
    juicios$relevante <- suppressWarnings(as.integer(juicios$relevante))
    juicios$relevante[is.na(juicios$relevante)] <- 0L


    res <- .leer_csv("evaluacion_resultados.csv")
    qs <- unique(res[, c("consulta_id", "consulta")])


    rel_q <- tapply(juicios$relevante, juicios$consulta_id, sum)
    qs <- qs[!(qs$consulta_id %in% names(rel_q)[rel_q == 0]), ]

    llave <- function(q, d) paste(q, tolower(trimws(d)), sep = "||")
    mapa <- setNames(juicios$relevante, llave(juicios$consulta_id, juicios$doi))

    p5 <- function(indice, est) {
      v <- numeric(nrow(qs))
      for (i in seq_len(nrow(qs))) {
        r <- buscar_articulos(qs$consulta[i], indice, estrategia = est, n = 5)
        if (nrow(r) == 0) { v[i] <- 0; next }
        rel <- unname(mapa[llave(qs$consulta_id[i], r$doi)])
        rel[is.na(rel)] <- 0L
        v[i] <- sum(rel) / 5
      }
      mean(v)
    }

    cat("\n\n=== ABLACION: UNIGRAMAS SOLOS vs UNIGRAMAS + BIGRAMAS ===\n\n")
    cat(sprintf("Evaluado sobre %d consultas con al menos un relevante.\n\n", nrow(qs)))

    cat("Construyendo indice solo con unigramas...\n")
    ix_uni <- .indice_temporal(tok_uni, meta)
    cat("Construyendo indice con unigramas + bigramas...\n")
    ix_bi  <- .indice_temporal(tok_ambos, meta)

    comparacion <- data.frame(
      representacion = c("unigramas", "unigramas + bigramas"),
      n_terminos = c(length(ix_uni$vocabulario), length(ix_bi$vocabulario)),
      mb_indice  = round(c(as.numeric(object.size(ix_uni)),
                           as.numeric(object.size(ix_bi))) / 1024^2, 2),
      p5_bm25 = round(c(p5(ix_uni, "bm25"), p5(ix_bi, "bm25")), 4),
      p5_tfidf = round(c(p5(ix_uni, "tfidf"), p5(ix_bi, "tfidf")), 4),
      p5_lsa  = round(c(p5(ix_uni, "lsa"),  p5(ix_bi, "lsa")), 4),
      p5_hib  = round(c(p5(ix_uni, "hibrido"), p5(ix_bi, "hibrido")), 4),
      stringsAsFactors = FALSE
    )

    cat("\n")
    print(comparacion, row.names = FALSE)

    write.csv(comparacion, "ablacion_bigramas.csv", row.names = FALSE,
              fileEncoding = "UTF-8")

    delta <- comparacion[2, c("p5_bm25", "p5_tfidf", "p5_lsa", "p5_hib")] -
             comparacion[1, c("p5_bm25", "p5_tfidf", "p5_lsa", "p5_hib")]
    cat("\nDiferencia que aportan los bigramas (positivo = ayudan):\n")
    print(round(delta, 4), row.names = FALSE)
    cat(sprintf("\nCosto: %+d terminos (%.1fx el vocabulario) y %+.2f MB de indice.\n",
                comparacion$n_terminos[2] - comparacion$n_terminos[1],
                comparacion$n_terminos[2] / comparacion$n_terminos[1],
                comparacion$mb_indice[2] - comparacion$mb_indice[1]))
    cat("\nEscritos: ablacion_bigramas.csv, vocab_zipf.csv, vocab_filtros.csv, vocab_eliminados_maxdf.csv\n\n")
  }
  invisible(TRUE)
}


# ==============================================================================
# ETAPA 4 -- EVALUACION
# ==============================================================================
evaluar <- function() {
  cat("\n=== EVALUACION DEL SISTEMA DE RECUPERACION ===\n\n")

  idx <- cargar_indice()
  if (is.null(idx)) stop("No se encontro search_index.rds. Ejecuta construir_indice() primero.")

  cat(sprintf("Indice: %d articulos | %s terminos | k = %d\n\n",
              nrow(idx$meta), format(length(idx$vocabulario), big.mark = ","),
              idx$parametros$k_final))

  # ==============================================================================
  # EJECUCION DE TODAS LAS CONSULTAS
  # ==============================================================================
  .correr_consultas <- function(indice, profundidad = PROFUNDIDAD_POOL,
                                estrategias = ESTRATEGIAS) {
    filas <- list()
    for (i in seq_len(nrow(CONSULTAS))) {
      for (est in estrategias) {
        r <- buscar_articulos(CONSULTAS$texto[i], indice, estrategia = est, n = profundidad)
        if (nrow(r) == 0) next
        filas[[length(filas) + 1]] <- data.frame(
          consulta_id = CONSULTAS$id[i],
          consulta    = CONSULTAS$texto[i],
          estrategia  = est,
          posicion    = r$posicion,
          doi         = r$doi,
          titulo      = r$titulo,
          puntaje     = r$puntaje,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(filas) == 0) return(data.frame())
    do.call(rbind, filas)
  }

  resultados <- .correr_consultas(idx)
  write.csv(resultados, ARCH_RESULT, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("Resultados de las %d consultas x %d estrategias -> %s (%d filas)\n",
              nrow(CONSULTAS), length(ESTRATEGIAS), ARCH_RESULT, nrow(resultados)))

  # ==============================================================================
  # POOL DE JUICIOS  (con fusion incremental)
  # ==============================================================================

  construir_pool <- function() {
    pool <- unique(resultados[, c("consulta_id", "consulta", "doi", "titulo")])

    veces <- aggregate(estrategia ~ consulta_id + doi, data = resultados,
                       FUN = function(x) length(unique(x)))
    names(veces)[3] <- "n_metodos"

    mejor <- aggregate(posicion ~ consulta_id + doi, data = resultados, FUN = min)
    names(mejor)[3] <- "mejor_posicion"

    pool <- merge(pool, veces, by = c("consulta_id", "doi"))
    pool <- merge(pool, mejor, by = c("consulta_id", "doi"))
    pool <- pool[order(pool$consulta_id, -pool$n_metodos, pool$mejor_posicion), ]

    ab <- idx$meta$abstract[match(pool$doi, idx$meta$doi)]
    pool$resumen_corto <- substr(ifelse(is.na(ab), "", ab), 1, 260)
    pool$relevante <- NA_integer_
    pool
  }

  pool <- construir_pool()

  en_pool <- function(d) paste(d$consulta_id, tolower(trimws(d$doi)), sep = "||")

  if (file.exists(ARCH_JUICIOS)) {
    previos <- .leer_csv(ARCH_JUICIOS)
    previos$relevante <- suppressWarnings(as.integer(previos$relevante))

    pool$relevante <- previos$relevante[match(en_pool(pool), en_pool(previos))]
    cat(sprintf("Juicios previos reutilizados: %d de %d pares del pool actual\n",
                sum(!is.na(pool$relevante)), nrow(pool)))

    huerfanos <- previos[is.na(match(en_pool(previos), en_pool(pool))), ]
    huerfanos <- huerfanos[!is.na(huerfanos$relevante), ]

    if (nrow(huerfanos) > 0) {
      for (cc in setdiff(names(pool), names(huerfanos))) huerfanos[[cc]] <- NA
      huerfanos$n_metodos     <- 0L   
      huerfanos$mejor_posicion <- NA_integer_
      pool <- rbind(pool, huerfanos[, names(pool)])
      cat(sprintf("Juicios conservados fuera del pool actual: %d\n", nrow(huerfanos)))
    }
  }

  pool$en_pool_actual <- pool$n_metodos > 0

  faltantes <- sum(is.na(pool$relevante) & pool$en_pool_actual)

  if (faltantes > 0) {
    salida <- pool[order(!is.na(pool$relevante), pool$consulta_id), 
                   c("consulta_id", "consulta", "doi", "titulo", "resumen_corto",
                     "n_metodos", "mejor_posicion", "relevante")]
    salida$relevante[is.na(salida$relevante)] <- ""
    write.csv(salida, ARCH_PLANTILLA, row.names = FALSE, fileEncoding = "UTF-8")

    cat(sprintf("\nPool -> %s (%d pares, %d SIN juzgar)\n",
                ARCH_PLANTILLA, nrow(pool), faltantes))
    cat("\n--------------------------------------------------------------\n")
    cat("Faltan juicios. Los que ya hiciste vienen prellenados.\n\n")
    cat("  1. Abre", ARCH_PLANTILLA, "y llena las filas vacias (1 o 0)\n")
    cat("  2. Guardalo como", ARCH_JUICIOS, "\n")
    cat("  3. Vuelve a ejecutar este script\n")
    cat("--------------------------------------------------------------\n\n")
    cat("Las filas SIN llenar quedaron arriba del archivo.\n\n")
    cat("Pendientes por consulta:\n")
    print(table(pool$consulta_id[is.na(pool$relevante) & pool$en_pool_actual]))
    cat("\n")
    quit_flag <- TRUE
  } else {
    quit_flag <- FALSE
  }

  if (exists("quit_flag") && isTRUE(quit_flag)) {
    cat("(evaluacion incompleta: faltan juicios)\n\n")
  } else {

  # ==============================================================================
  # METRICAS
  # ==============================================================================

  juicios_todos <- pool
  juicios_todos$relevante[is.na(juicios_todos$relevante)] <- 0L


  juicios <- juicios_todos[juicios_todos$en_pool_actual, ]

  clave <- function(q, d) paste(q, tolower(trimws(d)), sep = "||")
  mapa_rel <- setNames(juicios_todos$relevante,
                      clave(juicios_todos$consulta_id, juicios_todos$doi))

  es_relevante <- function(qid, dois) {
    v <- unname(mapa_rel[clave(qid, dois)])
    ifelse(is.na(v), 0L, v)
  }

  cat(sprintf("\nJuicios cargados: %d pares | %d relevantes (%.0f%%)\n\n",
              nrow(juicios), sum(juicios$relevante),
              100 * mean(juicios$relevante)))

  # ------------------------------------------------------------------------------
  # Metricas
  # ------------------------------------------------------------------------------
  precision_en_k <- function(rel, k = 5L) {
    if (length(rel) == 0) return(0)
    sum(head(rel, k)) / k
  }

  rr <- function(rel) {                 
    p <- which(rel == 1L)
    if (length(p) == 0) return(0)
    1 / p[1]
  }

  ndcg_en_k <- function(rel, k = 5L) {
    rel <- head(rel, k)
    if (length(rel) == 0 || sum(rel) == 0) return(0)
    dcg   <- sum(rel / log2(seq_along(rel) + 1))
    ideal <- sort(rel, decreasing = TRUE)
    idcg  <- sum(ideal / log2(seq_along(ideal) + 1))
    if (idcg == 0) return(0)
    dcg / idcg
  }

  evaluar_indice <- function(indice, estrategias = ESTRATEGIAS, etiqueta = NA) {
    out <- list()
    for (i in seq_len(nrow(CONSULTAS))) {
      qid <- CONSULTAS$id[i]
      for (est in estrategias) {
        r <- buscar_articulos(CONSULTAS$texto[i], indice, estrategia = est,
                              n = PROFUNDIDAD_POOL)
        rel <- if (nrow(r) == 0) integer(0) else es_relevante(qid, r$doi)
        out[[length(out) + 1]] <- data.frame(
          etiqueta      = etiqueta,
          consulta_id   = qid,
          tipo          = CONSULTAS$tipo[i],
          estrategia    = est,
          n_devueltos   = nrow(r),
          precision_5   = precision_en_k(rel, 5L),
          precision_10  = precision_en_k(rel, 10L),
          mrr           = rr(rel),
          ndcg_5        = ndcg_en_k(rel, 5L),
          ndcg_10       = ndcg_en_k(rel, 10L),
          stringsAsFactors = FALSE
        )
      }
    }
    do.call(rbind, out)
  }

  metricas <- evaluar_indice(idx, etiqueta = sprintf("k=%d", idx$parametros$k_final))
  write.csv(metricas, ARCH_METRICAS, row.names = FALSE, fileEncoding = "UTF-8")

  cat("--- Precision@5 por consulta y estrategia ---\n\n")
  tabla_p5 <- reshape(metricas[, c("consulta_id", "estrategia", "precision_5")],
                      idvar = "consulta_id", timevar = "estrategia", direction = "wide")
  names(tabla_p5) <- gsub("precision_5.", "", names(tabla_p5), fixed = TRUE)
  print(tabla_p5, row.names = FALSE)

  # ------------------------------------------------------------------------------
  # Consultas sin ningun documento relevante
  # ------------------------------------------------------------------------------

  rel_por_q <- tapply(juicios$relevante, juicios$consulta_id, sum)
  q_sin_rel <- names(rel_por_q)[rel_por_q == 0]

  promediar <- function(d) {
    a <- aggregate(cbind(precision_5, precision_10, mrr, ndcg_5, ndcg_10) ~ estrategia,
                   data = d, FUN = mean)
    a[, -1] <- round(a[, -1], 4)
    a
  }

  cat(sprintf("\n--- Promedios sobre las %d consultas ---\n\n", nrow(CONSULTAS)))
  print(promediar(metricas), row.names = FALSE)

  if (length(q_sin_rel) > 0) {
    cat(sprintf("\n--- Promedios excluyendo %s (sin documentos relevantes en el pool) ---\n\n",
                paste(q_sin_rel, collapse = ", ")))
    print(promediar(metricas[!(metricas$consulta_id %in% q_sin_rel), ]), row.names = FALSE)
  }

  # ------------------------------------------------------------------------------
  # Poder discriminante consulta por consulta
  # ------------------------------------------------------------------------------

  rango_q <- aggregate(precision_5 ~ consulta_id, data = metricas,
                       FUN = function(x) round(max(x) - min(x), 2))
  names(rango_q)[2] <- "rango_p5"
  rango_q$discrimina <- ifelse(rango_q$rango_p5 > 0, "si", "NO")
  rango_q$motivo <- ifelse(
    rango_q$rango_p5 > 0, "",
    ifelse(rango_q$consulta_id %in% q_sin_rel, "ningun relevante",
           "todas las estrategias empatan"))

  cat("\n--- Que consultas separan a las estrategias ---\n\n")
  print(rango_q, row.names = FALSE)
  cat(sprintf("\n%d de %d consultas producen diferencias entre estrategias.\n",
              sum(rango_q$rango_p5 > 0), nrow(rango_q)))

  # ==============================================================================
  # PODER DISCRIMINANTE DE LA EVALUACION
  # ==============================================================================

  cat("\n\n--- Saturacion del pool ---\n\n")
  sat <- aggregate(relevante ~ consulta_id, data = juicios,
                   FUN = function(x) c(n = length(x), rel = sum(x)))
  sat <- data.frame(consulta_id = sat$consulta_id,
                    juzgados    = sat$relevante[, "n"],
                    relevantes  = sat$relevante[, "rel"])
  sat$prop_relevante <- round(sat$relevantes / sat$juzgados, 2)
  print(sat, row.names = FALSE)
  cat(sprintf("\nGlobal: %.0f%% del pool es relevante.\n", 100 * mean(juicios$relevante)))
  if (mean(juicios$relevante) > 0.6) {
    cat("AVISO: por encima del 60% la metrica pierde resolucion. Considera un\n")
    cat("       criterio de relevancia mas estricto o consultas mas dificiles.\n")
  }

  # --- Solapamiento y correlacion de rangos entre estrategias -------------------

  cat("\n--- Solapamiento del top-10 entre estrategias ---\n\n")

  pares <- t(combn(ESTRATEGIAS, 2))
  filas_ov <- list()

  for (i in seq_len(nrow(CONSULTAS))) {
    qid  <- CONSULTAS$id[i]
    sub  <- resultados[resultados$consulta_id == qid, ]
    tops <- split(sub$doi[order(sub$posicion)], sub$estrategia[order(sub$posicion)])

    for (r in seq_len(nrow(pares))) {
      a <- pares[r, 1]; b <- pares[r, 2]
      A <- head(tops[[a]], 10); B <- head(tops[[b]], 10)
      if (is.null(A) || is.null(B) || length(A) == 0 || length(B) == 0) next

      jac <- length(intersect(A, B)) / length(union(A, B))

      # Kendall sobre la union: a lo no recuperado se le asigna el rango 11
      u  <- union(A, B)
      ra <- ifelse(is.na(match(u, A)), 11L, match(u, A))
      rb <- ifelse(is.na(match(u, B)), 11L, match(u, B))
      tau <- if (length(u) > 2 && sd(ra) > 0 && sd(rb) > 0) {
        suppressWarnings(cor(ra, rb, method = "kendall"))
      } else NA_real_

      filas_ov[[length(filas_ov) + 1]] <- data.frame(
        consulta_id = qid, estrategia_a = a, estrategia_b = b,
        jaccard_10 = round(jac, 3), kendall_tau = round(tau, 3),
        stringsAsFactors = FALSE)
    }
  }

  solapamiento <- do.call(rbind, filas_ov)
  write.csv(solapamiento, "evaluacion_solapamiento.csv", row.names = FALSE,
            fileEncoding = "UTF-8")

  resumen_ov <- aggregate(cbind(jaccard_10, kendall_tau) ~ estrategia_a + estrategia_b,
                          data = solapamiento, FUN = function(x) round(mean(x, na.rm = TRUE), 3))
  print(resumen_ov, row.names = FALSE)

  cat("\nJaccard 1.00 significa que las dos estrategias devolvieron el MISMO\n")
  cat("conjunto de documentos. Kendall alto con Jaccard alto significa que\n")
  cat("ademas los ordenaron igual: ahi el empate en P@5 es real, no un artefacto.\n")

  # ==============================================================================
  # BARRIDO DE k -- ELECCION DE LA DIMENSION POR RENDIMIENTO
  # ==============================================================================

  cat("\n\n--- Barrido de k (solo afecta a la estrategia LSA) ---\n\n")

  k_max <- min(max(REJILLA_K), nrow(idx$tfidf) - 2L, ncol(idx$tfidf) - 2L)
  cat(sprintf("Calculando SVD al k maximo (%d)...\n", k_max))
  sv <- RSpectra::svds(idx$tfidf, k = k_max)
  energia <- cumsum(sv$d^2) / sum(idx$tfidf@x^2)

  indice_con_k <- function(base, k) {
    V <- sv$v[, seq_len(k), drop = FALSE]
    D <- as.matrix(base$tfidf %*% V)
    nl <- sqrt(rowSums(D^2)); nl[nl == 0] <- 1
    base$V <- V
    base$docs_lsa <- D / nl
    base$parametros$k_final <- k
    base
  }

  filas_barrido <- list()
  for (k in REJILLA_K[REJILLA_K <= k_max]) {
    ik <- indice_con_k(idx, k)

    t0 <- Sys.time()
    m  <- evaluar_indice(ik, estrategias = c("lsa", "hibrido"), etiqueta = sprintf("k=%d", k))
    m  <- m[!(m$consulta_id %in% q_sin_rel), ]   
    ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000 / (nrow(CONSULTAS) * 2)

    agg <- aggregate(cbind(precision_5, precision_10, mrr, ndcg_5) ~ estrategia,
                     data = m, FUN = mean)

    filas_barrido[[length(filas_barrido) + 1]] <- data.frame(
      k            = k,
      energia      = round(100 * energia[k], 1),
      estrategia   = agg$estrategia,
      precision_5  = round(agg$precision_5, 4),
      precision_10 = round(agg$precision_10, 4),
      mrr          = round(agg$mrr, 4),
      ndcg_5       = round(agg$ndcg_5, 4),
      ms_consulta  = round(ms, 1),
      mb_matriz_V  = round(as.numeric(object.size(ik$V)) / 1024^2, 2),
      stringsAsFactors = FALSE
    )
    cat(sprintf("  k=%-4d energia %4.1f%%  P@5(lsa)=%.3f  %.1f ms  V=%.1f MB\n",
                k, 100 * energia[k],
                agg$precision_5[agg$estrategia == "lsa"], ms,
                as.numeric(object.size(ik$V)) / 1024^2))
  }

  barrido <- do.call(rbind, filas_barrido)
  write.csv(barrido, ARCH_BARRIDO, row.names = FALSE, fileEncoding = "UTF-8")

  solo_lsa <- barrido[barrido$estrategia == "lsa", ]
  mejor_p5 <- max(solo_lsa$precision_5)

  cat("\nCurva completa (Precision@5 de LSA por k):\n")
  print(solo_lsa[, c("k", "energia", "precision_5", "precision_10", "mb_matriz_V")],
        row.names = FALSE)


  k_elegido <- min(solo_lsa$k[solo_lsa$precision_5 >= mejor_p5])

  cat(sprintf("\nMejor Precision@5 en LSA: %.3f\n", mejor_p5))
  cat(sprintf("k recomendado           : %d  (el menor que alcanza ese maximo)\n", k_elegido))
  cat(sprintf("Energia en ese k        : %.1f%%\n", 100 * energia[k_elegido]))
  cat(sprintf("\nFija K_FORZADO <- %dL al inicio de pipeline.R y reconstruye.\n\n", k_elegido))

  cat(sprintf("Archivos escritos: %s, %s, %s, evaluacion_solapamiento.csv\n\n",
              ARCH_RESULT, ARCH_METRICAS, ARCH_BARRIDO))

  }
  invisible(TRUE)
}


# ==============================================================================
# ETAPA 5 -- PRUEBAS DE REGRESION DEL CORRECTOR
# ==============================================================================
probar_corrector <- function() {
  idx <- cargar_indice()
    if (is.null(idx)) stop("Falta search_index.rds. Ejecuta construir_indice() primero.")


  CASOS <- list(

    # --- REGRESIONES: bugs reales encontrados en uso -----------------------------
    list(q = "neural networks",  tipo = "intacta",
         nota = "BUG: 'neural' se convertia en 'natural' (empate a distancia 2)"),
    list(q = "natural language processing", tipo = "intacta",
         nota = "el reverso del anterior: no debe irse a 'neural'"),
    list(q = "general artificial intelligence", tipo = "intacta",
         nota = "'general' es clave del lexico y a la vez termino ingles"),
    list(q = "red teaming",      tipo = "intacta",
         nota = "'red' es clave ES ('network') pero tambien palabra inglesa"),
    list(q = "model compression", tipo = "intacta",
         nota = "'model' es clave ES y termino ingles frecuentisimo"),

    # --- Correcciones que SI deben ocurrir ---------------------------------------
    list(q = "macine learning",  tipo = "corrige", de = "macine",  a = "machine",
         nota = "BUG: corregia a 'main' por desempate arbitrario sobre stems"),
    list(q = "nueral netwoks",   tipo = "corrige", de = "netwoks", a = "networks",
         nota = "typo por omision"),
    list(q = "reinforcment learning", tipo = "corrige", de = "reinforcment",
         a = "reinforcement", nota = "typo por omision en palabra larga"),

    # --- Palabras pegadas ---------------------------------------------------------
    list(q = "neuralnetworks", tipo = "corrige", de = "neuralnetworks",
         a = "neural networks",
         nota = "compuesto sin espacio: debe segmentarse, no corregirse por distancia"),
    list(q = "machinelearning", tipo = "corrige", de = "machinelearning",
         a = "machine learning", nota = "idem"),
    list(q = "database systems", tipo = "intacta",
         nota = "'database' es termino real del corpus: NO debe partirse en data+base"),

    # --- Puente espanol -> ingles -------------------------------------------------
    list(q = "redes neuronales profundas", tipo = "traduce",
         nota = "los tres terminos son espanol puro, ninguno esta en el corpus"),
    list(q = "aprendizaje por refuerzo", tipo = "traduce",
         nota = "espanol puro"),

    # --- Casos que no deben romper ------------------------------------------------
    list(q = "ai",               tipo = "intacta", nota = "acronimo de dos letras"),
    list(q = "gpt-4",            tipo = "intacta", nota = "guion interno y digito"),
    list(q = "xyzqwk",           tipo = "sin_efecto",
         nota = "cadena sin sentido: no debe inventar una correccion plausible")
  )

  # ------------------------------------------------------------------------------
  cat("\n=== PRUEBAS DE REGRESION DEL CORRECTOR ===\n\n")

  pasan <- 0L
  fallan <- 0L

  for (caso in CASOS) {
    d   <- diagnosticar_consulta(caso$q, idx)
    cor <- if (is.null(d$correcciones)) character(0) else d$correcciones

    detalle <- if (length(cor) == 0) "sin correcciones" else
      paste(sprintf("%s -> %s", names(cor), unname(cor)), collapse = ", ")

    ok <- switch(caso$tipo,


      intacta = length(cor) == 0,


      corrige = caso$de %in% names(cor) && unname(cor[caso$de]) == caso$a,


      traduce = isTRUE(d$ok) && d$n_reconocidos > 0,

      sin_efecto = TRUE
    )

    if (ok) pasan <- pasan + 1L else fallan <- fallan + 1L

    cat(sprintf("[%s] %-34s %s\n", if (ok) "OK  " else "FALLA", sprintf('"%s"', caso$q), detalle))
    if (!ok) cat(sprintf("        esperado: %s | %s\n", caso$tipo, caso$nota))
  }

  cat(sprintf("\n%d pruebas: %d pasan, %d fallan\n\n", length(CASOS), pasan, fallan))

  if (fallan > 0) {
    cat("Hay regresiones. NO despliegues hasta resolverlas.\n\n")
  } else {
    cat("Sin regresiones.\n\n")
  }

  # ------------------------------------------------------------------------------
  # Inspeccion manual: que candidatos compiten para un token dado.
  # ------------------------------------------------------------------------------
  candidatos_de <- function(token, n = 8) {
    token <- tolower(token)
    en_corpus <- SnowballC::wordStem(token, language = "english") %in% idx$vocabulario

    cand <- idx$superficie[substr(idx$superficie, 1, 1) == substr(token, 1, 1) &
                             abs(nchar(idx$superficie) - nchar(token)) <= 2]
    if (length(cand) == 0) {
      cat(sprintf("'%s': sin candidatos cercanos\n", token))
      return(invisible(NULL))
    }

    d <- utils::adist(token, cand)[1, ]
    o <- order(d, -idx$superficie_df[match(cand, names(idx$superficie_df))])

    cat(sprintf("\n'%s'  |  en el corpus: %s\n", token, if (en_corpus) "SI (intocable)" else "no"))
    for (i in head(o, n)) {
      cat(sprintf("   %-18s distancia=%d  df=%d\n", cand[i], d[i],
                  idx$superficie_df[[cand[i]]]))
    }
    invisible(NULL)
  }

  cat("--- Por que 'neural' ya no se corrige ---\n")
  candidatos_de("neural")
  cat("\n--- Y por que 'macine' si ---\n")
  candidatos_de("macine")

  cat("\n--- Segmentacion de palabras pegadas ---\n")
  for (w in c("neuralnetworks", "machinelearning", "deeplearning", "database")) {
    en_corpus <- SnowballC::wordStem(w, language = "english") %in% idx$vocabulario
    partes <- if (en_corpus) NULL else
      .segmentar(w, idx$superficie, idx$superficie_df)
    cat(sprintf("  %-18s %s\n", w,
                if (en_corpus) "ya esta en el corpus: intocable"
                else if (is.null(partes)) "sin corte valido"
                else paste("->", paste(partes, collapse = " + "))))
  }

  cat("\n--- Conteo de palabras frente a terminos ---\n")
  for (q in c("neural networks", "deep learning models", "transformers")) {
    d <- diagnosticar_consulta(q, idx)
    cat(sprintf("  %-24s %s\n", sprintf('"%s"', q), d$mensaje))
  }
  cat("\n")
  invisible(TRUE)
}


# ==============================================================================
# EMPAQUETADO DE RESULTADOS
# ==============================================================================
empaquetar_resultados <- function(salida = "resultados.rds") {

  archivos <- c(
    metricas     = "evaluacion_metricas.csv",
    resultados   = "evaluacion_resultados.csv",
    solapamiento = "evaluacion_solapamiento.csv",
    barrido      = "barrido_k.csv",
    ablacion     = "ablacion_bigramas.csv",
    juicios      = "juicios.csv",
    vocab_zipf   = "vocab_zipf.csv",
    vocab_filt   = "vocab_filtros.csv",
    vocab_maxdf  = "vocab_eliminados_maxdf.csv"
  )

  paquete <- list()
  faltan  <- character(0)

  for (nom in names(archivos)) {
    f <- archivos[[nom]]
    if (file.exists(f)) {
      paquete[[nom]] <- .leer_csv(f)
    } else {
      faltan <- c(faltan, f)
    }
  }

  paquete$consultas  <- CONSULTAS
  paquete$parametros <- list(min_df = MIN_DF, max_df_prop = MAX_DF_PROP,
                             peso_titulo = PESO_TITULO,
                             peso_keywords = PESO_KEYWORDS,
                             bm25_k1 = BM25_K1, bm25_b = BM25_B,
                             k_forzado = K_FORZADO)
  paquete$generado <- Sys.time()

  saveRDS(paquete, salida, compress = "xz")

  cat(sprintf("\n%s escrito: %d tablas, %.2f MB\n",
              salida, length(paquete) - 3, file.size(salida) / 1024^2))
  if (length(faltan) > 0) {
    cat("  Faltaron (ejecuta las etapas correspondientes):",
        paste(faltan, collapse = ", "), "\n")
  }
  invisible(paquete)
}

# ==============================================================================
# DRIVER
# ==============================================================================
ejecutar_todo <- function(dependencias = TRUE) {

  if (dependencias) instalar_dependencias()

  construir_indice()
  analizar_vocabulario()
  probar_corrector()

  evaluar()

  if (file.exists("evaluacion_metricas.csv")) {
    empaquetar_resultados()
    cat("\nPipeline completo. Ya puedes tejer taller_4.Rmd.\n\n")
  } else {
    cat("\nFaltan juicios de relevancia. Llena juicios_plantilla.csv,",
        "guardalo como juicios.csv y vuelve a ejecutar.\n\n")
  }

  invisible(TRUE)
}
