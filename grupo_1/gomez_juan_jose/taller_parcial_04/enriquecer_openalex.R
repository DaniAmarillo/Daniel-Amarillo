# =============================================================================
# enriquecer_openalex.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
#
# Rellena `citations`, `n_references` y `publication_date` en la base SQLite
# consultando la API pública de OpenAlex.
#
#     Rscript enriquecer_openalex.R              # ensayo, NO escribe nada
#     Rscript enriquecer_openalex.R --escribir   # escribe en la base
#
# CONTEXTO. En el Taller 1 (mayo de 2026) esta consulta devolvió 0 resultados:
# OpenAlex no había indexado el Vol. 26 de JMLR bajo el `source` canónico del
# journal. Este script conserva esa vía como ESTRATEGIA A y añade una
# ESTRATEGIA B que no se probó entonces:
#
#   A. Filtro por source     — `primary_location.source.id:S118988714`
#      Autoritativa y barata (2 peticiones). Es la que falló en mayo.
#
#   B. Búsqueda por título   — `title.search:<título normalizado>`
#      El Taller 1 verificó que los artículos SÍ están en OpenAlex, pero
#      alojados en repositorios secundarios (arXiv, repositorios
#      institucionales), no bajo el source de JMLR. Una búsqueda por título los
#      encuentra estén donde estén. Cuesta 1 petición por artículo (~1 min).
#
# El script ejecuta A; si la cobertura es incompleta, completa con B.
# No escribe nada si no hay coincidencias, y nunca sobrescribe valores que ya
# estén poblados (salvo con --forzar).
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(httr); library(jsonlite)
})

# --- Configuración -----------------------------------------------------------
MAILTO <- "jugomezgar@unal.edu.co"   # polite pool de OpenAlex: mejor rate-limit

# Clave de API. Desde febrero de 2025 OpenAlex limita a ~100 peticiones diarias
# sin clave, insuficiente para las 308 de la estrategia B. La clave es gratuita
# (https://openalex.org -> "Get an API key") y sube el limite a 100.000/dia.
# Se lee de la variable de entorno para NO dejarla escrita en GitHub:
#     Sys.setenv(OPENALEX_KEY = "tu_clave")     # en R, antes de correr
#     export OPENALEX_KEY=tu_clave              # en la terminal
API_KEY <- Sys.getenv("OPENALEX_KEY", "")
if (!nzchar(API_KEY))
  message("AVISO: sin OPENALEX_KEY. Puede que la estrategia B falle por limite\n",
          "       de peticiones. La estrategia A (2 peticiones) si funcionara.")
sufijo_auth <- function() paste0("&mailto=", MAILTO,
                                 if (nzchar(API_KEY)) paste0("&api_key=", API_KEY) else "")
SOURCE_ID <- "S118988714"            # JMLR según Wikidata
ANIO <- 2025
PAUSA <- 0.12                        # segundos entre peticiones
UMBRAL_JACCARD <- 0.80               # similitud mínima para aceptar un título

args      <- commandArgs(trailingOnly = TRUE)
ESCRIBIR  <- "--escribir" %in% args
FORZAR    <- "--forzar"   %in% args
SOLO_A    <- "--solo-a"   %in% args

raiz_proyecto <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f[1])))
  for (fr in rev(sys.frames())) if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  getwd()
}
DB_PATH <- file.path(raiz_proyecto(), "data", "jmlr_q1_2025.sqlite")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# --- Capa HTTP (aislada para poder probarla sin red) -------------------------
oa_get <- function(url, intentos = 3) {
  for (i in seq_len(intentos)) {
    r <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
    if (!is.null(r) && httr::status_code(r) == 200) {
      return(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"),
                                simplifyVector = FALSE))
    }
    if (!is.null(r) && httr::status_code(r) == 404) return(NULL)
    if (!is.null(r) && httr::status_code(r) %in% c(401L, 403L, 429L)) {
      message("HTTP ", httr::status_code(r),
              ": limite de peticiones o clave invalida. Define OPENALEX_KEY.")
      if (i == intentos) return(NULL)
    }
    Sys.sleep(1.5 * i)   # backoff ante 429/5xx
  }
  NULL
}

# --- Utilidades --------------------------------------------------------------
normalizar_titulo <- function(s) {
  # Misma normalización del Taller 1, para que los emparejamientos sean
  # comparables entre ambos talleres.
  s <- tolower(as.character(s))
  s <- gsub("[^a-z0-9 ]", " ", s)
  gsub("\\s+", " ", trimws(s))
}

jaccard <- function(a, b) {
  ta <- unique(strsplit(a, " ")[[1]]); tb <- unique(strsplit(b, " ")[[1]])
  if (!length(ta) || !length(tb)) return(0)
  length(intersect(ta, tb)) / length(union(ta, tb))
}

#' Aplana un registro `work` de OpenAlex a una fila.
aplanar_work <- function(w) {
  doi <- w$doi %||% NA_character_
  if (!is.na(doi)) doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi)
  data.frame(
    openalex_id      = w$id %||% NA_character_,
    doi_openalex     = doi,
    title_oa         = w$title %||% NA_character_,
    publication_date = w$publication_date %||% NA_character_,
    citations        = as.integer(w$cited_by_count %||% NA_integer_),
    n_references     = length(w$referenced_works %||% list()),
    stringsAsFactors = FALSE
  )
}


# --- ESTRATEGIA A: filtro por source ----------------------------------------
estrategia_A <- function() {
  cat("\n[A] Filtro por source ", SOURCE_ID, " + publication_year:", ANIO, "\n", sep = "")
  out <- list(); cursor <- "*"; pag <- 0
  repeat {
    pag <- pag + 1
    url <- paste0("https://api.openalex.org/works",
                  "?filter=primary_location.source.id:", SOURCE_ID,
                  ",publication_year:", ANIO,
                  "&per-page=200&cursor=", cursor, sufijo_auth())
    j <- oa_get(url)
    if (is.null(j)) { cat("    petición fallida\n"); break }
    n <- length(j$results %||% list())
    cat(sprintf("    página %d: %d registros (total declarado: %s)\n",
                pag, n, j$meta$count %||% "?"))
    if (n == 0) break
    out <- c(out, j$results)
    cursor <- j$meta$next_cursor %||% ""
    if (!nzchar(cursor)) break
    Sys.sleep(PAUSA)
  }
  if (!length(out)) return(NULL)
  do.call(rbind, lapply(out, aplanar_work))
}


# --- ESTRATEGIA B: búsqueda por título --------------------------------------
estrategia_B <- function(titulos, ids) {
  cat("\n[B] Búsqueda por título (", length(titulos), " peticiones, ~",
      round(length(titulos) * (PAUSA + 0.25) / 60, 1), " min)\n", sep = "")
  filas <- vector("list", length(titulos))
  n_ok <- 0
  for (i in seq_along(titulos)) {
    tn <- normalizar_titulo(titulos[i])
    q  <- utils::URLencode(tn, reserved = TRUE)
    url <- paste0("https://api.openalex.org/works?filter=title.search:", q,
                  "&per-page=5", sufijo_auth())
    j <- oa_get(url)
    Sys.sleep(PAUSA)
    res <- j$results %||% list()
    if (!length(res)) next

    # Verificación: se acepta la primera candidata cuyo título normalizado
    # coincida exactamente o supere el umbral de Jaccard. Sin esta guarda,
    # `title.search` devolvería el artículo "más parecido" aunque sea otro.
    for (w in res) {
      to <- normalizar_titulo(w$title %||% "")
      s  <- if (identical(to, tn)) 1 else jaccard(tn, to)
      if (s >= UMBRAL_JACCARD) {
        f <- aplanar_work(w); f$paper_id <- ids[i]; f$similitud <- round(s, 3)
        filas[[i]] <- f; n_ok <- n_ok + 1
        break
      }
    }
    if (i %% 25 == 0) cat(sprintf("    %d/%d consultados, %d emparejados\n",
                                  i, length(titulos), n_ok))
  }
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) return(NULL)
  do.call(rbind, filas)
}


# =============================================================================
# Ejecución
# =============================================================================
cat("=== Enriquecimiento con OpenAlex ===\n")
cat("Base:", DB_PATH, "\n")
cat("Modo:", if (ESCRIBIR) "ESCRITURA" else "ENSAYO (no se modifica la base)", "\n")

con <- dbConnect(SQLite(), DB_PATH)
papers <- dbGetQuery(con, "SELECT paper_id, title, year, citations, n_references,
                           publication_date FROM papers ORDER BY paper_num")
cat("Artículos en la base:", nrow(papers), "\n")
cat("Ya tienen citas:", sum(!is.na(papers$citations)), "\n")

papers$title_norm <- normalizar_titulo(papers$title)

# --- A ---
enr <- NULL
dfA <- estrategia_A()
if (!is.null(dfA)) {
  dfA$title_norm <- normalizar_titulo(dfA$title_oa)
  m <- match(papers$title_norm, dfA$title_norm)
  ok <- !is.na(m)
  cat("    emparejados por título exacto:", sum(ok), "de", nrow(papers), "\n")
  if (any(ok)) {
    enr <- dfA[m[ok], c("openalex_id","doi_openalex","publication_date",
                        "citations","n_references")]
    enr$paper_id <- papers$paper_id[ok]
    enr$similitud <- 1
  }
} else {
  cat("    sin resultados (igual que en el Taller 1)\n")
}

# --- B, solo para lo que falte ---
if (!SOLO_A) {
  pendientes <- if (is.null(enr)) seq_len(nrow(papers))
                else which(!papers$paper_id %in% enr$paper_id)
  if (length(pendientes)) {
    dfB <- estrategia_B(papers$title[pendientes], papers$paper_id[pendientes])
    if (!is.null(dfB)) {
      cols <- c("paper_id","openalex_id","doi_openalex","publication_date",
                "citations","n_references","similitud")
      enr <- rbind(enr[, cols[cols %in% names(enr)]] , dfB[, cols])
    }
  }
}

# --- Informe ---
cat("\n=== Resultado ===\n")
if (is.null(enr) || nrow(enr) == 0) {
  cat("Cobertura: 0 de ", nrow(papers), " artículos.\n",
      "OpenAlex sigue sin indexar este volumen de forma recuperable.\n",
      "La base NO se modifica: el N/D del dashboard es correcto y está\n",
      "justificado en la sección 6.2 del enunciado del Taller 1.\n", sep = "")
  dbDisconnect(con); quit(save = "no", status = 0)
}

cat(sprintf("Cobertura: %d de %d (%.1f %%)\n", nrow(enr), nrow(papers),
            100 * nrow(enr) / nrow(papers)))
cat(sprintf("Citas    : media %.2f, máx %d, cero citas %d\n",
            mean(enr$citations, na.rm = TRUE), max(enr$citations, na.rm = TRUE),
            sum(enr$citations == 0, na.rm = TRUE)))
cat(sprintf("Refs     : media %.1f, sin referencias %d\n",
            mean(enr$n_references, na.rm = TRUE), sum(enr$n_references == 0)))
cat(sprintf("DOI real recuperado: %d\n", sum(!is.na(enr$doi_openalex))))
cat(sprintf("Emparejamientos no exactos (Jaccard < 1): %d  <-- REVISAR\n",
            sum(enr$similitud < 1)))

if (any(enr$similitud < 1)) {
  cat("\nEmparejamientos aproximados:\n")
  d <- enr[enr$similitud < 1, ]
  for (i in seq_len(min(10, nrow(d)))) {
    j <- match(d$paper_id[i], papers$paper_id)
    cat(sprintf("  [%.2f] JMLR: %s\n         OA  : %s\n",
                d$similitud[i], substr(papers$title[j], 1, 70),
                substr(d$openalex_id[i], 1, 70)))
  }
}

# --- Escritura ---
if (!ESCRIBIR) {
  cat("\nEnsayo terminado. Para aplicar los cambios:\n",
      "    Rscript enriquecer_openalex.R --escribir\n", sep = "")
  dbDisconnect(con); quit(save = "no", status = 0)
}

# Columnas de trazabilidad (idempotente)
for (col in c("openalex_id TEXT", "doi_openalex TEXT")) {
  try(dbExecute(con, paste("ALTER TABLE papers ADD COLUMN", col)), silent = TRUE)
}

cond <- if (FORZAR) "" else " AND (citations IS NULL)"
n <- 0
dbBegin(con)
for (i in seq_len(nrow(enr))) {
  n <- n + dbExecute(con, paste0(
    "UPDATE papers SET citations = ?, n_references = ?,
       publication_date = COALESCE(publication_date, ?),
       openalex_id = ?, doi_openalex = ?
     WHERE paper_id = ?", cond),
    params = list(enr$citations[i], enr$n_references[i],
                  enr$publication_date[i], enr$openalex_id[i],
                  enr$doi_openalex[i], enr$paper_id[i]))
}
dbCommit(con)

cat("\nFilas actualizadas:", n, "\n")
cat("Verificación:\n")
print(dbGetQuery(con, "SELECT COUNT(*) AS n, COUNT(citations) AS con_citas,
                       ROUND(AVG(citations),2) AS media_citas,
                       ROUND(AVG(n_references),1) AS media_refs FROM papers"))
dbDisconnect(con)

cat("\nListo. El dashboard mostrará las cifras al reiniciar la app.\n")
cat("NOTA: si vas a recompilar taller_4.Rmd, hazlo después de esto para que\n")
cat("      el conteo de valores faltantes del informe quede consistente.\n")
