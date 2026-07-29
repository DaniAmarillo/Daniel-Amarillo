# =============================================================================
# text_processing.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
# Normalización, tokenización y construcción de la matriz documento-término.
#
# El MISMO pipeline se aplica a documentos y a consultas: es la única forma de
# garantizar que la consulta caiga en el mismo espacio vectorial del corpus.
# =============================================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(SnowballC)
})

# --- Stopwords ---------------------------------------------------------------
# Lista Snowball para inglés (el corpus JMLR es 100 % anglófono) + un pequeño
# bloque de "stopwords de dominio": términos que aparecen en casi todos los
# abstracts académicos y no discriminan nada ("paper", "propose", "show"...).
STOPWORDS_EN <- c(
  "i","me","my","myself","we","our","ours","ourselves","you","your","yours",
  "yourself","yourselves","he","him","his","himself","she","her","hers",
  "herself","it","its","itself","they","them","their","theirs","themselves",
  "what","which","who","whom","this","that","these","those","am","is","are",
  "was","were","be","been","being","have","has","had","having","do","does",
  "did","doing","a","an","the","and","but","if","or","because","as","until",
  "while","of","at","by","for","with","about","against","between","into",
  "through","during","before","after","above","below","to","from","up","down",
  "in","out","on","off","over","under","again","further","then","once","here",
  "there","when","where","why","how","all","any","both","each","few","more",
  "most","other","some","such","no","nor","not","only","own","same","so",
  "than","too","very","s","t","can","will","just","don","should","now"
)

STOPWORDS_DOMINIO <- c(
  "paper", "papers", "articl", "articles", "propos", "propose", "proposed",
  "show", "shows", "shown", "present", "presents", "presented", "studi",
  "study", "studies", "result", "results", "approach", "approaches",
  "method", "methods", "methodology", "use", "used", "using", "also",
  "however", "moreover", "furthermore", "thus", "hence", "therefore",
  "give", "gives", "given", "consider", "considered", "based", "well",
  "new", "novel", "first", "second", "one", "two", "three", "may", "can",
  "obtain", "obtained", "provide", "provides", "provided"
)

# NOTA: STOPWORDS_DOMINIO se aplica DESPUÉS del stemming, por eso incluye
# formas ya truncadas ("studi", "articl"). Ver `tokenizar()`.


# --- 1. Normalización --------------------------------------------------------
#' Limpia una cadena de texto científico.
#'
#' Decisiones documentadas en §4.2 del informe:
#'  - Se eliminan las fórmulas LaTeX inline ($...$): en JMLR son frecuentes
#'    (p. ej. "$\\ell_0$-penalized") y al tokenizarse producen basura léxica
#'    ("ell", "0", "penal") que infla el vocabulario sin aportar semántica
#'    recuperable por un usuario que escribe en lenguaje natural.
#'  - Transliteración a ASCII: 37 abstracts contienen caracteres Unicode
#'    (nombres propios, guiones tipográficos, símbolos matemáticos).
#'  - Los guiones se convierten en espacio: "self-supervised" -> "self
#'    supervised". El bigrama posterior recupera el término compuesto sin
#'    duplicar entradas de vocabulario.
#'  - Se descartan los tokens puramente numéricos (años, índices, tamaños de
#'    muestra); se conservan los alfanuméricos con al menos una letra ("l1",
#'    "gpt2", "3d"), que sí son términos técnicos.
#'
#' REPRODUCIBILIDAD: la transliteración NO usa `iconv(..., "ASCII//TRANSLIT")`
#' porque su resultado depende del locale del sistema (en locale C descarta
#' caracteres que en un locale UTF-8 sí translitera). Eso hacía que el
#' vocabulario cambiara entre máquinas. Se usa un mapeo explícito con `chartr`,
#' que es determinista en cualquier plataforma.
ACENTOS_ORIGEN  <- "\u00e1\u00e0\u00e2\u00e4\u00e3\u00e5\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f6\u00f5\u00fa\u00f9\u00fb\u00fc\u00fd\u00ff\u00f1\u00e7\u00c1\u00c0\u00c2\u00c4\u00c3\u00c5\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf\u00d3\u00d2\u00d4\u00d6\u00d5\u00da\u00d9\u00db\u00dc\u00dd\u00d1\u00c7"
ACENTOS_DESTINO <- "aaaaaaeeeeiiiiooooouuuuyyncAAAAAAEEEEIIIIOOOOOUUUUYNC"

normalizar_texto <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- enc2utf8(as.character(x))
  x <- chartr(ACENTOS_ORIGEN, ACENTOS_DESTINO, x)
  x <- tolower(x)
  x <- gsub("\\$[^$]*\\$", " ", x)      # LaTeX inline
  x <- gsub("\\\\[a-z]+", " ", x)       # comandos LaTeX sueltos
  x <- gsub("[-_/]", " ", x)            # guiones y barras -> espacio
  x <- gsub("[^a-z0-9 ]", " ", x)       # resto de puntuación
  x <- gsub("\\s+", " ", x)
  trimws(x)
}


# --- 2. Tokenización ---------------------------------------------------------
#' Tokeniza un texto ya normalizado a unigramas + bigramas.
#'
#' Orden de operaciones: split -> filtro de longitud/numéricos -> stopwords
#' -> stemming (Porter) -> stopwords de dominio -> bigramas.
#'
#' Los bigramas se construyen DESPUÉS de quitar stopwords, de modo que
#' "learning from data" produce el bigrama "learn_data". Esto genera algunas
#' adyacencias artificiales, pero captura los términos compuestos que importan
#' en este corpus ("reinforc_learn", "neural_network", "causal_infer").
tokenizar <- function(txt, usar_stemming = TRUE, usar_bigramas = TRUE,
                      min_nchar = 2L) {
  tk <- strsplit(txt, " ", fixed = TRUE)[[1]]
  tk <- tk[nchar(tk) >= min_nchar]
  tk <- tk[grepl("[a-z]", tk)]                  # descarta números puros
  tk <- tk[!tk %in% STOPWORDS_EN]
  if (usar_stemming) tk <- SnowballC::wordStem(tk, language = "porter")
  tk <- tk[!tk %in% STOPWORDS_DOMINIO]
  tk <- tk[nchar(tk) >= min_nchar]
  if (usar_bigramas && length(tk) >= 2L) {
    bg <- paste(tk[-length(tk)], tk[-1], sep = "_")
    tk <- c(tk, bg)
  }
  tk
}

#' Aplica normalizar + tokenizar a un vector de textos.
procesar_corpus <- function(textos, ...) {
  lapply(normalizar_texto(textos), tokenizar, ...)
}


# --- 3. Matriz documento-término --------------------------------------------
#' Construye una DTM dispersa (dgCMatrix) de conteos.
#'
#' @param tokens lista de vectores de tokens (una entrada por documento)
#' @param min_df frecuencia documental mínima (entero, en n.º de documentos)
#' @param max_df_prop proporción documental máxima (términos más frecuentes
#'        que esto se descartan por no discriminar)
#' @return list(dtm, vocab)
construir_dtm <- function(tokens, min_df = 2L, max_df_prop = 0.60) {
  todos <- unlist(tokens, use.names = FALSE)
  vocab_full <- sort(unique(todos), method = "radix")

  # df: en cuántos documentos aparece cada término
  df_tab <- table(unlist(lapply(tokens, unique), use.names = FALSE))
  n_docs <- length(tokens)
  keep <- names(df_tab)[df_tab >= min_df & df_tab <= max_df_prop * n_docs]
  vocab <- sort(keep, method = "radix")   # radix = orden de bytes, independiente del locale
  pos <- setNames(seq_along(vocab), vocab)   # término -> columna

  i <- integer(0); j <- integer(0); v <- numeric(0)
  for (d in seq_along(tokens)) {
    tb <- table(tokens[[d]])
    p <- pos[names(tb)]
    ok <- !is.na(p)
    if (!any(ok)) next
    i <- c(i, rep.int(d, sum(ok)))
    j <- c(j, as.integer(p[ok]))
    v <- c(v, as.numeric(tb[ok]))
  }

  dtm <- Matrix::sparseMatrix(
    i = i, j = j, x = v,
    dims = c(n_docs, length(vocab)),
    dimnames = list(NULL, vocab)
  )
  list(dtm = dtm, vocab = vocab, vocab_full_size = length(vocab_full))
}

#' Vectoriza una consulta como vector de conteos alineado al vocabulario.
#' Los términos fuera de vocabulario (OOV) se descartan silenciosamente.
vectorizar_consulta <- function(q, vocab, ...) {
  tk <- tokenizar(normalizar_texto(q), ...)
  tb <- table(tk)
  p <- match(names(tb), vocab)
  ok <- !is.na(p)
  out <- Matrix::sparseVector(
    x = as.numeric(tb[ok]), i = as.integer(p[ok]), length = length(vocab)
  )
  list(vec = out, n_tokens = length(tk), n_oov = sum(!ok),
       terminos_oov = names(tb)[!ok])
}
