# -*- coding: utf-8 -*-
"""
ir.py — Módulo de recuperación de información del Taller 4.

Contiene TODA la lógica del buscador, de modo que el notebook
(`taller_4.ipynb`), el script de precómputo (`build_index.py`) y la aplicación
(`app.py`) usen exactamente el mismo código. Así se garantiza que lo
documentado sea lo que corre en la app.

Contenido:
  * Construcción del corpus a partir de la base SQLite del Taller 1.
  * Normalización y tokenización del texto (analizador único y reutilizable).
  * Estrategia A — BM25 Okapi sobre matriz dispersa de frecuencias (léxica).
  * Estrategia B — TF-IDF + Truncated SVD (LSA) con similitud coseno
    (semántica, sobre representación reducida).
  * Estrategia C — TF-IDF completo + coseno (referencia para medir el efecto
    de la reducción dimensional).
  * Estrategia D — Híbrido BM25 + LSA fusionado con Reciprocal Rank Fusion.
  * Construcción, serialización y carga del índice.
  * Función de búsqueda que devuelve un DataFrame listo para mostrar.

Requiere: pandas, numpy, scipy, scikit-learn, joblib.
"""

from __future__ import annotations

import re
import time
import unicodedata
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
import scipy.sparse as sp
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import (
    ENGLISH_STOP_WORDS,
    CountVectorizer,
    TfidfVectorizer,
)
from sklearn.preprocessing import normalize

# ─────────────────────────────────────────────────────────────────────────────
#  Parámetros del sistema (únicos y compartidos)
# ─────────────────────────────────────────────────────────────────────────────
INDEX_PATH = Path(__file__).parent / "index" / "ir_index.joblib"

PESO_TITULO = 3          # el título se repite 3 veces en el documento virtual
BM25_K1 = 1.5            # saturación de la frecuencia de término
BM25_B = 0.75            # normalización por longitud del documento
SVD_COMPONENTS = 40      # dimensión del espacio latente (ver notebook §5)
SVD_SEED = 42
RRF_K = 60               # constante de amortiguación de Reciprocal Rank Fusion

TFIDF_MIN_DF = 2         # un término debe aparecer en ≥ 2 artículos
TFIDF_MAX_DF = 0.5       # y en ≤ 50 % de los artículos
NGRAM_MAX = 2            # unigramas y bigramas para TF-IDF/LSA

METODOS = {
    "bm25": "BM25 (léxica)",
    "lsa": "LSA / TF-IDF + SVD (semántica, reducida)",
    "tfidf": "TF-IDF completo + coseno (léxica vectorial)",
    "hibrido": "Híbrido BM25 + LSA (RRF)",
}

# Stopwords: lista inglesa de scikit-learn + términos de relleno académico que
# aparecen en casi todos los abstracts y no discriminan nada.
STOPWORDS_EXTRA = {
    "paper", "papers", "method", "methods", "methodology", "approach",
    "approaches", "novel", "present", "presents", "presented", "propose",
    "proposed", "proposes", "result", "results", "using", "use", "used",
    "uses", "based", "new", "work", "works", "show", "shows", "shown",
    "demonstrate", "demonstrates", "state", "art", "introduce", "introduces",
    "allow", "allows", "provide", "provides", "however", "thus", "also",
    "furthermore", "moreover", "additionally", "finally", "well", "make",
    "makes", "given", "different", "various", "several", "many", "wide",
    "widely", "compared", "comparison", "code", "available",
}
STOPWORDS = frozenset(ENGLISH_STOP_WORDS) | STOPWORDS_EXTRA

_RE_NO_ALFANUM = re.compile(r"[^a-z0-9\-]+")
_RE_GUIONES = re.compile(r"-{2,}")


# ─────────────────────────────────────────────────────────────────────────────
#  1 · Procesamiento del texto
# ─────────────────────────────────────────────────────────────────────────────
def normalizar(texto) -> str:
    """
    Normaliza una cadena antes de tokenizar:
      * NFKD + descarte de diacríticos (los nombres propios y algunos títulos
        traen acentos: «Bézier», «Poisson», «Möbius»).
      * minúsculas;
      * todo lo que no sea [a-z0-9-] pasa a espacio, de modo que la puntuación
        desaparece pero se conserva el guion interno de los términos técnicos
        («real-time», «out-of-core», «signed-distance»).
    """
    if texto is None:
        return ""
    t = unicodedata.normalize("NFKD", str(texto))
    t = t.encode("ascii", "ignore").decode("ascii").lower()
    t = _RE_NO_ALFANUM.sub(" ", t)
    return _RE_GUIONES.sub("-", t)


def tokenizar(texto) -> list[str]:
    """
    Tokenización por espacios sobre el texto normalizado, descartando:
      * tokens de menos de 3 caracteres (ruido: «a», «of», iniciales);
      * stopwords (inglesas + relleno académico);
      * tokens puramente numéricos (años, números de ecuación, tamaños).

    NO se aplica stemming ni lematización; la justificación está en el
    notebook (§4.2): en el vocabulario de gráficos por computador el stemming
    fusiona términos que designan objetos distintos y la variación morfológica
    la absorbe el espacio latente de la estrategia B.
    """
    out = []
    for w in normalizar(texto).split():
        w = w.strip("-")
        if len(w) < 3 or w in STOPWORDS or w.isdigit():
            continue
        out.append(w)
    return out


def analizador_ngramas(texto) -> list[str]:
    """Unigramas + n-gramas hasta NGRAM_MAX sobre los tokens de `tokenizar`."""
    toks = tokenizar(texto)
    grams = list(toks)
    for n in range(2, NGRAM_MAX + 1):
        grams.extend(
            " ".join(toks[i:i + n]) for i in range(len(toks) - n + 1)
        )
    return grams


# ─────────────────────────────────────────────────────────────────────────────
#  2 · Construcción del corpus
# ─────────────────────────────────────────────────────────────────────────────
CAMPOS_META = [
    "paper_id", "title", "authors_raw", "publication_date", "year",
    "doi", "url", "abstract", "topic_label", "citations", "downloads",
]


def construir_corpus(db_path: str) -> pd.DataFrame:
    """
    Lee `papers` de la base SQLite y arma el documento virtual de cada
    artículo: título repetido PESO_TITULO veces + abstract.

    La base del Taller 1 no contiene palabras clave (el scraper de ACM no
    capturó los CCS tags), de modo que el texto disponible es título +
    abstract. No se inventa contenido para los campos ausentes.

    Devuelve un DataFrame con los metadatos y la columna `doc_text`.
    """
    import sqlite3

    con = sqlite3.connect(db_path, check_same_thread=False)
    try:
        df = pd.read_sql("SELECT * FROM papers", con)
    finally:
        con.close()

    for c in CAMPOS_META:
        if c not in df.columns:
            df[c] = None

    titulo = df["title"].fillna("").astype(str).str.strip()
    resumen = df["abstract"].fillna("").astype(str).str.strip()

    # Documento virtual: el título pesa PESO_TITULO veces frente al abstract.
    df["doc_text"] = ((titulo + " . ") * PESO_TITULO) + resumen

    # Criterio de inclusión: debe existir texto útil (título o abstract).
    df["texto_util"] = df["doc_text"].str.strip().str.len() > 0
    df = df[df["texto_util"]].drop(columns="texto_util").reset_index(drop=True)
    return df


def diagnostico_corpus(df: pd.DataFrame) -> pd.DataFrame:
    """Tabla de diagnóstico del corpus para documentar §4.1 del enunciado."""
    def vacio(col):
        if col not in df.columns:
            return len(df)
        s = df[col].fillna("").astype(str).str.strip().str.lower()
        return int(s.isin(["", "nan", "none", "n/a"]).sum())

    n_tokens = df["doc_text"].map(lambda t: len(tokenizar(t)))
    return pd.DataFrame(
        {
            "indicador": [
                "artículos en la base",
                "artículos incluidos en el índice",
                "sin título",
                "sin resumen (abstract)",
                "sin palabras clave (campo inexistente en la BD)",
                "sin DOI",
                "sin fecha de publicación",
                "tokens por documento (mediana)",
                "tokens por documento (mínimo)",
                "tokens por documento (máximo)",
            ],
            "valor": [
                len(df), len(df),
                vacio("title"), vacio("abstract"), len(df),
                vacio("doi"), vacio("publication_date"),
                int(n_tokens.median()), int(n_tokens.min()), int(n_tokens.max()),
            ],
        }
    )


# ─────────────────────────────────────────────────────────────────────────────
#  3 · Estrategia A — BM25 Okapi
# ─────────────────────────────────────────────────────────────────────────────
def matriz_bm25(C: sp.csr_matrix, k1: float = BM25_K1, b: float = BM25_B):
    """
    Precalcula la matriz de pesos BM25 W (misma dispersión que C) y el idf.

        idf_j = ln( 1 + (N - df_j + 0.5) / (df_j + 0.5) )

        W_ij  = idf_j · tf_ij (k1 + 1) / ( tf_ij + k1 (1 - b + b · dl_i/avgdl) )

    Con W precalculada, puntuar una consulta cuesta una suma de columnas:
    score_i = Σ_{j ∈ q} qtf_j · W_ij. No hay ningún cálculo pesado en la app.
    """
    C = C.tocsr().astype(np.float64)
    N = C.shape[0]
    df_t = np.asarray((C > 0).sum(axis=0)).ravel()
    idf = np.log1p((N - df_t + 0.5) / (df_t + 0.5))

    dl = np.asarray(C.sum(axis=1)).ravel()
    avgdl = dl.mean() if N else 0.0
    denom_doc = k1 * (1.0 - b + b * (dl / avgdl if avgdl else dl))

    W = C.tocoo()
    tf = W.data
    datos = tf * (k1 + 1.0) / (tf + denom_doc[W.row])
    datos = datos * idf[W.col]
    W = sp.csr_matrix((datos, (W.row, W.col)), shape=C.shape)
    return W, idf, df_t, dl, float(avgdl)


def puntajes_bm25(idx, consulta: str) -> np.ndarray:
    """Puntaje BM25 de todos los documentos para `consulta`."""
    vocab = idx["bm25_vocab"]
    W = idx["bm25_W"]
    cuenta: dict[int, float] = {}
    for t in tokenizar(consulta):
        j = vocab.get(t)
        if j is not None:
            cuenta[j] = cuenta.get(j, 0.0) + 1.0
    if not cuenta:
        return np.zeros(W.shape[0])
    cols = np.fromiter(cuenta.keys(), dtype=np.int64)
    qtf = np.fromiter(cuenta.values(), dtype=np.float64)
    return np.asarray(W[:, cols].multiply(qtf).sum(axis=1)).ravel()


# ─────────────────────────────────────────────────────────────────────────────
#  4 · Estrategias B y C — TF-IDF completo y TF-IDF reducido (LSA)
# ─────────────────────────────────────────────────────────────────────────────
def vector_consulta_tfidf(idx, consulta: str) -> sp.csr_matrix:
    """Proyecta la consulta al espacio TF-IDF de los artículos (mismo idf)."""
    return idx["tfidf"].transform([consulta])


def puntajes_tfidf(idx, consulta: str) -> np.ndarray:
    """Coseno en el espacio TF-IDF completo (ambos vectores están l2-normados)."""
    q = vector_consulta_tfidf(idx, consulta)
    if q.nnz == 0:
        return np.zeros(idx["X"].shape[0])
    return np.asarray(idx["X"] @ q.T.todense()).ravel()


def proyectar_consulta_lsa(idx, consulta: str) -> np.ndarray:
    """
    Proyecta la consulta al MISMO espacio latente de los artículos:
        q_tfidf  = tfidf.transform(q)          (mismo vocabulario e idf)
        q_latente = q_tfidf · V                (svd.transform)
        q_latente = q_latente / ||q_latente||  (para que el producto sea coseno)
    """
    q = vector_consulta_tfidf(idx, consulta)
    if q.nnz == 0:
        return np.zeros(idx["svd"].n_components, dtype=np.float32)
    z = idx["svd"].transform(q)
    n = np.linalg.norm(z)
    if n > 0:
        z = z / n
    return z.astype(np.float32).ravel()


def puntajes_lsa(idx, consulta: str) -> np.ndarray:
    """Coseno en el espacio latente reducido (matriz Z ya l2-normada)."""
    z = proyectar_consulta_lsa(idx, consulta)
    if not np.any(z):
        return np.zeros(idx["Z"].shape[0])
    return idx["Z"] @ z


# ─────────────────────────────────────────────────────────────────────────────
#  5 · Estrategia D — fusión híbrida (Reciprocal Rank Fusion)
# ─────────────────────────────────────────────────────────────────────────────
def rangos(puntajes: np.ndarray) -> np.ndarray:
    """
    Posición (1 = mejor) de cada documento. Los documentos con puntaje 0 se
    mandan al final. Empates: se resuelven por índice, de forma determinista.
    """
    orden = np.lexsort((np.arange(len(puntajes)), -puntajes))
    r = np.empty(len(puntajes), dtype=np.int64)
    r[orden] = np.arange(1, len(puntajes) + 1)
    return r


def puntajes_rrf(idx, consulta: str, k: int = RRF_K) -> np.ndarray:
    """
    RRF sobre BM25 y LSA:  score_i = Σ_m 1 / (k + rank_m(i)).

    Ventaja: no requiere que las escalas de BM25 (no acotada) y del coseno
    latente ([-1, 1]) sean comparables, porque solo usa las posiciones.
    """
    s_bm = puntajes_bm25(idx, consulta)
    s_ls = puntajes_lsa(idx, consulta)
    total = np.zeros(len(s_bm))
    for s in (s_bm, s_ls):
        if np.any(s):
            total += 1.0 / (k + rangos(s))
    return total


PUNTUADORES = {
    "bm25": puntajes_bm25,
    "lsa": puntajes_lsa,
    "tfidf": puntajes_tfidf,
    "hibrido": puntajes_rrf,
}


# ─────────────────────────────────────────────────────────────────────────────
#  6 · Construcción y persistencia del índice
# ─────────────────────────────────────────────────────────────────────────────
def construir_indice(db_path: str, n_components: int = SVD_COMPONENTS) -> dict:
    """
    Construye el índice completo a partir de la base SQLite.

    Devuelve un diccionario con los vectorizadores entrenados, la matriz
    dispersa TF-IDF, la matriz de pesos BM25, el modelo SVD, la matriz latente
    y los metadatos necesarios para mostrar resultados.
    """
    t0 = time.perf_counter()
    df = construir_corpus(db_path)
    textos = df["doc_text"].tolist()

    # ── Estrategia A: frecuencias crudas, unigramas, sin recorte de vocabulario
    #    (BM25 debe poder emparejar términos raros, que son los más informativos)
    cv = CountVectorizer(analyzer=tokenizar, min_df=1)
    C = cv.fit_transform(textos)
    W, idf_bm25, df_t, dl, avgdl = matriz_bm25(C)

    # ── Estrategias B y C: TF-IDF con unigramas + bigramas y recorte de vocabulario
    tv = TfidfVectorizer(
        analyzer=analizador_ngramas,
        min_df=TFIDF_MIN_DF,
        max_df=TFIDF_MAX_DF,
        sublinear_tf=True,
        norm="l2",
    )
    X = tv.fit_transform(textos)

    # ── Reducción dimensional: Truncated SVD directamente sobre la matriz dispersa
    n_components = int(min(n_components, min(X.shape) - 1))
    svd = TruncatedSVD(n_components=n_components, random_state=SVD_SEED,
                       algorithm="randomized", n_iter=10)
    Z = svd.fit_transform(X)
    Z = normalize(Z).astype(np.float32)      # l2 → el producto punto es coseno

    idx = {
        "meta": df[CAMPOS_META].copy(),
        "doc_text": df["doc_text"].tolist(),
        "bm25_vocab": {t: int(j) for t, j in cv.vocabulary_.items()},
        "bm25_W": W.astype(np.float32),
        "bm25_idf": idf_bm25,
        "bm25_df": df_t,
        "bm25_dl": dl,
        "bm25_avgdl": avgdl,
        "tfidf": tv,
        "X": X,
        "svd": svd,
        "Z": Z,
        "params": {
            "peso_titulo": PESO_TITULO,
            "bm25_k1": BM25_K1,
            "bm25_b": BM25_B,
            "svd_components": n_components,
            "svd_seed": SVD_SEED,
            "rrf_k": RRF_K,
            "tfidf_min_df": TFIDF_MIN_DF,
            "tfidf_max_df": TFIDF_MAX_DF,
            "ngram_max": NGRAM_MAX,
        },
        "info": {
            "n_docs": int(X.shape[0]),
            "vocab_bm25": int(C.shape[1]),
            "vocab_tfidf": int(X.shape[1]),
            "dim_original": int(X.shape[1]),
            "dim_reducida": int(n_components),
            "varianza_explicada": float(svd.explained_variance_ratio_.sum()),
            "nnz_tfidf": int(X.nnz),
            "dispersion_tfidf": float(1 - X.nnz / (X.shape[0] * X.shape[1])),
            "segundos_construccion": round(time.perf_counter() - t0, 2),
        },
    }
    return idx


def guardar_indice(idx: dict, ruta: str | Path = INDEX_PATH) -> Path:
    ruta = Path(ruta)
    ruta.parent.mkdir(parents=True, exist_ok=True)
    joblib.dump(idx, ruta, compress=3)
    return ruta


def cargar_indice(ruta: str | Path = INDEX_PATH) -> dict:
    ruta = Path(ruta)
    if not ruta.exists():
        raise FileNotFoundError(
            f"No existe el índice {ruta}. Ejecuta primero: python build_index.py"
        )
    return joblib.load(ruta)


# ─────────────────────────────────────────────────────────────────────────────
#  7 · Búsqueda
# ─────────────────────────────────────────────────────────────────────────────
def fragmento(texto: str, consulta: str, ancho: int = 300) -> str:
    """
    Fragmento del abstract centrado en la primera aparición de un término de
    la consulta. Si ningún término aparece (caso típico de la estrategia
    semántica), devuelve el inicio del abstract.
    """
    texto = str(texto or "").strip()
    if not texto:
        return ""
    terminos = [t for t in tokenizar(consulta) if len(t) >= 4]
    bajo = texto.lower()
    pos = -1
    for t in terminos:
        p = bajo.find(t)
        if p != -1 and (pos == -1 or p < pos):
            pos = p
    if pos == -1:
        frag = texto[:ancho]
        return frag + ("…" if len(texto) > ancho else "")
    ini = max(0, pos - ancho // 3)
    fin = min(len(texto), ini + ancho)
    frag = texto[ini:fin]
    return ("…" if ini > 0 else "") + frag + ("…" if fin < len(texto) else "")


def buscar(idx: dict, consulta: str, metodo: str = "bm25", top_k: int = 10,
           con_fragmento: bool = True) -> pd.DataFrame:
    """
    Ejecuta una consulta y devuelve los `top_k` artículos más relevantes.

    Ranking: puntaje descendente; los empates se rompen por número de citas y
    luego por `paper_id`, de forma determinista y documentada. Los documentos
    con puntaje ≤ 0 se descartan (no aportan evidencia de relevancia).
    """
    metodo = metodo if metodo in PUNTUADORES else "bm25"
    if not str(consulta).strip():
        return pd.DataFrame()

    t0 = time.perf_counter()
    s = PUNTUADORES[metodo](idx, consulta)
    ms = (time.perf_counter() - t0) * 1000

    meta = idx["meta"]
    res = meta.copy()
    res["puntaje"] = s
    res = res[res["puntaje"] > 0]
    if res.empty:
        out = pd.DataFrame(columns=list(meta.columns) + ["puntaje", "posicion"])
        out.attrs["ms"] = ms
        out.attrs["metodo"] = metodo
        return out

    res = res.sort_values(
        ["puntaje", "citations", "paper_id"], ascending=[False, False, True]
    ).head(top_k).reset_index(drop=True)
    res.insert(0, "posicion", np.arange(1, len(res) + 1))

    if con_fragmento:
        res["fragmento"] = [
            fragmento(a, consulta) for a in res["abstract"].fillna("")
        ]
    res.attrs["ms"] = ms
    res.attrs["metodo"] = metodo
    return res
