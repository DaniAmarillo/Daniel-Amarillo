"""
Taller 4 - Mineria de Datos (2016325)
Modulo del sistema de recuperacion de informacion.

Este archivo concentra TODA la logica del buscador: procesamiento del texto,
representacion vectorial, reduccion de dimensionalidad, recuperacion, ranking
y evaluacion. Lo importan tanto el notebook `taller_4.ipynb` como la
aplicacion `app.py`, de modo que el documento reproducible y la app usan
exactamente el mismo codigo.


"""

# CONFIGURACION Y RUTAS

import os
import re
import time
import unicodedata

import joblib
import numpy as np
import pandas as pd
import sqlite3

from nltk.stem import SnowballStemmer
from rank_bm25 import BM25Okapi
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS, TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.preprocessing import Normalizer

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DB_PATH = os.path.join(BASE_DIR, "sqlite_webscraping.db")
ARTEFACTOS_DIR = os.path.join(BASE_DIR, "artefactos")
ARTEFACTOS_PATH = os.path.join(ARTEFACTOS_DIR, "buscador.joblib")

# Numero de componentes latentes de LSA. 
N_COMPONENTES_LSA = 50

# El titulo se repite para que pese mas que el resumen: es el campo mas
# informativo y el mas corto.
PESO_TITULO = 3

SEMILLA = 42


# CARGA DEL CORPUS DESDE SQLITE

CAMPOS_CORPUS = ["title", "abstract", "topic_label"]


def cargar_corpus(db_path: str = DB_PATH) -> pd.DataFrame:
    """Lee la tabla `papers` y arma el campo de texto que indexa el buscador.

    Devuelve el DataFrame con una columna adicional `texto_corpus`, que es la
    concatenacion de titulo (repetido PESO_TITULO veces), resumen y tema.
    """
    conn = sqlite3.connect(db_path)
    try:
        df = pd.read_sql_query(
            """
            SELECT paper_id, title, abstract, authors_raw, doi, url,
                   publication_date, year, topic_label, citations, views,
                   n_authors, n_references
            FROM papers
            ORDER BY paper_id
            """,
            conn,
        )
    finally:
        conn.close()

    for col in CAMPOS_CORPUS:
        df[col] = df[col].fillna("").astype(str).str.strip()

    df["publication_date"] = (
        df["publication_date"].astype(str)
        .str.replace(r"\s\d{2}:\d{2}:\d{2}$", "", regex=True)
        .replace({"None": "", "nan": "", "NaT": ""})
    )

    df["texto_corpus"] = (
        (df["title"] + " ") * PESO_TITULO + df["abstract"] + " " + df["topic_label"]
    ).str.strip()

    # Criterio de inclusion: se indexa el articulo solo si tiene texto util.
    df = df[df["texto_corpus"].str.split().str.len() >= 5].reset_index(drop=True)
    return df


def diagnostico_corpus(db_path: str = DB_PATH) -> pd.DataFrame:
    """Tabla de faltantes por campo, para documentar la seccion 4.1."""
    conn = sqlite3.connect(db_path)
    try:
        df = pd.read_sql_query("SELECT * FROM papers", conn)
    finally:
        conn.close()

    filas = []
    for col in ["title", "abstract", "topic_label", "doi", "authors_raw"]:
        serie = df[col].fillna("").astype(str).str.strip()
        filas.append(
            {
                "campo": col,
                "n_nulos_sql": int(df[col].isna().sum()),
                "n_vacios": int((serie == "").sum()),
                "n_disponibles": int((serie != "").sum()),
                "long_media_palabras": round(serie.str.split().str.len().mean(), 1),
            }
        )
    filas.append(
        {
            "campo": "keywords",
            "n_nulos_sql": len(df),
            "n_vacios": len(df),
            "n_disponibles": 0,
            "long_media_palabras": 0.0,
        }
    )
    return pd.DataFrame(filas)


# PROCESAMIENTO DEL TEXTO

# Decisiones :
#   - minusculas y normalizacion Unicode: unifica variantes tipograficas
#   - solo tokens alfabeticos de 3+ letras: descarta numeros, anios y ruido
#   - stopwords: lista de sklearn + stopwords academicas del dominio
#   - stemming (Snowball) en vez de lematizacion: no requiere descargar
#     modelos, y en ingles academico basta para unir plural/singular y
#     familias como pricing / priced / prices
#   - se conservan bigramas en la vectorizacion (no aqui) para capturar
#     terminos compuestos como "asset pricing" o "monetary policy"

_STEMMER = SnowballStemmer("english")

# Palabras muy frecuentes en resumenes academicos que no discriminan entre
# articulos: son stopwords "del corpus" (clase 23, filtrado por max_df).
STOPWORDS_DOMINIO = {
    "paper", "papers", "study", "studies", "article", "articles", "research",
    "evidence", "find", "finds", "finding", "findings", "show", "shows",
    "result", "results", "using", "used", "use", "uses", "based", "data",
    "analysis", "approach", "model", "models", "method", "methods", "new",
    "however", "also", "we", "our", "this", "these", "abstract", "journal",
    "finance", "financial",
}

STOPWORDS = set(ENGLISH_STOP_WORDS) | STOPWORDS_DOMINIO

_PATRON_TOKEN = re.compile(r"[a-z]{3,}")


def normalizar(texto: str) -> str:
    """Minusculas, normalizacion Unicode NFKD y limpieza de simbolos."""
    texto = str(texto).lower()
    texto = unicodedata.normalize("NFKD", texto)
    texto = "".join(c for c in texto if not unicodedata.combining(c))
    texto = texto.replace("&amp;", " ").replace("\u2010", "-").replace("\u2011", "-")
    texto = re.sub(r"[^a-z\s-]", " ", texto)
    texto = texto.replace("-", " ")
    return re.sub(r"\s{2,}", " ", texto).strip()


def tokenizar(texto: str) -> list:
    """Pipeline completo: normaliza, tokeniza, quita stopwords y aplica stem.

    Es una funcion de nivel de modulo (no un lambda) para que el
    TfidfVectorizer que la usa se pueda serializar con joblib.
    """
    texto = normalizar(texto)
    tokens = _PATRON_TOKEN.findall(texto)
    tokens = [t for t in tokens if t not in STOPWORDS]
    tokens = [_STEMMER.stem(t) for t in tokens]
    return [t for t in tokens if len(t) >= 3 and t not in STOPWORDS]


#  REPRESENTACION VECTORIAL (TF-IDF)


def construir_tfidf(textos, min_df: int = 2, max_df: float = 0.5,
                    ngram_range=(1, 2)):
    """Ajusta el TfidfVectorizer sobre el corpus y devuelve (modelo, matriz).

    """
    vectorizador = TfidfVectorizer(
        tokenizer=tokenizar,
        preprocessor=None,
        lowercase=False,
        token_pattern=None,
        ngram_range=ngram_range,
        min_df=min_df,
        max_df=max_df,
        sublinear_tf=True,
        norm="l2",
    )
    matriz = vectorizador.fit_transform(textos)
    return vectorizador, matriz


# REDUCCION DE DIMENSIONALIDAD (LSA / TRUNCATED SVD)

# Truncated SVD se escoge sobre PCA porque opera directamente sobre la matriz
# dispersa sin densificarla .


def construir_lsa(matriz_tfidf, n_componentes: int = N_COMPONENTES_LSA,
                  semilla: int = SEMILLA):
    """Ajusta Truncated SVD y devuelve (svd, normalizador, matriz reducida)."""
    n_max = min(matriz_tfidf.shape) - 1
    n_componentes = int(min(n_componentes, n_max))

    svd = TruncatedSVD(n_components=n_componentes, random_state=semilla,
                       algorithm="randomized", n_iter=10)
    reducida = svd.fit_transform(matriz_tfidf)

    # Normalizar a norma 1 hace que el producto punto sea la similitud coseno.
    normalizador = Normalizer(copy=True)
    reducida = normalizador.fit_transform(reducida)
    return svd, normalizador, reducida


def curva_varianza(matriz_tfidf, n_max: int = None, semilla: int = SEMILLA):
    """Varianza explicada por componente y acumulada (para el scree plot)."""
    if n_max is None:
        n_max = min(matriz_tfidf.shape) - 1
    svd = TruncatedSVD(n_components=int(n_max), random_state=semilla,
                       algorithm="randomized", n_iter=10)
    svd.fit(matriz_tfidf)
    ratio = svd.explained_variance_ratio_
    return pd.DataFrame(
        {
            "componente": np.arange(1, len(ratio) + 1),
            "varianza_explicada": ratio,
            "varianza_acumulada": np.cumsum(ratio),
        }
    )


# ESTRATEGIAS DE RECUPERACION Y RANKING
# 
# A - TF-IDF + coseno   : lexica, espacio disperso de alta dimension
# B - LSA + coseno      : semantica latente, espacio denso reducido
# C - BM25 (Okapi)      : lexica, ranking probabilistico con saturacion
#                         de frecuencia y ajuste por longitud

ESTRATEGIAS = {
    "A": "TF-IDF + similitud coseno (lexica)",
    "B": f"LSA / Truncated SVD + similitud coseno (semantica latente)",
    "C": "BM25 Okapi (lexica, ranking probabilistico)",
}


def puntajes_tfidf(consulta: str, artefactos: dict) -> np.ndarray:
    q = artefactos["tfidf"].transform([consulta])
    return cosine_similarity(q, artefactos["matriz_tfidf"]).ravel()


def puntajes_lsa(consulta: str, artefactos: dict) -> np.ndarray:
    """Proyecta la consulta al MISMO espacio latente antes de comparar."""
    q = artefactos["tfidf"].transform([consulta])
    q_reducida = artefactos["svd"].transform(q)
    q_reducida = artefactos["normalizador"].transform(q_reducida)
    return (artefactos["matriz_lsa"] @ q_reducida.ravel())


def puntajes_bm25(consulta: str, artefactos: dict) -> np.ndarray:
    tokens = tokenizar(consulta)
    if not tokens:
        return np.zeros(len(artefactos["df"]))
    return np.asarray(artefactos["bm25"].get_scores(tokens), dtype=float)


FUNCIONES_PUNTAJE = {
    "A": puntajes_tfidf,
    "B": puntajes_lsa,
    "C": puntajes_bm25,
}


def _desempatar(puntajes: np.ndarray, artefactos: dict) -> np.ndarray:
    """Orden descendente por puntaje; los empates se rompen por citas y luego
    por fecha de publicacion mas reciente. El criterio secundario NUNCA
    reordena documentos con puntajes distintos.
    """
    df = artefactos["df"]
    citas = df["citations"].fillna(0).to_numpy(dtype=float)
    fechas = pd.to_datetime(df["publication_date"], errors="coerce")
    fechas = fechas.fillna(pd.Timestamp("1900-01-01")).astype("int64").to_numpy()
    # np.lexsort ordena por la ultima clave primero -> puntaje manda.
    return np.lexsort((-fechas, -citas, -np.round(puntajes, 10)))

#  CONSTRUCCION Y PERSISTENCIA DE ARTEFACTOS

def construir_artefactos(db_path: str = DB_PATH,
                         n_componentes: int = N_COMPONENTES_LSA) -> dict:
    """Ejecuta el pipeline completo y devuelve el diccionario de artefactos."""
    df = cargar_corpus(db_path)
    textos = df["texto_corpus"].tolist()

    tfidf, matriz_tfidf = construir_tfidf(textos)
    svd, normalizador, matriz_lsa = construir_lsa(matriz_tfidf, n_componentes)

    corpus_tokenizado = [tokenizar(t) for t in textos]
    bm25 = BM25Okapi(corpus_tokenizado)

    return {
        "df": df,
        "tfidf": tfidf,
        "matriz_tfidf": matriz_tfidf,
        "svd": svd,
        "normalizador": normalizador,
        "matriz_lsa": matriz_lsa,
        "bm25": bm25,
        "corpus_tokenizado": corpus_tokenizado,
        "meta": {
            "n_documentos": len(df),
            "dim_original": matriz_tfidf.shape[1],
            "dim_reducida": matriz_lsa.shape[1],
            "varianza_acumulada": float(svd.explained_variance_ratio_.sum()),
            "densidad_tfidf": float(matriz_tfidf.nnz /
                                    (matriz_tfidf.shape[0] * matriz_tfidf.shape[1])),
            "peso_titulo": PESO_TITULO,
        },
    }


def guardar_artefactos(artefactos: dict, ruta: str = ARTEFACTOS_PATH) -> str:
    os.makedirs(os.path.dirname(ruta), exist_ok=True)
    joblib.dump(artefactos, ruta, compress=3)
    return ruta


def cargar_artefactos(ruta: str = ARTEFACTOS_PATH, db_path: str = DB_PATH) -> dict:
    """Carga los objetos precalculados; si no existen, los construye.

    """
    if os.path.exists(ruta):
        try:
            return joblib.load(ruta)
        except Exception:
            pass
    artefactos = construir_artefactos(db_path)
    try:
        guardar_artefactos(artefactos, ruta)
    except Exception:
        pass
    return artefactos


# INTERFAZ DE BUSQUEDA

COLUMNAS_RESULTADO = [
    "posicion", "paper_id", "title", "authors_raw", "publication_date",
    "topic_label", "doi", "url", "puntaje", "fragmento",
]


def _fragmento(texto: str, consulta: str, n_palabras: int = 45) -> str:
    """Extrae del resumen la ventana con mas terminos de la consulta."""
    palabras = str(texto).split()
    if not palabras:
        return ""
    if len(palabras) <= n_palabras:
        return " ".join(palabras)

    objetivo = set(tokenizar(consulta))
    mejor_i, mejor_n = 0, -1
    for i in range(0, len(palabras) - n_palabras + 1, 5):
        ventana = palabras[i:i + n_palabras]
        n = sum(1 for t in tokenizar(" ".join(ventana)) if t in objetivo)
        if n > mejor_n:
            mejor_i, mejor_n = i, n
    prefijo = "..." if mejor_i > 0 else ""
    sufijo = "..." if mejor_i + n_palabras < len(palabras) else ""
    return prefijo + " ".join(palabras[mejor_i:mejor_i + n_palabras]) + sufijo


def buscar(consulta: str, artefactos: dict, estrategia: str = "A",
           top_k: int = 10) -> pd.DataFrame:
    """Devuelve los `top_k` articulos mas relevantes para la consulta.

    estrategia: "A" (TF-IDF), "B" (LSA) o "C" (BM25).
    """
    if estrategia not in FUNCIONES_PUNTAJE:
        raise ValueError(f"Estrategia desconocida: {estrategia}")
    if not str(consulta).strip():
        return pd.DataFrame(columns=COLUMNAS_RESULTADO)

    df = artefactos["df"]
    puntajes = FUNCIONES_PUNTAJE[estrategia](consulta, artefactos)
    orden = _desempatar(puntajes, artefactos)[:top_k]

    res = df.iloc[orden].copy().reset_index(drop=True)
    res.insert(0, "posicion", np.arange(1, len(res) + 1))
    res["puntaje"] = puntajes[orden]
    res["fragmento"] = [_fragmento(t, consulta) for t in res["abstract"]]
    return res[COLUMNAS_RESULTADO]


def buscar_con_tiempo(consulta: str, artefactos: dict, estrategia: str = "A",
                      top_k: int = 10):
    """Igual que `buscar`, pero devuelve tambien el tiempo de respuesta (ms)."""
    t0 = time.perf_counter()
    res = buscar(consulta, artefactos, estrategia, top_k)
    return res, (time.perf_counter() - t0) * 1000


# EVALUACION (PRECISION@K, MRR)

def precision_at_k(ids_recuperados, ids_relevantes, k: int = 5) -> float:
    """Proporcion de relevantes entre los primeros k resultados."""
    top = list(ids_recuperados)[:k]
    if not top:
        return 0.0
    relevantes = set(ids_relevantes)
    return sum(1 for i in top if i in relevantes) / k


def reciprocal_rank(ids_recuperados, ids_relevantes) -> float:
    """Inverso de la posicion del primer resultado relevante."""
    relevantes = set(ids_relevantes)
    for pos, pid in enumerate(ids_recuperados, start=1):
        if pid in relevantes:
            return 1.0 / pos
    return 0.0


def evaluar(consultas: dict, artefactos: dict, estrategias=("A", "B", "C"),
            k: int = 5) -> pd.DataFrame:
    """Evalua cada consulta con cada estrategia.

    `consultas` es un diccionario {texto_consulta: [paper_id relevantes]}.
    """
    filas = []
    for texto, relevantes in consultas.items():
        for est in estrategias:
            res, ms = buscar_con_tiempo(texto, artefactos, est, top_k=max(k, 10))
            ids = res["paper_id"].tolist()
            filas.append(
                {
                    "consulta": texto,
                    "estrategia": est,
                    "nombre_estrategia": ESTRATEGIAS[est],
                    f"Precision@{k}": precision_at_k(ids, relevantes, k),
                    "MRR": reciprocal_rank(ids, relevantes),
                    "tiempo_ms": round(ms, 2),
                    "top_ids": ids[:k],
                }
            )
    return pd.DataFrame(filas)


if __name__ == "__main__":
    art = construir_artefactos()
    guardar_artefactos(art)
    print("Artefactos construidos:", art["meta"])
