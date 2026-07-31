"""
buscador.py
-----------
Sistema de recuperación de información para artículos científicos de la
revista MAKE (Machine Learning and Knowledge Extraction).

Implementa dos estrategias:
  1. TF-IDF + Similitud Coseno  (recuperación léxica)
  2. LSA (TF-IDF + Truncated SVD) + Similitud Coseno (recuperación semántica)

Los modelos se calculan UNA SOLA VEZ (en taller_4.ipynb) y se guardan en
la carpeta /modelos. La app los carga desde disco sin reconstruir nada.

Uso básico:
    from buscador import Buscador
    b = Buscador(db_path="make_q1_2025.sqlite")
    resultados = b.buscar("deep learning for medical imaging", metodo="tfidf", top_n=10)
"""

import os
import re
import sqlite3
import joblib
import numpy as np
import pandas as pd
from scipy import sparse
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.preprocessing import normalize

# ---------------------------------------------------------------------------
# Configuración
# ---------------------------------------------------------------------------
MODELOS_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "modelos")
os.makedirs(MODELOS_DIR, exist_ok=True)

# Rutas de los objetos guardados
PATH_VECTORIZER   = os.path.join(MODELOS_DIR, "tfidf_vectorizer.pkl")
PATH_TFIDF_MATRIX = os.path.join(MODELOS_DIR, "tfidf_matrix.npz")
PATH_SVD_MODEL    = os.path.join(MODELOS_DIR, "svd_model.pkl")
PATH_LSA_MATRIX   = os.path.join(MODELOS_DIR, "lsa_matrix.npy")
PATH_IDS          = os.path.join(MODELOS_DIR, "paper_ids.npy")

# Parámetros del modelo
N_COMPONENTES_LSA = 100  # dimensión reducida para LSA
MAX_FEATURES      = 5000 # tamaño máximo del vocabulario TF-IDF


# ---------------------------------------------------------------------------
# Preprocesamiento de texto
# ---------------------------------------------------------------------------
def limpiar_texto(texto: str) -> str:
    """
    Limpieza básica del texto:
    - Minúsculas
    - Elimina caracteres especiales y números sueltos
    - Colapsa espacios múltiples
    Conservamos términos científicos (siglas, números en contexto).
    NO aplicamos stemming ni lematización para conservar precisión semántica.
    """
    if not isinstance(texto, str) or not texto.strip():
        return ""
    texto = texto.lower()
    texto = re.sub(r"<[^>]+>", " ", texto)      # quitar etiquetas HTML/JATS
    texto = re.sub(r"[^a-z0-9\s]", " ", texto)  # solo alfanuméricos
    texto = re.sub(r"\b\d+\b", " ", texto)       # quitar números sueltos
    texto = re.sub(r"\s+", " ", texto).strip()
    return texto


def construir_corpus(df: pd.DataFrame) -> tuple[list[str], list[int]]:
    """
    Construye el corpus combinando título + abstract de cada artículo.
    El título se pondera más repitiéndolo 3 veces.
    Devuelve (corpus, paper_ids).
    """
    corpus, ids = [], []
    for _, row in df.iterrows():
        titulo   = limpiar_texto(str(row.get("title", "") or ""))
        abstract = limpiar_texto(str(row.get("abstract", "") or ""))
        # Si no hay abstract, usamos el topic_label como respaldo
        if not abstract:
            abstract = limpiar_texto(str(row.get("topic_label", "") or ""))
        # Ponderación: repetimos el título 3 veces
        texto = (titulo + " ") * 3 + abstract
        if texto.strip():
            corpus.append(texto)
            ids.append(int(row["paper_id"]))
    return corpus, ids


# ---------------------------------------------------------------------------
# Entrenamiento y guardado de modelos
# ---------------------------------------------------------------------------
def entrenar_y_guardar(db_path: str) -> dict:
    """
    Lee la base SQLite, construye el corpus, entrena TF-IDF y LSA,
    y guarda todos los objetos en /modelos. Devuelve métricas del proceso.
    """
    # 1. Leer datos
    con = sqlite3.connect(db_path)
    df  = pd.read_sql_query("SELECT * FROM papers", con)
    con.close()

    n_total    = len(df)
    n_sin_abs  = df["abstract"].isna().sum() + (df["abstract"].fillna("").str.strip() == "").sum()
    n_sin_tit  = df["title"].isna().sum()

    # 2. Construir corpus
    corpus, ids = construir_corpus(df)
    n_incluidos = len(corpus)

    # 3. TF-IDF
    vectorizer = TfidfVectorizer(
        max_features=MAX_FEATURES,
        min_df=2,          # ignorar términos que aparecen en menos de 2 docs
        max_df=0.95,       # ignorar términos que aparecen en más del 95% de docs
        sublinear_tf=True, # aplicar log al TF para suavizar frecuencias altas
        ngram_range=(1, 2) # unigramas y bigramas
    )
    tfidf_matrix = vectorizer.fit_transform(corpus)  # dispersa (sparse)
    dim_original = tfidf_matrix.shape[1]

    # 4. LSA = TF-IDF + Truncated SVD
    n_comp = min(N_COMPONENTES_LSA, tfidf_matrix.shape[0] - 1,
                 tfidf_matrix.shape[1] - 1)
    svd_model = TruncatedSVD(n_components=n_comp, random_state=42)
    lsa_matrix = svd_model.fit_transform(tfidf_matrix)
    lsa_matrix = normalize(lsa_matrix)  # normalizar para coseno
    dim_reducida = lsa_matrix.shape[1]
    varianza_exp = svd_model.explained_variance_ratio_.sum()

    # 5. Guardar objetos
    joblib.dump(vectorizer, PATH_VECTORIZER)
    sparse.save_npz(PATH_TFIDF_MATRIX, tfidf_matrix)
    joblib.dump(svd_model, PATH_SVD_MODEL)
    np.save(PATH_LSA_MATRIX, lsa_matrix)
    np.save(PATH_IDS, np.array(ids))

    return {
        "n_total": n_total,
        "n_sin_abstract": int(n_sin_abs),
        "n_sin_titulo": int(n_sin_tit),
        "n_incluidos": n_incluidos,
        "vocabulario": dim_original,
        "dim_original": dim_original,
        "dim_reducida": dim_reducida,
        "n_componentes_lsa": n_comp,
        "varianza_explicada_lsa": round(float(varianza_exp), 4),
    }


# ---------------------------------------------------------------------------
# Clase principal del buscador
# ---------------------------------------------------------------------------
class Buscador:
    """
    Buscador de artículos científicos con dos estrategias:
      - 'tfidf': TF-IDF + Similitud Coseno (léxica)
      - 'lsa'  : LSA (TF-IDF + SVD) + Similitud Coseno (semántica)

    Los modelos se cargan desde disco al instanciar la clase.
    """

    def __init__(self, db_path: str):
        self.db_path = db_path
        self._cargar_datos()
        self._cargar_modelos()

    def _cargar_datos(self):
        con = sqlite3.connect(self.db_path)
        self.df = pd.read_sql_query("SELECT * FROM papers", con)
        con.close()
        # Índice por paper_id para búsqueda rápida
        self.df = self.df.set_index("paper_id")

    def _cargar_modelos(self):
        """Carga los modelos desde disco. Lanza error si no existen."""
        if not os.path.exists(PATH_VECTORIZER):
            raise FileNotFoundError(
                "Modelos no encontrados. Ejecuta entrenar_y_guardar() primero."
            )
        self.vectorizer  = joblib.load(PATH_VECTORIZER)
        self.tfidf_matrix = sparse.load_npz(PATH_TFIDF_MATRIX)
        self.svd_model   = joblib.load(PATH_SVD_MODEL)
        self.lsa_matrix  = np.load(PATH_LSA_MATRIX)
        self.paper_ids   = np.load(PATH_IDS)

    def _buscar_tfidf(self, consulta: str, top_n: int) -> pd.DataFrame:
        """Recuperación léxica: TF-IDF + Similitud Coseno."""
        consulta_limpia = limpiar_texto(consulta)
        q_vec = self.vectorizer.transform([consulta_limpia])
        sims  = cosine_similarity(q_vec, self.tfidf_matrix).flatten()
        return self._construir_resultados(sims, top_n, "TF-IDF")

    def _buscar_lsa(self, consulta: str, top_n: int) -> pd.DataFrame:
        """
        Recuperación semántica: LSA (TF-IDF + Truncated SVD) + Similitud Coseno.
        La consulta se proyecta al espacio reducido antes de calcular similitud.
        """
        consulta_limpia = limpiar_texto(consulta)
        q_vec  = self.vectorizer.transform([consulta_limpia])
        # Proyectar la consulta al mismo espacio reducido que los artículos
        q_lsa  = self.svd_model.transform(q_vec)
        q_lsa  = normalize(q_lsa)
        sims   = cosine_similarity(q_lsa, self.lsa_matrix).flatten()
        return self._construir_resultados(sims, top_n, "LSA")

    def _construir_resultados(self, sims: np.ndarray,
                               top_n: int, metodo: str) -> pd.DataFrame:
        """Construye el DataFrame de resultados ordenado por similitud."""
        indices = np.argsort(sims)[::-1][:top_n]
        filas = []
        for pos, idx in enumerate(indices, start=1):
            pid  = self.paper_ids[idx]
            row  = self.df.loc[pid] if pid in self.df.index else None
            if row is None:
                continue
            abstract = str(row.get("abstract") or "").strip()
            titulo   = str(row.get("title") or "")
            # Si no hay abstract, usar el título como fragmento con una nota
            if not abstract or abstract == "nan":
                fragmento = f"[Sin resumen disponible] {titulo}"
            else:
                fragmento = abstract[:250] + "..." if len(abstract) > 250 else abstract
            filas.append({
                "Posición":   pos,
                "paper_id":   int(pid),
                "Título":     str(row.get("title") or ""),
                "Autores":    str(row.get("authors_raw") or ""),
                "Fecha":      str(row.get("publication_date") or ""),
                "Tema":       str(row.get("topic_label") or ""),
                "DOI":        str(row.get("doi") or ""),
                "Puntaje":    round(float(sims[idx]), 4),
                "Fragmento":  fragmento,
                "Método":     metodo,
            })
        return pd.DataFrame(filas)

    def buscar(self, consulta: str,
               metodo: str = "tfidf", top_n: int = 10) -> pd.DataFrame:
        """
        Busca artículos relevantes para una consulta en lenguaje natural.

        Args:
            consulta: texto de búsqueda en lenguaje natural.
            metodo: 'tfidf' | 'lsa' | 'ambos'
            top_n: número de resultados a devolver.

        Returns:
            DataFrame con los artículos ordenados por relevancia.
        """
        if not consulta.strip():
            return pd.DataFrame()

        if metodo == "tfidf":
            return self._buscar_tfidf(consulta, top_n)
        elif metodo == "lsa":
            return self._buscar_lsa(consulta, top_n)
        elif metodo == "ambos":
            r1 = self._buscar_tfidf(consulta, top_n)
            r2 = self._buscar_lsa(consulta, top_n)
            return pd.concat([r1, r2], ignore_index=True)
        else:
            raise ValueError(f"Método '{metodo}' no reconocido. Usa 'tfidf', 'lsa' o 'ambos'.")

    def modelos_disponibles(self) -> bool:
        """Comprueba si los modelos ya están entrenados."""
        return all(os.path.exists(p) for p in [
            PATH_VECTORIZER, PATH_TFIDF_MATRIX,
            PATH_SVD_MODEL, PATH_LSA_MATRIX, PATH_IDS
        ])
