"""
db.py — Capa de acceso a la base de datos SQLite del Taller 1.

Centraliza:
  * Lectura de la base (papers y tablas relacionadas).
  * Parseo de fechas (soporta los dos formatos presentes en la BD).
  * Clasificación temática (idéntica a la del Taller 1, con apoyo del título).
  * Inserción normalizada en las 5 tablas (papers, authors, paper_authors,
    references_tbl, paper_references).
  * Actualización de métricas (citas / descargas) para la reconsulta.

Tanto el scraper por navegador como el scraper por API usan estas funciones,
de modo que la lógica de inserción es única y consistente.
"""

import re
import sqlite3
import pandas as pd

# ─────────────────────────────────────────────────────────────────────────────
#  Parseo de fechas
#  La BD del Taller 1 trae dos formatos:
#     "01 October 2024"   (la mayoría)
#     "09-may-25"         (los últimos 7 artículos; mes en español, año a 2 díg.)
# ─────────────────────────────────────────────────────────────────────────────
_MESES = {
    "enero": 1, "ene": 1, "january": 1, "jan": 1,
    "febrero": 2, "feb": 2, "february": 2,
    "marzo": 3, "mar": 3, "march": 3,
    "abril": 4, "abr": 4, "april": 4, "apr": 4,
    "mayo": 5, "may": 5,
    "junio": 6, "jun": 6, "june": 6,
    "julio": 7, "jul": 7, "july": 7,
    "agosto": 8, "ago": 8, "august": 8, "aug": 8,
    "septiembre": 9, "sep": 9, "sept": 9, "september": 9,
    "octubre": 10, "oct": 10, "october": 10,
    "noviembre": 11, "nov": 11, "november": 11,
    "diciembre": 12, "dic": 12, "december": 12, "dec": 12,
}


def parse_fecha(s):
    """Convierte una cadena de fecha a Timestamp; NaT si no se puede."""
    if s is None:
        return pd.NaT
    txt = str(s).strip().lower()
    if txt in ("", "nan", "none", "n/a"):
        return pd.NaT

    tokens = re.split(r"[\s\-/.,]+", txt)
    day = month = year = None
    for tk in tokens:
        if tk.isdigit():
            v = int(tk)
            if v > 31:                 # año de 4 dígitos
                year = v
            elif day is None:
                day = v
            else:                      # segundo número: año a 2 dígitos
                year = v
        elif tk in _MESES:
            month = _MESES[tk]

    if year is not None and year < 100:
        year += 2000

    try:
        if year and month and day:
            return pd.Timestamp(year=year, month=month, day=day)
        if year and month:
            return pd.Timestamp(year=year, month=month, day=1)
        if year:
            return pd.Timestamp(year=year, month=1, day=1)
    except (ValueError, OverflowError):
        pass
    return pd.NaT


def extraer_year(s):
    """Año (int) a partir de una cadena de fecha; None si no se detecta."""
    ts = parse_fecha(s)
    if pd.notna(ts):
        return int(ts.year)
    m = re.search(r"\d{4}", str(s))
    return int(m.group()) if m else None


def to_int(x, default=0):
    """Convierte a int de forma tolerante (maneja '1,234', 'N/A', None, floats)."""
    if x is None:
        return default
    if isinstance(x, (int,)):
        return x
    if isinstance(x, float):
        return int(x) if pd.notna(x) else default
    s = str(x).strip().replace(",", "")
    if s in ("", "nan", "none", "n/a"):
        return default
    m = re.search(r"-?\d+", s)
    return int(m.group()) if m else default


def clean_authors(raw):
    """Lista de autores deduplicada preservando el orden (igual que Taller 1)."""
    parts = [p.strip() for p in str(raw).split(";") if p.strip()]
    seen, result = set(), []
    for p in parts:
        if p not in seen:
            seen.add(p)
            result.append(p)
    return result


def autores_legibles(raw):
    """Cadena 'A; B; C' deduplicada para mostrar en la tabla."""
    return "; ".join(clean_authors(raw)) or "N/A"


# ─────────────────────────────────────────────────────────────────────────────
#  Clasificación temática (criterio del Taller 1)
#  Prioridad: IA Generativa → Machine Learning → Estadística → Otro
# ─────────────────────────────────────────────────────────────────────────────
GEN_KW = [
    "generative adversarial", "generative model", "diffusion model",
    "text-to-image", "large language model", "llm", "gpt",
    "stable diffusion", "foundation model", "latent diffusion",
    "denoising diffusion", "imagen", "dall-e", "generative",
]
ML_KW = [
    "machine learning", "deep learning", "neural network",
    "reinforcement learning", "convolutional",
    "supervised learning", "unsupervised learning", "transformer",
]
STAT_KW = [
    "statistical", "bayesian", "regression", "markov chain",
    "monte carlo", "stochastic", "probabilistic", "gaussian process",
]


def classify_topic(datos):
    """
    Asigna topic_label a partir de los campos de texto disponibles.
    `datos` es un dict con (algunas de) estas claves: ccs_tags, tags,
    resumen, titulo. Para el modo API (sin CCS) el título y el abstract
    aportan la señal principal.
    """
    combined = " ".join([
        str(datos.get("ccs_tags", "") or ""),
        str(datos.get("tags", "") or ""),
        str(datos.get("resumen", "") or ""),
        str(datos.get("titulo", "") or ""),
    ]).lower()
    for k in GEN_KW:
        if k in combined:
            return "IA Generativa"
    for k in ML_KW:
        if k in combined:
            return "Machine Learning"
    for k in STAT_KW:
        if k in combined:
            return "Estadística"
    return "Otro"


# ─────────────────────────────────────────────────────────────────────────────
#  Conexión y lectura
# ─────────────────────────────────────────────────────────────────────────────
def get_connection(db_path):
    """Conexión SQLite apta para Streamlit (varios hilos)."""
    return sqlite3.connect(db_path, check_same_thread=False)


def load_all_papers(db_path):
    """
    Devuelve un DataFrame con todos los artículos + columnas derivadas:
      fecha_dt (Timestamp), year (reconstruido), autores_clean.
    """
    con = get_connection(db_path)
    try:
        df = pd.read_sql("SELECT * FROM papers", con)
    finally:
        con.close()

    df["fecha_dt"] = df["publication_date"].apply(parse_fecha)
    # Reconstruimos el año desde la fecha parseada (corrige los 7 NULL de la BD)
    year_from_dt = df["fecha_dt"].dt.year
    df["year"] = year_from_dt.where(year_from_dt.notna(), df["year"])
    df["year"] = df["year"].astype("Int64")
    df["autores_clean"] = df["authors_raw"].apply(autores_legibles)
    for col in ("citations", "downloads", "n_references", "n_authors"):
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0).astype(int)
    return df


def cargar_dois(db_path):
    """Conjunto de DOIs ya almacenados (para detectar artículos nuevos)."""
    con = get_connection(db_path)
    try:
        rows = con.execute("SELECT doi FROM papers WHERE doi IS NOT NULL").fetchall()
    finally:
        con.close()
    return {r[0].strip().lower() for r in rows if r[0]}


def ultimos_papers(db_path, n=5):
    """Los n artículos almacenados más recientemente (por paper_id)."""
    con = get_connection(db_path)
    try:
        rows = con.execute(
            "SELECT paper_id, title, url, doi, citations, downloads "
            "FROM papers ORDER BY paper_id DESC LIMIT ?", (n,)
        ).fetchall()
    finally:
        con.close()
    cols = ["paper_id", "title", "url", "doi", "citations", "downloads"]
    return [dict(zip(cols, r)) for r in rows]


def run_select(db_path, query):
    """
    Ejecuta una consulta SOLO de lectura para el panel de consulta dinámica.
    Bloquea cualquier sentencia que no sea un único SELECT.
    """
    q = query.strip().rstrip(";")
    low = q.lower()
    prohibidas = ("insert", "update", "delete", "drop", "alter",
                  "create", "replace", "attach", "pragma", "vacuum")
    if not low.startswith("select"):
        raise ValueError("Solo se permiten consultas SELECT.")
    if ";" in q:
        raise ValueError("Solo se permite una sentencia.")
    if any(re.search(rf"\b{p}\b", low) for p in prohibidas):
        raise ValueError("La consulta contiene una palabra clave no permitida.")
    con = get_connection(db_path)
    try:
        return pd.read_sql(q, con)
    finally:
        con.close()


# ─────────────────────────────────────────────────────────────────────────────
#  Inserción normalizada (usada por ambos scrapers)
# ─────────────────────────────────────────────────────────────────────────────
def insertar_papers(db_path, lista_datos):
    """
    Inserta artículos nuevos en las 5 tablas, replicando la lógica del Taller 1.
    `lista_datos` es una lista de dicts con claves:
        titulo, doi, url_paper, fecha, autores, resumen,
        num_citas, num_descargas, tags, ccs_tags, num_referencias, referencias
    Devuelve la cantidad de artículos efectivamente insertados.
    """
    if not lista_datos:
        return 0

    con = get_connection(db_path)
    cur = con.cursor()
    insertados = 0
    try:
        max_id = cur.execute(
            "SELECT COALESCE(MAX(paper_id), 0) FROM papers"
        ).fetchone()[0]
        pid = max_id

        for d in lista_datos:
            doi = (d.get("doi") or "").strip()
            if doi:
                existe = cur.execute(
                    "SELECT 1 FROM papers WHERE doi = ?", (doi,)
                ).fetchone()
                if existe:
                    continue  # ya estaba: no duplicar

            pid += 1
            year = extraer_year(d.get("fecha"))
            autores = clean_authors(d.get("autores", ""))
            topic = classify_topic(d)

            cur.execute(
                "INSERT INTO papers VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                (
                    pid,
                    "ACM Transactions on Graphics",
                    d.get("titulo", "N/A"),
                    str(d.get("fecha", "")).strip(),
                    year,
                    doi or None,
                    d.get("url_paper"),
                    d.get("resumen"),
                    d.get("autores"),
                    len(autores),
                    to_int(d.get("num_citas")),
                    to_int(d.get("num_descargas")),
                    to_int(d.get("num_referencias")),
                    topic,
                ),
            )

            # authors + paper_authors
            for order, name in enumerate(autores, 1):
                cur.execute(
                    "INSERT OR IGNORE INTO authors(author_name) VALUES (?)", (name,)
                )
                aid = cur.execute(
                    "SELECT author_id FROM authors WHERE author_name = ?", (name,)
                ).fetchone()[0]
                cur.execute(
                    "INSERT OR IGNORE INTO paper_authors VALUES (?,?,?)",
                    (pid, aid, order),
                )

            # references_tbl + paper_references
            raw = str(d.get("referencias", "") or "")
            if raw and raw.lower() != "nan":
                refs = [re.sub(r"\s+", " ", r).strip()
                        for r in raw.split(" ||| ") if r.strip()]
                for ref in refs:
                    cur.execute(
                        "INSERT OR IGNORE INTO references_tbl"
                        "(reference_text_normalized) VALUES (?)", (ref,)
                    )
                    rid = cur.execute(
                        "SELECT reference_id FROM references_tbl "
                        "WHERE reference_text_normalized = ?", (ref,)
                    ).fetchone()[0]
                    cur.execute(
                        "INSERT OR IGNORE INTO paper_references VALUES (?,?)",
                        (pid, rid),
                    )

            insertados += 1

        con.commit()
    finally:
        con.close()
    return insertados


def actualizar_metricas(db_path, paper_id, citas, descargas):
    """Actualiza citas/descargas de un artículo existente (reconsulta)."""
    con = get_connection(db_path)
    try:
        con.execute(
            "UPDATE papers SET citations = ?, downloads = ? WHERE paper_id = ?",
            (to_int(citas), to_int(descargas), paper_id),
        )
        con.commit()
    finally:
        con.close()
