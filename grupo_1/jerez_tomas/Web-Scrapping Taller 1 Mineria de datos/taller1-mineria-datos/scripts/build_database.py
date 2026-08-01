import json
import re
import sqlite3
from pathlib import Path
from collections import Counter


SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_DIR = SCRIPT_DIR.parent
DATA_DIR = PROJECT_DIR / "data"
OUTPUT_DIR = PROJECT_DIR / "output"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

PAPERS_JSON = DATA_DIR / "papers_raw.json"
DB_FILE = DATA_DIR / "revista_q1_2025.sqlite"
RESULTS_FILE = OUTPUT_DIR / "query_results.json"


GEN_AI_PATTERNS = [
    r"\b(generative\s+(model|ai|adversarial|network)s?)\b",
    r"\b(diffusion\s+(model|process)s?)\b",
    r"\b(large\s+language\s+model|llm|llms)\b",
    r"\b(foundation\s+model|chatgpt|gpt-?\d|bert\b|llama)\b",
    r"\b(text-?to-?(image|video|3d|protein))\b",
    r"\b(stable\s+diffusion|dall-?e|midjourney)\b",
    r"\b(variational\s+autoencoder|vae)\b",
    r"\bgan\b|\bgans\b",
    r"\b(autoregressive\s+(model|generation))\b",
    r"\b(image|text|video|protein|molecule|sequence)\s+generation\b",
    r"\b(prompt(ing)?|in-context\s+learning)\b",
    r"\b(transformer-based\s+generat)",
]

ML_PATTERNS = [
    r"\b(machine\s+learning|ml\s+model)\b",
    r"\b(deep\s+learning|neural\s+network)s?\b",
    r"\b(reinforcement\s+learning|rl\s+agent)\b",
    r"\b(self-supervised|supervised|unsupervised)\s+learning\b",
    r"\b(transfer\s+learning|federated\s+learning|meta-?learning)\b",
    r"\b(transformer|attention\s+mechanism)s?\b",
    r"\b(convolutional|recurrent|graph)\s+neural\b",
    r"\b(cnn|rnn|lstm|gnn|mlp)\b",
    r"\b(classification|regression|clustering)\s+(model|algorithm|task)",
    r"\b(feature\s+(learning|extraction|engineering))\b",
    r"\b(embedding|representation\s+learning)s?\b",
    r"\b(pretrain|fine-?tun|backbone\s+model)",
    r"\b(artificial\s+intelligence|ai\s+(model|system|agent))\b",
]

STATS_PATTERNS = [
    r"\b(bayesian\s+(inference|model|method|network))\b",
    r"\b(statistical\s+(method|model|test|inference|analysis))\b",
    r"\b(hypothesis\s+test|p-?value|confidence\s+interval)\b",
    r"\b(monte\s+carlo|markov\s+chain|mcmc)\b",
    r"\b(maximum\s+likelihood|likelihood\s+estimation)\b",
    r"\b(uncertainty\s+quantification)\b",
    r"\b(probabilistic\s+(model|inference|programming))\b",
    r"\b(stochastic\s+process|gaussian\s+process)\b",
    r"\b(regression\s+analysis|generalized\s+linear\s+model)\b",
]


def classify_paper(paper):
    parts = [
        paper.get("title", "") or "",
        paper.get("abstract", "") or "",
        " ".join(paper.get("subjects", []) or []),
    ]
    text = " ".join(parts).lower()

    for pat in GEN_AI_PATTERNS:
        if re.search(pat, text, flags=re.IGNORECASE):
            return "IA Generativa"

    for pat in ML_PATTERNS:
        if re.search(pat, text, flags=re.IGNORECASE):
            return "Machine Learning"

    for pat in STATS_PATTERNS:
        if re.search(pat, text, flags=re.IGNORECASE):
            return "Estadística"

    return "Otros"


def normalize_reference(ref_text):
    """Normaliza una referencia para poder agruparlas (encontrar duplicados).
    
    Convierte a minúsculas, elimina puntuación, espacios extra, números de
    volumen/página comunes, etc. El objetivo es que dos citas a la misma
    obra sean iguales tras la normalización.
    """
    if not ref_text:
        return ""
    s = ref_text.lower()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"doi[: ]\S+", "", s)
    s = re.sub(r"[^\w\s]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def parse_year(date_str):
    """Extrae el año de strings tipo '2025/01/23' o '2025-01-23'."""
    if not date_str:
        return None
    m = re.search(r"(\d{4})", date_str)
    return int(m.group(1)) if m else None


def build_database(papers):
    """Crea la BD SQLite con todas las tablas y carga los datos."""
    if DB_FILE.exists():
        DB_FILE.unlink()  # empezar limpio
    
    conn = sqlite3.connect(DB_FILE)
    cur = conn.cursor()

    cur.executescript("""
        CREATE TABLE papers (
            paper_id          TEXT PRIMARY KEY,
            journal_name      TEXT,
            title             TEXT,
            publication_date  TEXT,
            year              INTEGER,
            doi               TEXT,
            url               TEXT,
            abstract          TEXT,
            authors_raw       TEXT,
            n_authors         INTEGER,
            citations         INTEGER,
            downloads         INTEGER,
            altmetric         INTEGER,
            n_references      INTEGER,
            topic_label       TEXT,
            article_type      TEXT
        );

        CREATE TABLE authors (
            author_id    INTEGER PRIMARY KEY AUTOINCREMENT,
            author_name  TEXT UNIQUE
        );

        CREATE TABLE paper_authors (
            paper_id      TEXT,
            author_id     INTEGER,
            author_order  INTEGER,
            PRIMARY KEY (paper_id, author_id),
            FOREIGN KEY (paper_id)  REFERENCES papers(paper_id),
            FOREIGN KEY (author_id) REFERENCES authors(author_id)
        );

        CREATE TABLE refs (
            reference_id              INTEGER PRIMARY KEY AUTOINCREMENT,
            reference_text            TEXT,
            reference_text_normalized TEXT UNIQUE
        );

        CREATE TABLE paper_refs (
            paper_id     TEXT,
            reference_id INTEGER,
            ref_order    INTEGER,
            PRIMARY KEY (paper_id, reference_id),
            FOREIGN KEY (paper_id)     REFERENCES papers(paper_id),
            FOREIGN KEY (reference_id) REFERENCES refs(reference_id)
        );

        CREATE INDEX idx_papers_topic ON papers(topic_label);
        CREATE INDEX idx_papers_year  ON papers(year);
        CREATE INDEX idx_pa_paper     ON paper_authors(paper_id);
        CREATE INDEX idx_pr_paper     ON paper_refs(paper_id);
    """)

    print(f"Insertando {len(papers)} papers...")
    topic_counter = Counter()
    
    for p in papers:
        topic = classify_paper(p)
        topic_counter[topic] += 1
        
        cur.execute("""
            INSERT INTO papers (paper_id, journal_name, title, publication_date,
                                year, doi, url, abstract, authors_raw, n_authors,
                                citations, downloads, altmetric, n_references,
                                topic_label, article_type)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            p["paper_id"],
            p.get("journal", "Nature Machine Intelligence"),
            p.get("title"),
            p.get("publication_date"),
            parse_year(p.get("publication_date") or p.get("online_date") or p.get("date_listing")),
            p.get("doi"),
            p.get("url"),
            p.get("abstract"),
            "; ".join(p.get("authors", [])),
            p.get("n_authors", 0),
            p.get("citations_nature"),  
            p.get("accesses"),          
            p.get("altmetric"),
            p.get("n_references", 0),
            topic,
            p.get("article_type"),
        ))

    print("Insertando autores...")
    for p in papers:
        for order, name in enumerate(p.get("authors", []), start=1):
            name = name.strip()
            if not name:
                continue
            cur.execute("INSERT OR IGNORE INTO authors (author_name) VALUES (?)", (name,))
            author_id = cur.execute(
                "SELECT author_id FROM authors WHERE author_name = ?", (name,)
            ).fetchone()[0]
            cur.execute("""
                INSERT OR IGNORE INTO paper_authors (paper_id, author_id, author_order)
                VALUES (?, ?, ?)
            """, (p["paper_id"], author_id, order))

    print("Insertando referencias ")
    for p in papers:
        for order, ref in enumerate(p.get("references", []), start=1):
            ref = ref.strip()
            if not ref:
                continue
            norm = normalize_reference(ref)
            if not norm:
                continue
            cur.execute(
                "INSERT OR IGNORE INTO refs (reference_text, reference_text_normalized) VALUES (?, ?)",
                (ref, norm)
            )
            ref_id = cur.execute(
                "SELECT reference_id FROM refs WHERE reference_text_normalized = ?", (norm,)
            ).fetchone()[0]
            cur.execute("""
                INSERT OR IGNORE INTO paper_refs (paper_id, reference_id, ref_order)
                VALUES (?, ?, ?)
            """, (p["paper_id"], ref_id, order))

    conn.commit()

    print("\nDISTRIBUCIÓN POR CATEGORÍA TEMÁTICA")
    for cat, count in topic_counter.most_common():
        pct = 100 * count / len(papers)
        print(f"  {cat:20s} {count:3d}  ({pct:5.1f}%)")

    n_authors = cur.execute("SELECT COUNT(*) FROM authors").fetchone()[0]
    n_refs = cur.execute("SELECT COUNT(*) FROM refs").fetchone()[0]
    n_paper_refs = cur.execute("SELECT COUNT(*) FROM paper_refs").fetchone()[0]
    print(f"\nTABLAS POBLADAS")
    print(f"  papers        : {len(papers)}")
    print(f"  authors       : {n_authors} (únicos)")
    print(f"  refs          : {n_refs} (únicas tras normalizar)")
    print(f"  paper_refs    : {n_paper_refs} (relaciones totales)")

    conn.close()
    return DB_FILE


QUERIES = {
    "Q1_promedio_autores_por_paper": """
        SELECT ROUND(AVG(n_authors), 2) AS promedio_autores
        FROM papers;
    """,

    "Q2_articulos_machine_learning": """
        SELECT COUNT(*) AS n_papers_ML
        FROM papers
        WHERE topic_label = 'Machine Learning';
    """,

    "Q3_articulos_ia_generativa": """
        SELECT COUNT(*) AS n_papers_IAGen
        FROM papers
        WHERE topic_label = 'IA Generativa';
    """,

    "Q4_articulos_estadistica_otros": """
        SELECT topic_label, COUNT(*) AS n_papers
        FROM papers
        WHERE topic_label IN ('Estadística', 'Otros')
        GROUP BY topic_label
        ORDER BY n_papers DESC;
    """,

    "Q5_total_descargas_2025": """
        SELECT SUM(downloads) AS total_descargas
        FROM papers
        WHERE year = 2025
          AND downloads IS NOT NULL;
    """,

    "Q6_promedio_referencias_por_paper": """
        SELECT ROUND(AVG(n_references), 2) AS promedio_referencias
        FROM papers
        WHERE n_references > 0;
    """,

    "Q7_referencia_mas_repetida": """
        SELECT r.reference_text AS referencia,
               COUNT(*)        AS veces_citada
        FROM paper_refs pr
        JOIN refs r ON r.reference_id = pr.reference_id
        GROUP BY r.reference_id
        ORDER BY veces_citada DESC
        LIMIT 5;
    """,

    "Q8_promedio_citas_por_paper": """
        SELECT ROUND(AVG(citations), 2) AS promedio_citas,
               COUNT(*)                  AS papers_con_dato
        FROM papers
        WHERE citations IS NOT NULL;
    """,

    "Q9_paper_mas_citado": """
        SELECT title, doi, citations, downloads, topic_label
        FROM papers
        WHERE citations IS NOT NULL
        ORDER BY citations DESC
        LIMIT 1;
    """,

    "Q10_paper_topico_mas_citado": """
        SELECT title, doi, citations, topic_label
        FROM papers
        WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
          AND citations IS NOT NULL
        ORDER BY citations DESC
        LIMIT 1;
    """,

    "Q11_paper_topico_mas_descargado": """
        SELECT title, doi, downloads, topic_label
        FROM papers
        WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
          AND downloads IS NOT NULL
        ORDER BY downloads DESC
        LIMIT 1;
    """,
}


def run_queries(db_path):
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()

    results = {}
    print("\n" + "=" * 70)
    print("RESULTADOS DE LAS 11 CONSULTAS SQL")
    print("=" * 70)

    for qname, sql in QUERIES.items():
        rows = cur.execute(sql).fetchall()
        rows_dict = [dict(r) for r in rows]
        results[qname] = {"sql": sql.strip(), "result": rows_dict}

        print(f"\n[{qname}]")
        for r in rows_dict:
            for k, v in r.items():
                if isinstance(v, str) and len(v) > 100:
                    v = v[:100] + "..."
                print(f"   {k}: {v}")

    conn.close()

    with open(RESULTS_FILE, "w") as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    print(f"\nResultados guardados en: {RESULTS_FILE}")
    return results


if __name__ == "__main__":
    print("=" * 70)
    print(" CONSTRUCCIÓN DE BASE DE DATOS SQLITE Y CONSULTAS")
    print("=" * 70)
    print(f" JSON entrada: {PAPERS_JSON}")
    print(f" BD salida:    {DB_FILE}")
    print()

    with open(PAPERS_JSON) as f:
        papers = json.load(f)
    print(f"Cargados {len(papers)} papers desde JSON")

    db_path = build_database(papers)
    print(f"\nBD creada en: {db_path}")
    print(f"Tamaño: {db_path.stat().st_size / 1024:.1f} KB")

    run_queries(db_path)

    print("\n" + "=" * 70)
    print(" PROCESO COMPLETADO")
    print("=" * 70)
