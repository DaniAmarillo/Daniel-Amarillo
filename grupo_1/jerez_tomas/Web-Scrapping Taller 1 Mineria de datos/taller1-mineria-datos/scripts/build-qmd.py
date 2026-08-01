

from pathlib import Path

OUT_FILE = Path(__file__).resolve().parent.parent / "taller1.qmd"

QMD_CONTENT = r'''---
title: "Taller 1 Minería de Datos: Recolección y análisis de artículos de Nature Machine Intelligence (2025)"
author: "[Tomas Jerez]"
date: today
format:
  html:
    toc: true
    toc-depth: 3
    number-sections: true
    code-fold: show
    code-tools: true
    theme: cosmo
    embed-resources: true
  pdf:
    toc: true
    number-sections: true
execute:
  echo: true
  warning: false
  message: false
jupyter: python3
---




Se eligió **[Nature Machine Intelligence](https://www.nature.com/natmachintell/)**
(ISSN 2522-5839), publicada por *Springer Nature*. Es una revista clasificada
como **Q1** en *Computer Science (Artificial Intelligence)* y *Computer Science
(Software)* según Scimago Journal Rank.



1. **Clasificación Q1 confirmada** en múltiples categorías relevantes.
2. **Cobertura completa de 2025**: 138 artículos, paginación clara, URLs estables.
3. **Metadatos públicos abundantes**: DOI, autores, abstract, accesses, Altmetric, referencias.
4. **Pertinencia temática**: revista enfocada en IA/ML.



Listado de 2025: `https://www.nature.com/natmachintell/research-articles?year=2025`

Distribuido en 7 páginas con el patrón:
`?searchType=journalSearch&sort=PubDate&year=2025&page={N}`

Cada artículo: `https://www.nature.com/articles/s42256-YYYY-NNNNN-X`



```{python}

import json
import sqlite3
import re
from pathlib import Path
from collections import Counter
import pandas as pd
```

| Librería | Uso |
|---|---|
| `requests` | Descarga HTTP de páginas y artículos |
| `beautifulsoup4` + `lxml` | Parseo HTML para extraer metadatos |
| `tqdm` | Barra de progreso durante el scraping |
| `sqlite3` | Construcción y consulta de la base de datos |
| `pandas` | Manipulación tabular y presentación |
| `re` | Expresiones regulares para clasificación y normalización |


El scraping se ejecutó en dos fases (script `web-scrapper.py`):

**Fase A — Listado:** se recorrieron las 7 páginas del índice 2025 obteniendo
URLs, títulos y fechas de los 138 artículos.

**Fase B — Artículos individuales:** se visitó cada URL para extraer:

- Metadatos del `<head>` mediante meta tags `citation_*` (estándar Google Scholar).
- Subjects desde elementos `<a data-track-action="view subject">`.
- Métricas (accesses, Altmetric, citas) parseando texto como "10k Accesses" → 10000.
- Lista de referencias desde `<ol class="c-article-references">`.

**Buenas prácticas aplicadas:**

- *User-Agent* de navegador real para evitar bloqueos.
- Pausas aleatorias de 1.5–3.5 segundos entre requests.
- Reintentos con *backoff* exponencial ante errores HTTP.
- *Checkpoint* persistente para reanudar si se interrumpe.


```{python}
PAPERS_JSON = "data/papers_raw.json"
DB_FILE = "data/revista_q1_2025.sqlite"

with open(PAPERS_JSON) as f:
    papers = json.load(f)

print(f"Total de artículos cargados: {len(papers)}")
```


Se diseñó un esquema relacional con la tabla principal `papers` y tres tablas
auxiliares para normalizar autores y referencias:

```{python}

SCHEMA = """
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
    PRIMARY KEY (paper_id, author_id)
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
    PRIMARY KEY (paper_id, reference_id)
);
"""
print("Esquema definido")
```

**Notas de diseño:**

- `downloads` almacena el campo *Accesses* reportado por Nature, métrica
  equivalente aceptada por el taller (sección 6.1).
- `citations` corresponde a las citas reportadas por Nature. Para los 5 papers
  sin esta métrica se deja `NULL`.
- `reference_text_normalized` permite identificar referencias duplicadas entre
  papers tras normalizar (minúsculas, sin puntuación, sin URLs).



Como **Nature Machine Intelligence** es una revista especializada en IA, una
clasificación naive llevaría a más del 90% de papers a "Machine Learning". Se
aplicó una **estrategia jerárquica por especificidad**:

1. **IA Generativa** (más específico): LLMs, difusión, GANs, modelos de fundación.
2. **Machine Learning**: aprendizaje supervisado/no supervisado, redes neuronales, RL.
3. **Estadística**: métodos estadísticos puros (Bayesian, MCMC, MLE).
4. **Otros**: lo que no caiga en ninguna anterior.

La búsqueda se hace sobre **título + abstract + subjects** combinados, con regex.

```{python}

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
    r"\b(embedding|representation\s+learning)s?\b",
    r"\b(pretrain|fine-?tun|backbone\s+model)",
    r"\b(artificial\s+intelligence|ai\s+(model|system|agent))\b",
]

STATS_PATTERNS = [
    r"\b(bayesian\s+(inference|model|method|network))\b",
    r"\b(statistical\s+(method|model|test|inference|analysis))\b",
    r"\b(monte\s+carlo|markov\s+chain|mcmc)\b",
    r"\b(maximum\s+likelihood|likelihood\s+estimation)\b",
    r"\b(uncertainty\s+quantification)\b",
    r"\b(probabilistic\s+(model|inference|programming))\b",
    r"\b(stochastic\s+process|gaussian\s+process)\b",
]


def classify_paper(paper):
    text = " ".join([
        paper.get("title", "") or "",
        paper.get("abstract", "") or "",
        " ".join(paper.get("subjects", []) or []),
    ]).lower()
    for pat in GEN_AI_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "IA Generativa"
    for pat in ML_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "Machine Learning"
    for pat in STATS_PATTERNS:
        if re.search(pat, text, re.IGNORECASE):
            return "Estadística"
    return "Otros"


labels = [classify_paper(p) for p in papers]
dist = Counter(labels)
df_dist = pd.DataFrame(
    [(k, v, f"{100*v/len(papers):.1f}%") for k, v in dist.most_common()],
    columns=["Categoría", "N° papers", "Porcentaje"]
)
df_dist
```



```{python}

def normalize_reference(ref_text):
    if not ref_text:
        return ""
    s = ref_text.lower()
    s = re.sub(r"https?://\S+", "", s)
    s = re.sub(r"doi[: ]\S+", "", s)
    s = re.sub(r"[^\w\s]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def parse_year(date_str):
    if not date_str:
        return None
    m = re.search(r"(\d{4})", date_str)
    return int(m.group(1)) if m else None


Path(DB_FILE).unlink(missing_ok=True)
conn = sqlite3.connect(DB_FILE)
cur = conn.cursor()
cur.executescript(SCHEMA)

for p in papers:
    cur.execute("""
        INSERT INTO papers VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    """, (
        p["paper_id"],
        p.get("journal", "Nature Machine Intelligence"),
        p.get("title"),
        p.get("publication_date"),
        parse_year(p.get("publication_date") or p.get("online_date")),
        p.get("doi"),
        p.get("url"),
        p.get("abstract"),
        "; ".join(p.get("authors", [])),
        p.get("n_authors", 0),
        p.get("citations_nature"),
        p.get("accesses"),
        p.get("altmetric"),
        p.get("n_references", 0),
        classify_paper(p),
        p.get("article_type"),
    ))

for p in papers:
    for order, name in enumerate(p.get("authors", []), start=1):
        name = name.strip()
        if not name:
            continue
        cur.execute("INSERT OR IGNORE INTO authors (author_name) VALUES (?)", (name,))
        aid = cur.execute("SELECT author_id FROM authors WHERE author_name = ?", (name,)).fetchone()[0]
        cur.execute("INSERT OR IGNORE INTO paper_authors VALUES (?, ?, ?)",
                    (p["paper_id"], aid, order))

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
        rid = cur.execute(
            "SELECT reference_id FROM refs WHERE reference_text_normalized = ?", (norm,)
        ).fetchone()[0]
        cur.execute("INSERT OR IGNORE INTO paper_refs VALUES (?, ?, ?)",
                    (p["paper_id"], rid, order))

conn.commit()

counts = {
    "papers":        cur.execute("SELECT COUNT(*) FROM papers").fetchone()[0],
    "authors":       cur.execute("SELECT COUNT(*) FROM authors").fetchone()[0],
    "paper_authors": cur.execute("SELECT COUNT(*) FROM paper_authors").fetchone()[0],
    "refs (únicas)": cur.execute("SELECT COUNT(*) FROM refs").fetchone()[0],
    "paper_refs":    cur.execute("SELECT COUNT(*) FROM paper_refs").fetchone()[0],
}
pd.DataFrame(counts.items(), columns=["Tabla", "N° registros"])
```


```{python}
def q(sql):
    return pd.read_sql_query(sql, conn)
```


```{python}
q("SELECT ROUND(AVG(n_authors), 2) AS promedio_autores FROM papers;")
```

**Interpretación:** en promedio cada artículo tiene casi 9 autores, clara muestra de que la investigacion de IA moderna requiere de varios autores que aporten su experticia en diferentes campos para su desarrollo.


```{python}
q("SELECT COUNT(*) AS n_papers_ML FROM papers WHERE topic_label = 'Machine Learning';")
```

**Interpretación:** la mayoría de papers (el 62%) cae en la categoría general de ML.


```{python}
q("SELECT COUNT(*) AS n_papers_IAGen FROM papers WHERE topic_label = 'IA Generativa';")
```

**Interpretación:** los modelos generativos representan casi el 30% de la
producción de la revista en 2025.


```{python}
q("""
    SELECT topic_label, COUNT(*) AS n_papers
    FROM papers
    WHERE topic_label IN ('Estadística', 'Otros')
    GROUP BY topic_label
    ORDER BY n_papers DESC;
""")
```

**Interpretación:** ningún paper se clasifica como puramente estadístico — los
métodos estadísticos en esta revista aparecen integrados con modelos de
aprendizaje, no como objeto principal. Los 12 papers en "Otros" en general
corresponden a robótica pura, biomedicina aplicada y computación cuántica.


```{python}
q("""
    SELECT SUM(downloads) AS total_descargas
    FROM papers
    WHERE year = 2025
      AND downloads IS NOT NULL;
""")
```

**Interpretación:** los artículos publicados en 2025 acumulan más de 2 millones
de visualizaciones. La sección 6.1 del taller acepta "Accesses" como equivalente.

```{python}
q("""
    SELECT ROUND(AVG(n_references), 2) AS promedio_referencias
    FROM papers
    WHERE n_references > 0;
""")
```

**Interpretación:** los artículos tienen en promedio 66 referencias, para ser articulos
relativamente recientes es un numero alto de referencias que muestra la fuerte tendencia en 
los campos academicos hacia la IA Generativa y el Machine Learning.


```{python}
q("""
    SELECT SUBSTR(r.reference_text, 1, 120) || '...' AS referencia,
           COUNT(*) AS veces_citada
    FROM paper_refs pr
    JOIN refs r ON r.reference_id = pr.reference_id
    GROUP BY r.reference_id
    ORDER BY veces_citada DESC
    LIMIT 5;
""")
```

**Interpretación:** la referencia más citada es **ESM-2** de Lin et al con 18 citas. 
Le sigue **AlphaFold** de Jumper et al. con 17 citas y **AlphaFold 3** de Abramson et al. con 13 citas.
Notese que los cuatro primeros artículos citados pertenecen al campo de la biología estructural con un enfoque a la
prediccion de estructuras proteicas usando modelos de lenguaje.


```{python}
q("""
    SELECT ROUND(AVG(citations), 2) AS promedio_citas,
           COUNT(*) AS papers_con_dato
    FROM papers
    WHERE citations IS NOT NULL;
""")
```

**Interpretación:** los papers acumulan en promedio casi 20 citas. Cinco papers
(los más recientes) no tienen contador de citas probablemente por lo reciente de su publicación.


```{python}
q("""
    SELECT title, doi, citations, downloads, topic_label
    FROM papers
    WHERE citations IS NOT NULL
    ORDER BY citations DESC
    LIMIT 1;
""")
```

**Interpretación:** *The design space of E(3)-equivariant atom-centred
interatomic potentials* (Batatia et al.) lidera con 193 citas. Es un paper
fundacional sobre redes neuronales equivariantes para simulación molecular.


```{python}
q("""
    SELECT title, doi, citations, topic_label
    FROM papers
    WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
      AND citations IS NOT NULL
    ORDER BY citations DESC
    LIMIT 1;
""")
```

**Interpretación:** el paper más citado entre las tres categorías target
coincide con el general (Q9), categorizado como Machine Learning.


```{python}
q("""
    SELECT title, doi, downloads, topic_label
    FROM papers
    WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
      AND downloads IS NOT NULL
    ORDER BY downloads DESC
    LIMIT 1;
""")
```

**Interpretación:** *What large language models know and what people think they
know* (Steyvers et al.) tiene 79 000 accesses, casi tres veces más que el más
citado. Los temas sobre LLMs generan más interés del público general que los
temas técnicos especializados, aunque la academia los cite menos.


1. **Distribución temática:** la revista está sesgada hacia ML genérico (62%)
   e IA generativa (29%); el contenido de estadística pura es prácticamente
   inexistente.

2. **Modelos de lenguaje para proteínas dominan las referencias compartidas:**
   ESM-2, AlphaFold y AlphaFold 3 son las tres referencias más citadas. La
   intersección **AI y biología estructural** es uno de los frentes más activos en 2025.

3. **Citas vs accesos no están correlacionados linealmente:** el paper más
   descargado (sobre LLMs) supera al más citado en órdenes de magnitud en
   accesses, pero recibe muchas menos citas académicas.

4. **Colaboración intensiva:** 9 autores promedio sobre 1 207 autores únicos,
   mostrando la naturaleza interdisciplinaria del campo.

5. **Densidad bibliográfica:** 66 referencias promedio por paper; alta unicidad
   (92%), pocas referencias se repiten muchas veces.

**Limitaciones:**

1. **Ausencia de citas para 5 papers:** los más recientes aún no tenían
   contador de citas. Quedaron como `NULL` y se excluyen de Q8–Q10.

2. **Métrica de descargas aproximada:** Nature reporta "Accesses" en formato
   compactado ("10k", "1.2M"). La conversión a entero introduce un error de ±5%.

3. **Clasificación por palabras clave:** aunque reproducible, puede fallar en
   papers con lenguaje específico del campo de aplicación. Una clasificación más robusta
   requeriría un modelo de embeddings (p. ej. SciBERT).

4. **Categoría "Estadística" vacía:** refleja la realidad editorial de la
   revista — los métodos estadísticos aparecen como herramienta dentro de
   modelos ML, no como objeto principal.

5. **Normalización de referencias imperfecta:** podría fallar si dos papers
   citan la misma obra con diferente cantidad de coautores listados. Una
   normalización más robusta requeriría matching difuso o consultas a CrossRef
   por DOI.



Los hallazgos cuantitativos modelos
generativos representando ~30% de la producción, dominio de las referencias a
modelos de lenguaje proteicos, alta colaboración multi-autor, muestran las tendencias actuales de la revista Nature Machine Intelligence
hacia la intersección de IA y biología estructural, ademas de el fuerte interés del público general por los LLMs. 
Sin embargo, la ausencia de papers puramente estadísticos y la clasificación basada en palabras clave son limitaciones a considerar al interpretar estos resultados.

```{python}

conn.close()
```
'''


def main():
    OUT_FILE.write_text(QMD_CONTENT, encoding="utf-8")
    print(f"Archivo generado: {OUT_FILE}")
    print(f"Tamaño: {OUT_FILE.stat().st_size / 1024:.1f} KB")
    print()
    print("Para renderizar:")
    print("  quarto render taller1.qmd")


if __name__ == "__main__":
    main()