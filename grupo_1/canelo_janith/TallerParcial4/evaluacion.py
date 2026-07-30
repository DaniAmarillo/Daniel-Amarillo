# -*- coding: utf-8 -*-
"""
evaluacion.py — Consultas de prueba, juicios de relevancia y métricas.

Los juicios de relevancia son binarios y fueron asignados manualmente leyendo
el título y el resumen de cada candidato. El conjunto de candidatos juzgados se
obtuvo por *pooling*: la unión de los 10 primeros resultados de las cuatro
estrategias implementadas para cada consulta. Cada consulta declara el criterio
con el que se decidió la relevancia, de modo que el juicio sea auditable.

Consecuencia metodológica: al ser un pool y no el corpus completo, Recall@k es
un límite superior optimista (puede haber artículos relevantes que ninguna
estrategia recuperó). Precision@5, MRR y nDCG@5 sí son directamente
comparables entre estrategias, porque se calculan sobre los mismos juicios.

Uso:
    python evaluacion.py            # escribe evaluacion_consultas.{csv,md}
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

import ir

BASE = Path(__file__).parent

# ─────────────────────────────────────────────────────────────────────────────
#  Consultas (las cinco categorías exigidas por el enunciado)
# ─────────────────────────────────────────────────────────────────────────────
CONSULTAS = [
    {
        "id": "C1",
        "tipo": "Términos literales del corpus",
        "texto": "hexahedral mesh extraction",
        "criterio": (
            "Relevante: el aporte central del artículo es generar, extraer o "
            "simplificar mallas (volumétricas, cuadriláteras o triangulares). "
            "No relevante: artículos que solo consumen mallas ya construidas "
            "(renderizado, parametrización, operaciones booleanas)."
        ),
        "relevantes": [18, 19, 28, 78, 149, 154, 175],
    },
    {
        "id": "C2",
        "tipo": "Términos relacionados / sinónimos",
        "texto": "teaching virtual characters to move using learned controllers",
        "criterio": (
            "Relevante: control o animación de personajes/agentes simulados "
            "mediante políticas o modelos aprendidos. No relevante: "
            "controladores de otros dominios (drones, robots de manipulación) "
            "y sistemas interactivos sin política aprendida. Ninguno de los "
            "términos de la consulta («teaching», «learned controllers») "
            "aparece literalmente en los artículos objetivo, que usan "
            "«skills», «policy», «control»."
        ),
        "relevantes": [30, 31, 41, 72, 168, 200],
    },
    {
        "id": "C3",
        "tipo": "Consulta general",
        "texto": "physics-based simulation",
        "criterio": (
            "Relevante: el artículo propone o mejora un método de simulación "
            "física (sólidos, fluidos, granulares, colisiones, integradores) o "
            "un controlador que opera sobre una simulación física."
        ),
        "relevantes": [24, 25, 30, 31, 41, 54, 107, 140, 155, 200, 203],
    },
    {
        "id": "C4",
        "tipo": "Consulta específica",
        "texto": "bounding volumes for signed distance fields",
        "criterio": (
            "Relevante: representaciones implícitas (SDF, superficies "
            "neuronales implícitas, level sets) y las estructuras que acotan o "
            "aceleran su trazado. No relevante: mallas explícitas, "
            "splatting y aplicaciones donde la representación implícita es "
            "solo un medio (p. ej. planeación de impresión 3D)."
        ),
        "relevantes": [6, 13, 59, 61, 198],
    },
    {
        "id": "C5",
        "tipo": "Consulta donde se esperan resultados poco relevantes",
        "texto": "simulating how clothes wrinkle and fold on a moving body",
        "criterio": (
            "Relevante: simulación de telas, prendas o tejidos deformables. "
            "No relevante: dinámica de cuerpos rígidos, fluidos o granulares. "
            "La consulta está redactada con vocabulario coloquial "
            "(«clothes», «body») y «body» es polisémico en este corpus: "
            "designa cuerpos rígidos, no el cuerpo humano."
        ),
        "relevantes": [20, 116, 117, 118, 184, 204],
    },
    {
        "id": "C6",
        "tipo": "Consulta en lenguaje natural (estilo enunciado)",
        "texto": "applications of generative artificial intelligence for creating 3D content",
        "criterio": (
            "Relevante: modelos generativos aplicados a la creación o "
            "edición de contenido tridimensional (mallas, texturas, "
            "materiales, escenas, rigging). No relevante: generación o "
            "edición puramente 2D (imágenes, diseño gráfico, retoque), aunque "
            "también use modelos de difusión."
        ),
        "relevantes": [29, 43, 74, 82, 125, 151, 161, 184],
    },
]

METODOS_EVAL = ["bm25", "lsa", "tfidf", "hibrido"]


# ─────────────────────────────────────────────────────────────────────────────
#  Métricas
# ─────────────────────────────────────────────────────────────────────────────
def precision_at_k(recuperados: list[int], relevantes: set[int], k: int = 5) -> float:
    if k <= 0:
        return 0.0
    top = recuperados[:k]
    return sum(1 for d in top if d in relevantes) / k


def recall_at_k(recuperados: list[int], relevantes: set[int], k: int = 10) -> float:
    if not relevantes:
        return 0.0
    top = recuperados[:k]
    return sum(1 for d in top if d in relevantes) / len(relevantes)


def mrr(recuperados: list[int], relevantes: set[int]) -> float:
    """Recíproco de la posición del primer resultado relevante."""
    for i, d in enumerate(recuperados, 1):
        if d in relevantes:
            return 1.0 / i
    return 0.0


def ndcg_at_k(recuperados: list[int], relevantes: set[int], k: int = 5) -> float:
    """nDCG con ganancia binaria y descuento logarítmico."""
    top = recuperados[:k]
    dcg = sum(
        (1.0 if d in relevantes else 0.0) / np.log2(i + 1)
        for i, d in enumerate(top, 1)
    )
    ideal = min(len(relevantes), k)
    idcg = sum(1.0 / np.log2(i + 1) for i in range(1, ideal + 1))
    return float(dcg / idcg) if idcg else 0.0


# ─────────────────────────────────────────────────────────────────────────────
#  Evaluación
# ─────────────────────────────────────────────────────────────────────────────
def evaluar(idx: dict, consultas=CONSULTAS, metodos=METODOS_EVAL,
            k_lista: int = 10) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Ejecuta todas las consultas con todas las estrategias.

    Devuelve:
      * `metricas`: una fila por (consulta, estrategia) con P@5, MRR, nDCG@5,
        Recall@10 y el tiempo de respuesta en milisegundos.
      * `detalle`: una fila por resultado recuperado, con la marca de
        relevancia, para inspeccionar los rankings uno a uno.
    """
    filas_m, filas_d = [], []
    for c in consultas:
        rel = set(c["relevantes"])
        for m in metodos:
            r = ir.buscar(idx, c["texto"], metodo=m, top_k=k_lista)
            ids = r["paper_id"].tolist() if not r.empty else []
            filas_m.append({
                "consulta": c["id"],
                "tipo": c["tipo"],
                "estrategia": m,
                "P@5": precision_at_k(ids, rel, 5),
                "MRR": mrr(ids, rel),
                "nDCG@5": ndcg_at_k(ids, rel, 5),
                "Recall@10": recall_at_k(ids, rel, 10),
                "ms": round(float(r.attrs.get("ms", 0.0)), 2),
            })
            for _, x in r.iterrows():
                filas_d.append({
                    "consulta": c["id"],
                    "estrategia": m,
                    "posicion": int(x["posicion"]),
                    "paper_id": int(x["paper_id"]),
                    "titulo": x["title"],
                    "puntaje": round(float(x["puntaje"]), 5),
                    "relevante": int(x["paper_id"]) in rel,
                })
    return pd.DataFrame(filas_m), pd.DataFrame(filas_d)


def resumen(metricas: pd.DataFrame) -> pd.DataFrame:
    """Promedio de cada métrica por estrategia."""
    return (
        metricas.groupby("estrategia")[["P@5", "MRR", "nDCG@5", "Recall@10", "ms"]]
        .mean().round(4)
        .reindex(METODOS_EVAL)
    )


def barrido_componentes(db_path: str, valores=(40, 60, 80, 100, 120, 150)) -> pd.DataFrame:
    """
    Reconstruye el índice para varios tamaños del espacio latente y evalúa la
    estrategia LSA en cada uno. Es el criterio cuantitativo con el que se
    escogió SVD_COMPONENTS (además de la varianza explicada).
    """
    filas = []
    for k in valores:
        idx = ir.construir_indice(db_path, n_components=k)
        met, _ = evaluar(idx, metodos=["lsa"])
        filas.append({
            "componentes": idx["info"]["dim_reducida"],
            "varianza_explicada": round(idx["info"]["varianza_explicada"], 4),
            "P@5": round(met["P@5"].mean(), 4),
            "MRR": round(met["MRR"].mean(), 4),
            "nDCG@5": round(met["nDCG@5"].mean(), 4),
            "Recall@10": round(met["Recall@10"].mean(), 4),
            "ms_promedio": round(met["ms"].mean(), 2),
        })
    return pd.DataFrame(filas)


# ─────────────────────────────────────────────────────────────────────────────
#  Reporte en disco
# ─────────────────────────────────────────────────────────────────────────────
def escribir_reporte(idx: dict, carpeta: Path = BASE) -> list[Path]:
    met, det = evaluar(idx)
    res = resumen(met)

    p_csv = carpeta / "evaluacion_consultas.csv"
    p_det = carpeta / "evaluacion_detalle.csv"
    p_md = carpeta / "evaluacion_consultas.md"
    met.to_csv(p_csv, index=False, encoding="utf-8")
    det.to_csv(p_det, index=False, encoding="utf-8")

    lineas = [
        "# Evaluación del buscador — Taller 4",
        "",
        "Corpus: {} artículos de *ACM Transactions on Graphics* "
        "(base `tog_q1_2025.sqlite`).".format(idx["info"]["n_docs"]),
        "",
        "Estrategias: **BM25** (léxica), **LSA** = TF-IDF + Truncated SVD "
        "({} → {} dimensiones, semántica reducida), **TF-IDF** completo "
        "(referencia sin reducción) e **híbrido** BM25+LSA por RRF."
        .format(idx["info"]["dim_original"], idx["info"]["dim_reducida"]),
        "",
        "Juicios de relevancia binarios, asignados manualmente sobre el pool de "
        "los 10 primeros resultados de las cuatro estrategias.",
        "",
        "## Resumen (promedio sobre las {} consultas)".format(len(CONSULTAS)),
        "",
        res.to_markdown(),
        "",
    ]
    for c in CONSULTAS:
        sub = met[met["consulta"] == c["id"]].set_index("estrategia")
        lineas += [
            "## {} · {}".format(c["id"], c["tipo"]),
            "",
            "**Consulta:** «{}»".format(c["texto"]),
            "",
            "**Criterio de relevancia:** {}".format(c["criterio"]),
            "",
            "**Artículos juzgados relevantes (paper_id):** {}".format(
                ", ".join(str(x) for x in c["relevantes"])),
            "",
            sub[["P@5", "MRR", "nDCG@5", "Recall@10", "ms"]].to_markdown(),
            "",
        ]
        for m in METODOS_EVAL:
            d = det[(det["consulta"] == c["id"]) & (det["estrategia"] == m)].head(5)
            lineas.append("*{}* — primeros 5:".format(ir.METODOS[m]))
            lineas.append("")
            for _, x in d.iterrows():
                marca = "✔" if x["relevante"] else "✘"
                lineas.append("{}. {} `[{}]` {} — puntaje {}".format(
                    x["posicion"], marca, x["paper_id"], x["titulo"], x["puntaje"]))
            lineas.append("")

    p_md.write_text("\n".join(lineas), encoding="utf-8")
    return [p_md, p_csv, p_det]


if __name__ == "__main__":
    idx = ir.cargar_indice()
    rutas = escribir_reporte(idx)
    met, _ = evaluar(idx)
    print(resumen(met).to_string())
    for r in rutas:
        print("escrito:", r)
