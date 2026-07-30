from __future__ import annotations

import json
import time
from dataclasses import dataclass, asdict
from pathlib import Path

import numpy as np

from search_engine import SearchEngine, STRATEGIES


@dataclass
class EvalQuery:
    qid: str
    query: str
    kind: str            
    rationale: str       
    relevant: set[str]   
    criterion: str       


EVAL_QUERIES: list[EvalQuery] = [
    EvalQuery(
        qid="Q1",
        query="protein ligand docking with deep learning",
        kind="Términos que aparecen directamente en los artículos",
        rationale=(
            "Todos los términos de la consulta ('protein', 'ligand', 'docking', "
            "'deep learning') aparecen literalmente en varios títulos y resúmenes. "
            "Es el escenario favorable para la recuperación léxica."
        ),
        criterion=(
            "Relevante = el artículo trata sobre acoplamiento molecular "
            "(docking) proteína–ligando o proteína–péptido, o sobre la "
            "puntuación/optimización de esos complejos."
        ),
        relevant={
            "s42256-025-01160-1",  # Assessing deep learning for protein–ligand docking
            "s42256-025-01091-x",  # Informed protein–ligand docking via geodesic guidance
            "s42256-025-00993-0",  # Benchmarking AI-powered docking methods
            "s42256-025-01077-9",  # Protein–peptide docking with diffusion model
            "s42256-025-01136-1",  # Graph learning for scoring protein–peptide complexes
            "s42256-025-00997-w",  # Deep lead optimization in protein pocket
        },
    ),
    EvalQuery(
        qid="Q2",
        query="making AI decisions transparent and understandable",
        kind="Términos relacionados o sinónimos",
        rationale=(
            "La consulta describe la interpretabilidad sin usar el vocabulario "
            "del dominio: no contiene 'explainable', 'interpretable', 'XAI' ni "
            "'attribution'. Obliga a los métodos a salvar la brecha léxica."
        ),
        criterion=(
            "Relevante = el artículo tiene como objeto de estudio explicar, "
            "auditar o calibrar la confianza en modelos de IA."
        ),
        relevant={
            "s42256-025-00998-9",  # Sensitive and decisive patterns in explainable AI
            "s42256-025-01000-2",  # Explainable AI reveals Clever Hans effects
            "s42256-025-01084-w",  # SemanticLens: mechanistic understanding
            "s42256-025-01111-w",  # Interpretable model explainer
            "s42256-025-01086-8",  # Error-controlled interaction discovery
            "s42256-025-01083-x",  # Interpretable 3D tracking
            "s42256-024-00976-7",  # What LLMs know vs what people think they know
            "s42256-025-01113-8",  # LLMs cannot distinguish belief from knowledge
        },
    ),
    EvalQuery(
        qid="Q3",
        query="drug discovery",
        kind="Consulta general",
        rationale=(
            "Dos palabras, sin restricciones metodológicas. Cubre una línea "
            "temática amplia y muy representada en la revista, por lo que hay "
            "muchos documentos relevantes y el reto está en ordenarlos bien."
        ),
        criterion=(
            "Relevante = el artículo aborda alguna etapa del descubrimiento de "
            "fármacos: diseño de moléculas, cribado virtual, acoplamiento, "
            "predicción de afinidad, optimización de leads o reposicionamiento."
        ),
        relevant={
            "s42256-025-00991-2",  # Federated learning in drug discovery
            "s42256-025-01154-z",  # RNA–ligand binding specificity
            "s42256-025-00997-w",  # Deep lead optimization
            "s42256-025-01095-7",  # ED2Mol de novo molecular design
            "s42256-025-01030-w",  # 3D small binding molecules with diffusion
            "s42256-025-01091-x",  # Protein–ligand docking
            "s42256-025-01151-2",  # Binding affinity for polypharmacology
            "s42256-025-01077-9",  # Protein–peptide docking
            "s42256-025-01124-5",  # Data bias in binding affinity prediction
            "s42256-025-01082-y",  # De novo design of drug candidates
            "s42256-025-00987-y",  # Drug repurposing
            "s42256-025-01160-1",  # Deep learning for protein–ligand docking
            "s42256-025-00993-0",  # Benchmarking docking / virtual screening
            "s42256-025-00982-3",  # SketchMol: image-based molecule design
        },
    ),
    EvalQuery(
        qid="Q4",
        query="E(3)-equivariant interatomic potentials for atomistic simulation",
        kind="Consulta específica",
        rationale=(
            "Terminología técnica de una subárea muy concreta (potenciales "
            "interatómicos aprendidos para simulación de materiales). Sólo un "
            "puñado de artículos del corpus la abordan."
        ),
        criterion=(
            "Relevante = el artículo construye o evalúa potenciales "
            "interatómicos / campos de fuerza aprendidos, o simulación "
            "atomística de materiales."
        ),
        relevant={
            "s42256-024-00956-x",  # Design space of E(3)-equivariant potentials
            "s42256-025-01009-7",  # ML force-field for liquid electrolytes
            "s42256-025-01125-4",  # Flow matching for atomic transport
            "s42256-025-01055-1",  # Evaluating ML crystal stability predictions
        },
    ),
    EvalQuery(
        qid="Q5",
        query="software that writes and understands human text",
        kind="Consulta en la que los métodos producen resultados poco relevantes",
        rationale=(
            "Describe modelos de lenguaje en términos completamente coloquiales. "
            "La palabra 'text' actúa como trampa léxica: en este corpus aparece "
            "sobre todo en artículos de biología computacional que usan texto "
            "como MODALIDAD auxiliar (diseño de proteínas guiado por texto, "
            "anotaciones textuales, supervisión texto–imagen), no en los "
            "artículos que realmente estudian modelos de lenguaje."
        ),
        criterion=(
            "Relevante = el artículo estudia modelos de lenguaje en su "
            "capacidad lingüística o cognitiva (conocimiento, comportamiento, "
            "escalamiento, representaciones). Se EXCLUYEN los que sólo aplican "
            "un LLM como herramienta a otro dominio (química, célula única)."
        ),
        relevant={
            "s42256-025-01115-6",  # Psychometric framework for LLM personality
            "s42256-024-00976-7",  # What LLMs know vs what people think
            "s42256-025-01113-8",  # Belief vs knowledge
            "s42256-025-01049-z",  # Object concept representations in LLMs
            "s42256-025-01137-0",  # Densing law of LLMs
            "s42256-024-00963-y",  # Visual cognition in multimodal LLMs
            "s42256-025-00986-z",  # LLMs replacing human participants
            "s42256-025-01072-0",  # Brain representations aligned with LLMs
            "s42256-025-01033-7",  # Lossless data compression by large models
            "s42256-024-00975-8",  # Evolutionary optimization of model merging
        },
    ),
]



def precision_at_k(ranked: list[str], relevant: set[str], k: int = 5) -> float:
    """Fracción de documentos relevantes entre los primeros k."""
    if k == 0:
        return 0.0
    return sum(1 for d in ranked[:k] if d in relevant) / k


def recall_at_k(ranked: list[str], relevant: set[str], k: int = 10) -> float:
    if not relevant:
        return 0.0
    return sum(1 for d in ranked[:k] if d in relevant) / len(relevant)


def reciprocal_rank(ranked: list[str], relevant: set[str]) -> float:
    for i, d in enumerate(ranked, start=1):
        if d in relevant:
            return 1.0 / i
    return 0.0


def average_precision(ranked: list[str], relevant: set[str], k: int = 10) -> float:
    if not relevant:
        return 0.0
    hits, acc = 0, 0.0
    for i, d in enumerate(ranked[:k], start=1):
        if d in relevant:
            hits += 1
            acc += hits / i
    return acc / min(len(relevant), k)


def ndcg_at_k(ranked: list[str], relevant: set[str], k: int = 10) -> float:
    dcg = sum(1.0 / np.log2(i + 1)
              for i, d in enumerate(ranked[:k], start=1) if d in relevant)
    ideal = min(len(relevant), k)
    idcg = sum(1.0 / np.log2(i + 1) for i in range(1, ideal + 1))
    return float(dcg / idcg) if idcg else 0.0



def run_evaluation(engine: SearchEngine, top_k: int = 10) -> dict:
    per_query, timings = [], {s: [] for s in STRATEGIES}

    for eq in EVAL_QUERIES:
        row = {"qid": eq.qid, "query": eq.query, "kind": eq.kind,
               "n_relevant": len(eq.relevant), "strategies": {}}
        for strat in STRATEGIES:
            t0 = time.perf_counter()
            results = engine.search(eq.query, strat, top_k=top_k)
            elapsed_ms = (time.perf_counter() - t0) * 1000
            timings[strat].append(elapsed_ms)

            ranked = [r.paper_id for r in results]
            row["strategies"][strat] = {
                "P@5": precision_at_k(ranked, eq.relevant, 5),
                "P@10": precision_at_k(ranked, eq.relevant, 10),
                "Recall@10": recall_at_k(ranked, eq.relevant, 10),
                "MRR": reciprocal_rank(ranked, eq.relevant),
                "AP@10": average_precision(ranked, eq.relevant, 10),
                "nDCG@10": ndcg_at_k(ranked, eq.relevant, 10),
                "latency_ms": elapsed_ms,
                "top5": [
                    {"rank": r.rank, "paper_id": r.paper_id, "title": r.title,
                     "score": round(r.score, 4),
                     "relevant": r.paper_id in eq.relevant}
                    for r in results[:5]
                ],
            }
        per_query.append(row)

    summary = {}
    for strat in STRATEGIES:
        metrics = ["P@5", "P@10", "Recall@10", "MRR", "AP@10", "nDCG@10"]
        summary[strat] = {
            m: float(np.mean([q["strategies"][strat][m] for q in per_query]))
            for m in metrics
        }
        summary[strat]["MAP@10"] = summary[strat].pop("AP@10")
        summary[strat]["latency_ms_mean"] = float(np.mean(timings[strat]))

    return {"per_query": per_query, "summary": summary,
            "config": engine.config, "top_k": top_k}


def print_report(report: dict) -> None:
    print("=" * 78)
    print(" EVALUACIÓN DE ESTRATEGIAS DE RECUPERACIÓN")
    print("=" * 78)

    for q in report["per_query"]:
        print(f"\n{q['qid']} — {q['kind']}")
        print(f"  consulta : {q['query']!r}")
        print(f"  relevantes en el corpus: {q['n_relevant']}")
        print(f"  {'estrategia':<34} {'P@5':>6} {'R@10':>6} {'MRR':>6} {'nDCG':>6}")
        for strat, m in q["strategies"].items():
            print(f"  {STRATEGIES[strat]:<34} {m['P@5']:6.2f} {m['Recall@10']:6.2f} "
                  f"{m['MRR']:6.2f} {m['nDCG@10']:6.2f}")

    print("\n" + "=" * 78)
    print(" PROMEDIO SOBRE LAS 5 CONSULTAS")
    print("=" * 78)
    print(f"{'estrategia':<34} {'P@5':>6} {'P@10':>6} {'R@10':>6} "
          f"{'MAP':>6} {'MRR':>6} {'nDCG':>6} {'ms':>7}")
    for strat, m in report["summary"].items():
        print(f"{STRATEGIES[strat]:<34} {m['P@5']:6.3f} {m['P@10']:6.3f} "
              f"{m['Recall@10']:6.3f} {m['MAP@10']:6.3f} {m['MRR']:6.3f} "
              f"{m['nDCG@10']:6.3f} {m['latency_ms_mean']:7.2f}")


def save_report(report: dict, path: str | Path = "evaluation_results.json") -> Path:
    path = Path(path)
    payload = dict(report)
    payload["queries"] = [
        {**{k: v for k, v in asdict(eq).items() if k != "relevant"},
         "relevant": sorted(eq.relevant)}
        for eq in EVAL_QUERIES
    ]
    path.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")
    return path


if __name__ == "__main__":
    import sys
    art = Path("search_index.joblib")
    engine = (SearchEngine.load(art) if art.exists()
              else SearchEngine.build("revista_q1_2025.sqlite", n_components=60))
    report = run_evaluation(engine)
    print_report(report)
    out = save_report(report)
    print(f"\nResultados guardados en: {out}")
