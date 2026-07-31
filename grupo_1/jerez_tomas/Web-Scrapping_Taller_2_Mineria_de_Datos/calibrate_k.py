from __future__ import annotations

import sqlite3
import time
from collections import defaultdict

import numpy as np

from search_engine import (
    BM25Index, LSAIndex, make_analyzer, reciprocal_rank_fusion,
)

DB = "revista_q1_2025.sqlite"
MIN_RELEVANT, MAX_RELEVANT = 3, 30



def load_calibration_data(db_path: str = DB):
    """Devuelve (paper_ids, textos sin subjects, pseudo-consultas)."""
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    rows = conn.execute(
        "SELECT paper_id, title, abstract, subjects_raw FROM papers ORDER BY paper_id"
    ).fetchall()
    conn.close()

    paper_ids, texts = [], []
    subj2papers: dict[str, list[str]] = defaultdict(list)
    for r in rows:
        paper_ids.append(r["paper_id"])
        texts.append(" ".join([r["title"] or ""] * 3 + [r["abstract"] or ""]))
        for s in (r["subjects_raw"] or "").split("; "):
            s = s.strip()
            if s:
                subj2papers[s].append(r["paper_id"])

    queries = {
        s: set(p) for s, p in subj2papers.items()
        if MIN_RELEVANT <= len(p) <= MAX_RELEVANT
    }
    return paper_ids, texts, queries



def precision_at_k(ranked: list[str], relevant: set[str], k: int) -> float:
    if k == 0:
        return 0.0
    return sum(1 for d in ranked[:k] if d in relevant) / k


def recall_at_k(ranked: list[str], relevant: set[str], k: int) -> float:
    if not relevant:
        return 0.0
    return sum(1 for d in ranked[:k] if d in relevant) / len(relevant)


def average_precision(ranked: list[str], relevant: set[str]) -> float:
    if not relevant:
        return 0.0
    hits, total = 0, 0.0
    for i, d in enumerate(ranked, start=1):
        if d in relevant:
            hits += 1
            total += hits / i
    return total / len(relevant)


def reciprocal_rank(ranked: list[str], relevant: set[str]) -> float:
    for i, d in enumerate(ranked, start=1):
        if d in relevant:
            return 1.0 / i
    return 0.0


def ndcg_at_k(ranked: list[str], relevant: set[str], k: int) -> float:
    """nDCG con relevancia binaria."""
    dcg = sum((1.0 / np.log2(i + 1)) for i, d in enumerate(ranked[:k], start=1)
              if d in relevant)
    ideal_hits = min(len(relevant), k)
    idcg = sum(1.0 / np.log2(i + 1) for i in range(1, ideal_hits + 1))
    return float(dcg / idcg) if idcg > 0 else 0.0


def evaluate(rank_fn, queries: dict[str, set[str]], paper_ids: list[str],
             k: int = 5) -> dict[str, float]:
    """Promedia las métricas sobre todas las pseudo-consultas."""
    ids = np.array(paper_ids)
    p, r, ap, rr, nd = [], [], [], [], []
    for q, relevant in queries.items():
        scores = rank_fn(q)
        ranked = ids[np.argsort(-scores, kind="stable")].tolist()
        p.append(precision_at_k(ranked, relevant, k))
        r.append(recall_at_k(ranked, relevant, 10))
        ap.append(average_precision(ranked, relevant))
        rr.append(reciprocal_rank(ranked, relevant))
        nd.append(ndcg_at_k(ranked, relevant, 10))
    return {
        f"P@{k}": float(np.mean(p)),
        "Recall@10": float(np.mean(r)),
        "MAP": float(np.mean(ap)),
        "MRR": float(np.mean(rr)),
        "nDCG@10": float(np.mean(nd)),
    }



def sweep(k_values=(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 137),
          verbose: bool = True):

    paper_ids, texts, queries = load_calibration_data()
    analyzer = make_analyzer(use_stemming=True)

    bm25 = BM25Index().fit(texts, analyzer, min_df=1, max_df=1.0)
    bm25_metrics = evaluate(bm25.score, queries, paper_ids)

    if verbose:
        print(f"Pseudo-consultas: {len(queries)}  |  documentos: {len(paper_ids)}")
        print(f"\nBM25 (referencia, sin reducción): "
              + "  ".join(f"{k}={v:.3f}" for k, v in bm25_metrics.items()))
        print(f"\n{'k':>5} {'var.expl':>9} {'P@5':>7} {'MAP':>7} {'nDCG@10':>8} "
              f"{'MRR':>7} {'t_idx(s)':>9} {'t_qry(ms)':>10} {'RAM(KB)':>9}")
        print("-" * 80)

    records = []
    for k in k_values:
        t0 = time.perf_counter()
        lsa = LSAIndex(n_components=k).fit(texts, analyzer, min_df=2, max_df=0.6)
        t_index = time.perf_counter() - t0

        t0 = time.perf_counter()
        for q in queries:
            lsa.score(q)
        t_query = (time.perf_counter() - t0) / len(queries) * 1000

        m = evaluate(lsa.score, queries, paper_ids)
        mem_kb = lsa.doc_vectors.nbytes / 1024

        rec = {"k": lsa.n_components,
               "explained_variance": lsa.explained_variance_,
               **m, "t_index_s": t_index, "t_query_ms": t_query,
               "mem_kb": mem_kb}
        records.append(rec)

        if verbose:
            print(f"{lsa.n_components:5d} {lsa.explained_variance_*100:8.1f}% "
                  f"{m['P@5']:7.3f} {m['MAP']:7.3f} {m['nDCG@10']:8.3f} "
                  f"{m['MRR']:7.3f} {t_index:9.2f} {t_query:10.2f} {mem_kb:9.1f}")

    return records, bm25_metrics, queries, paper_ids, texts


if __name__ == "__main__":
    records, bm25_metrics, *_ = sweep()

    best_p5 = max(records, key=lambda r: r["P@5"])
    best_map = max(records, key=lambda r: r["MAP"])
    print(f"\nMejor P@5 : k={best_p5['k']} ({best_p5['P@5']:.3f})")
    print(f"Mejor MAP : k={best_map['k']} ({best_map['MAP']:.3f})")
