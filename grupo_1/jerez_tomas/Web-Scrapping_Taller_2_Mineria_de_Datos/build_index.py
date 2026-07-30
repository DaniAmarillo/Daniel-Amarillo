from __future__ import annotations

import argparse
import json
import sqlite3
import time
from pathlib import Path

BASE = Path(__file__).resolve().parent
DB_PATH = BASE / "revista_q1_2025.sqlite"
INDEX_PATH = BASE / "search_index.joblib"


DEFAULT_K = 60


def ensure_subjects_column(db_path: Path, raw_json: Path | None = None) -> bool:
    conn = sqlite3.connect(str(db_path))
    cur = conn.cursor()
    cols = {r[1] for r in cur.execute("PRAGMA table_info(papers)")}
    if "subjects_raw" in cols:
        conn.close()
        return True

    print("  · La columna `subjects_raw` no existe; añadiéndola…")
    cur.execute("ALTER TABLE papers ADD COLUMN subjects_raw TEXT")

    candidates = [raw_json] if raw_json else []
    candidates += [BASE / "papers_raw.json", BASE / "data" / "papers_raw.json"]
    src = next((p for p in candidates if p and p.exists()), None)

    if src is None:
        conn.commit()
        conn.close()
        print("  · No se encontró papers_raw.json: la columna queda vacía "
              "(el índice usará sólo título + resumen).")
        return False

    papers = json.loads(src.read_text(encoding="utf-8"))
    n = 0
    for p in papers:
        subs = "; ".join(p.get("subjects") or [])
        if subs:
            cur.execute("UPDATE papers SET subjects_raw = ? WHERE paper_id = ?",
                        (subs, p["paper_id"]))
            n += cur.rowcount
    conn.commit()
    conn.close()
    print(f"  · Palabras clave cargadas desde {src.name} para {n} artículos.")
    return True


def main() -> None:
    ap = argparse.ArgumentParser(description="Construye el índice de búsqueda.")
    ap.add_argument("--k", type=int, default=DEFAULT_K,
                    help=f"componentes del Truncated SVD (def. {DEFAULT_K})")
    ap.add_argument("--no-stemming", action="store_true",
                    help="desactiva el stemming de Snowball")
    ap.add_argument("--db", type=Path, default=DB_PATH)
    ap.add_argument("--out", type=Path, default=INDEX_PATH)
    ap.add_argument("--evaluate", action="store_true",
                    help="ejecuta la evaluación y guarda evaluation_results.json")
    args = ap.parse_args()

    if not args.db.exists():
        raise SystemExit(f"No se encontró la base de datos: {args.db}")

    print("=" * 70)
    print(" CONSTRUCCIÓN DEL ÍNDICE DE BÚSQUEDA — TALLER 4")
    print("=" * 70)
    print(f" Base de datos : {args.db}")
    print(f" Componentes k : {args.k}")
    print(f" Stemming      : {not args.no_stemming}")
    print()

    print("[1/3] Verificando el esquema de la base…")
    ensure_subjects_column(args.db)

    from search_engine import SearchEngine

    print("[2/3] Construyendo representaciones…")
    t0 = time.perf_counter()
    engine = SearchEngine.build(
        args.db, n_components=args.k, use_stemming=not args.no_stemming
    )
    build_s = time.perf_counter() - t0

    cfg = engine.config
    print(f"  · documentos               : {cfg['n_documents']}")
    print(f"  · vocabulario BM25         : {cfg['bm25_vocabulary']:,}")
    print(f"  · matriz TF-IDF            : {cfg['tfidf_shape'][0]} x "
          f"{cfg['tfidf_shape'][1]}  (densidad {cfg['tfidf_density']*100:.2f}%)")
    print(f"  · componentes LSA          : {cfg['lsa_components']}")
    print(f"  · varianza explicada       : {cfg['lsa_explained_variance']*100:.1f}%")
    print(f"  · reducción dimensional    : {cfg['tfidf_shape'][1]} -> "
          f"{cfg['lsa_components']}  "
          f"({cfg['tfidf_shape'][1]/cfg['lsa_components']:.1f}x)")
    print(f"  · tiempo de construcción   : {build_s:.2f} s")

    print("[3/3] Serializando…")
    path = engine.save(args.out)
    size_mb = path.stat().st_size / 1024 / 1024
    print(f"  · {path.name}  ({size_mb:.2f} MB)")

    t0 = time.perf_counter()
    reloaded = SearchEngine.load(path)
    load_ms = (time.perf_counter() - t0) * 1000
    probe = reloaded.search("protein structure prediction", "hybrid", top_k=1)
    print(f"  · carga verificada en {load_ms:.0f} ms — "
          f"consulta de prueba OK ({len(probe)} resultado)")

    if args.evaluate:
        print("\n" + "=" * 70)
        from evaluation import run_evaluation, print_report, save_report
        report = run_evaluation(reloaded)
        print_report(report)
        out = save_report(report, BASE / "evaluation_results.json")


if __name__ == "__main__":
    main()
