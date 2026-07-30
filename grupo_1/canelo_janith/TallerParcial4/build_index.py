# -*- coding: utf-8 -*-
"""
build_index.py — Precómputo del índice de recuperación de información.

Se ejecuta UNA vez (o cada vez que el scraping agrega artículos):

    python build_index.py

Deja en `index/ir_index.joblib` los vectorizadores entrenados, la matriz
TF-IDF dispersa, la matriz de pesos BM25, el modelo Truncated SVD y la matriz
latente. La aplicación Streamlit solo carga ese archivo: nunca vuelve a
tokenizar el corpus, ni a ajustar el TF-IDF, ni a entrenar el SVD.
"""

import argparse
import sys
from pathlib import Path

import ir

BASE = Path(__file__).parent
DB_POR_DEFECTO = BASE / "tog_q1_2025.sqlite"


def main() -> int:
    p = argparse.ArgumentParser(description="Construye el índice del buscador.")
    p.add_argument("--db", default=str(DB_POR_DEFECTO), help="ruta a la base SQLite")
    p.add_argument("--componentes", type=int, default=ir.SVD_COMPONENTS,
                   help="número de componentes del Truncated SVD")
    p.add_argument("--salida", default=str(ir.INDEX_PATH), help="ruta del índice")
    args = p.parse_args()

    if not Path(args.db).exists():
        print(f"[error] no encuentro la base {args.db}", file=sys.stderr)
        return 1

    print(f"Base            : {args.db}")
    idx = ir.construir_indice(args.db, n_components=args.componentes)
    ruta = ir.guardar_indice(idx, args.salida)
    info = idx["info"]

    print(f"Artículos       : {info['n_docs']}")
    print(f"Vocab. BM25     : {info['vocab_bm25']:,} términos (unigramas)")
    print(f"Vocab. TF-IDF   : {info['vocab_tfidf']:,} términos (uni + bigramas)")
    print(f"Dispersión      : {info['dispersion_tfidf']:.4f} "
          f"({info['nnz_tfidf']:,} entradas no nulas)")
    print(f"Reducción       : {info['dim_original']:,} → {info['dim_reducida']} "
          f"dimensiones ({info['varianza_explicada']:.1%} de varianza)")
    print(f"Tiempo          : {info['segundos_construccion']} s")
    print(f"Índice guardado : {ruta}  "
          f"({ruta.stat().st_size / 1024 / 1024:.2f} MB)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
