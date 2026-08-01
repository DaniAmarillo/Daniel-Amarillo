"""Auditoría reproducible de calidad de la base transaccional."""
from collections import defaultdict
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from common import (CHUNKSIZE, DATA, EXPECTED_COLUMNS, FIGURES, TABLES, check_data,
                    clean_cid, clean_cost, clean_id, clean_text, ensure_directories,
                    save_csv)


def main() -> None:
    ensure_directories()
    available = check_data(["CID", "CHAVE_FUNCIONAL", "VALOR_UTILIZACAO"])
    print("Columnas disponibles:", available)
    missing_expected = sorted(set(EXPECTED_COLUMNS) - set(available))
    save_csv(pd.DataFrame({"columna": available, "esperada": [c in EXPECTED_COLUMNS for c in available]}),
             "auditoria_columnas_disponibles.csv")
    if missing_expected:
        print("ADVERTENCIA - columnas esperadas ausentes:", missing_expected)

    required = [c for c in EXPECTED_COLUMNS if c in available]
    missing_counts = pd.Series(0, index=required, dtype="int64")
    total = 0
    cid_counts = defaultdict(int)
    value_parts, samples = [], []
    # Conjuntos por beneficiario: permiten auditar inconsistencias sin retener transacciones.
    attributes = {c: defaultdict(set) for c in
                  ["SEXO_BENEFICIARIO", "DT_NASCIMENTO_BENEFICIARIO", "TIPO_BENEFICIARIO"]}

    for number, chunk in enumerate(pd.read_csv(DATA, usecols=required, chunksize=CHUNKSIZE,
                                                low_memory=False), 1):
        total += len(chunk)
        ids = clean_id(chunk["CHAVE_FUNCIONAL"]) if "CHAVE_FUNCIONAL" in chunk else pd.Series(pd.NA, index=chunk.index)
        for col in required:
            cleaned = clean_text(chunk[col]) if chunk[col].dtype == object else chunk[col]
            missing_counts[col] += int(cleaned.isna().sum())
        if "CID" in chunk:
            cid = clean_cid(chunk["CID"])
            cid_counts["faltante"] += int(cid.isna().sum())
            cid_counts["formato_valido"] += int(cid.str.match(r"^[A-Z][0-9]{2}(?:\.?[0-9A-Z]{0,4})?$", na=False).sum())
            cid_counts["familia_m54"] += int(cid.str.startswith("M54", na=False).sum())
            cid_counts["inconsistente"] += int((cid.notna() & ~cid.str.match(r"^[A-Z][0-9]{2}(?:\.?[0-9A-Z]{0,4})?$", na=False)).sum())
        if "VALOR_UTILIZACAO" in chunk:
            values = clean_cost(chunk["VALOR_UTILIZACAO"])
            value_parts.append(values.describe(percentiles=[.01, .25, .5, .75, .9, .95, .99]))
            cid_counts["costo_faltante"] += int(values.isna().sum())
            cid_counts["costo_cero"] += int(values.eq(0).sum())
            cid_counts["costo_negativo"] += int(values.lt(0).sum())
            # Muestra determinista acotada para la figura; evita cargar 1,66 GB.
            samples.append(values.dropna().iloc[::max(1, len(values) // 3000)].head(3000))
        for col, mapping in attributes.items():
            if col not in chunk:
                continue
            vals = clean_text(chunk[col])
            for beneficiary, val in zip(ids, vals):
                if pd.notna(beneficiary) and pd.notna(val):
                    mapping[beneficiary].add(str(val))
        print(f"Bloque {number}: {total:,} registros auditados")

    missing_table = pd.DataFrame({"variable": required, "n_faltantes": missing_counts.values})
    missing_table["porcentaje"] = 100 * missing_table["n_faltantes"] / max(total, 1)
    save_csv(missing_table.sort_values("porcentaje", ascending=False), "faltantes_por_variable.csv")
    save_csv(pd.DataFrame([{"categoria": k, "n_registros": v} for k, v in cid_counts.items()
                           if k.startswith(("faltante", "formato", "familia", "inconsistente"))]), "calidad_cid.csv")

    inconsistency_rows = []
    for col, mapping in attributes.items():
        counts = pd.Series({key: len(values) for key, values in mapping.items()})
        inconsistency_rows.append({"variable": col, "beneficiarios_evaluados": len(mapping),
                                   "beneficiarios_inconsistentes": int((counts > 1).sum()),
                                   "criterio": "más de un valor válido observado"})
    save_csv(pd.DataFrame(inconsistency_rows), "inconsistencias_beneficiario.csv")

    # Resumen exacto combinando estadísticos suficientes de cada bloque.
    stats = pd.DataFrame(value_parts)
    valid_n = stats["count"].sum()
    mean = np.average(stats["mean"].fillna(0), weights=stats["count"]) if valid_n else np.nan
    variance_num = sum((row["count"] - 1) * row["std"] ** 2 + row["count"] * (row["mean"] - mean) ** 2
                       for _, row in stats.dropna(subset=["std"]).iterrows())
    summary = pd.DataFrame([{
        "n_registros": total, "n_validos": int(valid_n), "n_faltantes": cid_counts["costo_faltante"],
        "n_cero": cid_counts["costo_cero"], "n_negativos": cid_counts["costo_negativo"],
        "promedio": mean, "desviacion_estandar": np.sqrt(variance_num / max(valid_n - 1, 1)),
        "minimo": stats["min"].min(), "maximo": stats["max"].max(),
    }])
    save_csv(summary, "resumen_valor_utilizacion.csv")

    plt.figure(figsize=(10, 5)); top = missing_table.sort_values("porcentaje", ascending=False)
    plt.bar(top["variable"], top["porcentaje"]); plt.xticks(rotation=75, ha="right")
    plt.ylabel("Porcentaje faltante"); plt.tight_layout(); plt.savefig(FIGURES / "valores_faltantes.png", dpi=150); plt.close()
    sample = pd.concat(samples, ignore_index=True) if samples else pd.Series(dtype=float)
    sample = sample[sample >= 0]
    upper = sample.quantile(.99) if not sample.empty else 1
    plt.figure(figsize=(8, 5)); plt.hist(sample.clip(upper=upper), bins=50)
    plt.xlabel("Valor de utilización (recortado en p99 para visualizar)"); plt.ylabel("Frecuencia")
    plt.tight_layout(); plt.savefig(FIGURES / "distribucion_valor_utilizacion.png", dpi=150); plt.close()
    print(f"Auditoría terminada: {total:,} registros.")


if __name__ == "__main__":
    main()
