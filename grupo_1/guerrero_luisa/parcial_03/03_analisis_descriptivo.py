"""Análisis descriptivo transaccional y por estado de dorsalgia."""
from collections import Counter, defaultdict

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from common import (CHUNKSIZE, DATA, FIGURES, TABLES, check_data, clean_cost,
                    clean_id, clean_text, ensure_directories, is_positive, safe_age, save_csv)

COLS = ["CHAVE_FUNCIONAL", "DT_UTILIZACAO", "CD_PROCEDIMENTO", "SEXO_BENEFICIARIO",
        "DT_NASCIMENTO_BENEFICIARIO", "TIPO_BENEFICIARIO", "UF_CNES_PREST_HOSPITALAR",
        "DESC_ESPECIALIDADE", "TIPO_UNIDADE_PREST_HOSPITALAR", "INTERNADO", "UTI", "VALOR_UTILIZACAO"]


def main() -> None:
    print("Iniciando 03_analisis_descriptivo.py...", flush=True)
    ensure_directories(); check_data(COLS)
    target_path = TABLES / "variable_objetivo_dorsalgia.csv"
    if not target_path.exists(): raise FileNotFoundError("Ejecute primero 02_variable_objetivo.py.")
    print("Cargando la variable objetivo...", flush=True)
    target = pd.read_csv(target_path, dtype={"CHAVE_FUNCIONAL": "string"})
    target["CHAVE_FUNCIONAL"] = clean_id(target["CHAVE_FUNCIONAL"])
    target_map = target.set_index("CHAVE_FUNCIONAL")["dorsalgia"]
    total = 0; beneficiaries = set(); uses = set(); procedure_codes = set(); procedure_records = 0
    distributions = {name: Counter() for name in ["sexo", "tipo_beneficiario", "estado", "especialidad", "unidad", "internado", "uti"]}
    costs = {0: [], 1: []}; ages = {0: [], 1: []}
    missing = Counter(); inconsistent = {"sexo": defaultdict(set), "nacimiento": defaultdict(set), "tipo": defaultdict(set)}
    print("Leyendo la base transaccional por bloques. El primer bloque puede tardar...", flush=True)
    for number, chunk in enumerate(pd.read_csv(DATA, usecols=COLS, chunksize=CHUNKSIZE, low_memory=False), 1):
        total += len(chunk); ids = clean_id(chunk["CHAVE_FUNCIONAL"]); dates = pd.to_datetime(chunk["DT_UTILIZACAO"], errors="coerce")
        birth = pd.to_datetime(chunk["DT_NASCIMENTO_BENEFICIARIO"], errors="coerce"); y = ids.map(target_map).fillna(0).astype(int)
        beneficiaries.update(ids.dropna()); uses.update(zip(ids.dropna().astype(str), dates.loc[ids.notna()].dt.strftime("%Y-%m-%d").fillna("SIN_FECHA")))
        cleaned_procedures = clean_text(chunk["CD_PROCEDIMENTO"])
        procedure_records += int(cleaned_procedures.notna().sum())
        procedure_codes.update(cleaned_procedures.dropna())
        mapping = {"sexo": "SEXO_BENEFICIARIO", "tipo_beneficiario": "TIPO_BENEFICIARIO", "estado": "UF_CNES_PREST_HOSPITALAR",
                   "especialidad": "DESC_ESPECIALIDADE", "unidad": "TIPO_UNIDADE_PREST_HOSPITALAR"}
        cleaned_text = {}
        for label, col in mapping.items():
            values = clean_text(chunk[col]); missing[col] += int(values.isna().sum())
            cleaned_text[col] = values
            distributions[label].update(zip(values.fillna("FALTANTE"), y))
        for label, flag in [("internado", is_positive(chunk["INTERNADO"])), ("uti", is_positive(chunk["UTI"]))]:
            distributions[label].update(zip(flag.astype(int), y))
        value = clean_cost(chunk["VALOR_UTILIZACAO"]); age = safe_age(birth, dates)
        for group in (0, 1):
            costs[group].extend(value[y.eq(group)].dropna().iloc[::max(1, int(y.eq(group).sum()/10000))].head(10000))
            ages[group].extend(age[y.eq(group)].dropna().iloc[::max(1, int(y.eq(group).sum()/10000))].head(10000))
        for i, key in ids.items():
            if pd.isna(key): continue
            vals = [cleaned_text["SEXO_BENEFICIARIO"].at[i], birth.at[i],
                    cleaned_text["TIPO_BENEFICIARIO"].at[i]]
            for name, val in zip(inconsistent, vals):
                if pd.notna(val): inconsistent[name][str(key)].add(str(val))
        print(f"Bloque {number}: {total:,} registros", flush=True)

    counts = target["dorsalgia"].value_counts().reindex([0, 1], fill_value=0)
    save_csv(pd.DataFrame({"indicador": ["registros", "beneficiarios", "utilizaciones_aproximadas", "registros_con_procedimiento", "codigos_procedimiento_distintos", "porcentaje_dorsalgia"],
                           "valor": [total, len(beneficiaries), len(uses), procedure_records, len(procedure_codes), 100*counts[1]/max(counts.sum(), 1)]}), "resumen_general.csv")
    desc = pd.DataFrame([{"dorsalgia": g, "n_beneficiarios": counts[g], "edad_promedio_muestra": np.mean(ages[g]) if ages[g] else np.nan,
                          "costo_promedio_muestra": np.mean(costs[g]) if costs[g] else np.nan} for g in (0, 1)])
    save_csv(desc, "descriptivo_por_objetivo.csv")
    filenames = {"sexo": "distribucion_sexo.csv", "tipo_beneficiario": "distribucion_tipo_beneficiario.csv",
                 "estado": "distribucion_estado.csv", "especialidad": "distribucion_especialidad.csv"}
    for label, filename in filenames.items():
        rows = [{label: key[0], "dorsalgia": key[1], "n_registros": val} for key, val in distributions[label].items()]
        save_csv(pd.DataFrame(rows), filename)
    # Tablas complementarias exigidas por el análisis, aunque no tengan nombre
    # obligatorio en el enunciado.
    for label, filename in [("unidad", "distribucion_unidad_hospitalaria.csv"),
                            ("internado", "distribucion_internacion.csv"),
                            ("uti", "distribucion_uti.csv")]:
        rows = [{label: key[0], "dorsalgia": key[1], "n_registros": value}
                for key, value in distributions[label].items()]
        save_csv(pd.DataFrame(rows), filename)
    age_rows = []
    for group in (0, 1):
        for interval, count in pd.cut(pd.Series(ages[group]), bins=[0, 18, 30, 45, 60, 75, 110], right=False).value_counts(sort=False).items():
            age_rows.append({"grupo_edad": str(interval), "dorsalgia": group, "n_muestra": count})
    save_csv(pd.DataFrame(age_rows), "distribucion_edad.csv")
    save_csv(pd.DataFrame([{"dorsalgia": g, **pd.Series(costs[g]).describe(percentiles=[.5,.9,.95,.99]).to_dict()} for g in (0,1)]), "resumen_costos_descriptivo.csv")
    save_csv(pd.DataFrame([{"variable": k, "beneficiarios_inconsistentes": sum(len(v)>1 for v in m.values())} for k,m in inconsistent.items()]), "inconsistencias_descriptivo.csv")
    save_csv(pd.DataFrame([{"variable": k, "n_faltantes": v} for k,v in missing.items()]), "faltantes_descriptivo.csv")

    # Figuras basadas en muestras acotadas y conteos completos.
    plt.figure(); plt.boxplot([ages[0], ages[1]], tick_labels=["No", "Sí"], showfliers=False); plt.ylabel("Edad"); plt.tight_layout(); plt.savefig(FIGURES/"edad_por_dorsalgia.png", dpi=150); plt.close()
    for label, filename in [("sexo", "sexo_por_dorsalgia.png"), ("tipo_beneficiario", "tipo_beneficiario_por_dorsalgia.png")]:
        frame = pd.DataFrame([{"categoria": k[0], "dorsalgia": k[1], "n": v} for k,v in distributions[label].items()])
        pivot = frame.pivot_table(index="categoria", columns="dorsalgia", values="n", aggfunc="sum", fill_value=0).head(15)
        pivot.plot(kind="bar", figsize=(9,5)); plt.tight_layout(); plt.savefig(FIGURES/filename, dpi=150); plt.close()
    top = pd.DataFrame([{"categoria": k[0], "dorsalgia": k[1], "n": v} for k,v in distributions["especialidad"].items()]).groupby("categoria")["n"].sum().nlargest(15)
    top.sort_values().plot(kind="barh", figsize=(9,6)); plt.tight_layout(); plt.savefig(FIGURES/"top_especialidades.png", dpi=150); plt.close()
    plt.figure(); plt.bar(["No", "Sí"], [np.mean(costs[0]), np.mean(costs[1])]); plt.ylabel("Costo promedio (muestra)"); plt.tight_layout(); plt.savefig(FIGURES/"costos_por_dorsalgia.png", dpi=150); plt.close()
    plt.figure(); plt.boxplot([costs[0], costs[1]], tick_labels=["No", "Sí"], showfliers=False); plt.ylabel("Costo"); plt.tight_layout(); plt.savefig(FIGURES/"boxplot_costos.png", dpi=150); plt.close()
    print(f"Análisis descriptivo terminado: {total:,} registros y {len(beneficiaries):,} beneficiarios.", flush=True)


if __name__ == "__main__": main()
