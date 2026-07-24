"""Agregación de transacciones a una fila por beneficiario, sin usar CID como predictor."""
from collections import Counter, defaultdict

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from common import (CHUNKSIZE, DATA, FIGURES, cetipo_category, check_data, clean_cid,
                    clean_cost, clean_id, clean_text, ensure_directories, is_positive,
                    mode_or_missing, safe_age, save_csv)

USECOLS = ["CID", "UTI", "INTERNADO", "PORTE_ANESTESICO", "DT_UTILIZACAO",
           "DESC_ESPECIALIDADE", "TIPO_UNIDADE_PREST_HOSPITALAR",
           "UF_CNES_PREST_HOSPITALAR", "DT_NASCIMENTO_BENEFICIARIO",
           "TIPO_BENEFICIARIO", "SEXO_BENEFICIARIO", "CETIPO",
           "CD_PROCEDIMENTO", "VALOR_UTILIZACAO", "CHAVE_FUNCIONAL"]


def new_state() -> dict:
    return {"dorsalgia": 0, "dates": set(), "procedures": set(), "specialties": set(),
            "states": set(), "units": set(), "internado": 0, "uti": 0, "costs": [],
            "birth": Counter(), "sex": Counter(), "beneficiary_type": Counter(),
            "anesthesia": Counter(), "specialty_count": Counter(), "state_count": Counter(),
            "unit_count": Counter(), "cetipo": Counter(), "last_date": pd.NaT}


def most(counter: Counter):
    return sorted(counter.items(), key=lambda item: (-item[1], str(item[0])))[0][0] if counter else pd.NA


def main() -> None:
    print("Iniciando 04_construccion_variables.py...", flush=True)
    ensure_directories(); available = check_data([c for c in USECOLS if c != "PORTE_ANESTESICO"])
    usecols = [c for c in USECOLS if c in available]
    states: dict[str, dict] = defaultdict(new_state)
    print("Leyendo y agregando la base por beneficiario. El primer bloque puede tardar...", flush=True)
    for number, chunk in enumerate(pd.read_csv(DATA, usecols=usecols, chunksize=CHUNKSIZE, low_memory=False), 1):
        ids = clean_id(chunk["CHAVE_FUNCIONAL"]); cid = clean_cid(chunk["CID"])
        dates = pd.to_datetime(chunk["DT_UTILIZACAO"], errors="coerce")
        births = pd.to_datetime(chunk["DT_NASCIMENTO_BENEFICIARIO"], errors="coerce")
        costs = clean_cost(chunk["VALOR_UTILIZACAO"])
        cleaned = {col: clean_text(chunk[col]) for col in ["CD_PROCEDIMENTO", "DESC_ESPECIALIDADE",
                   "TIPO_UNIDADE_PREST_HOSPITALAR", "UF_CNES_PREST_HOSPITALAR",
                   "TIPO_BENEFICIARIO", "SEXO_BENEFICIARIO"]}
        anesthesia = clean_text(chunk["PORTE_ANESTESICO"]) if "PORTE_ANESTESICO" in chunk else pd.Series(pd.NA, index=chunk.index)
        cets = cetipo_category(chunk["CETIPO"])
        interned, uti = is_positive(chunk["INTERNADO"]), is_positive(chunk["UTI"])
        for i, beneficiary in ids.items():
            if pd.isna(beneficiary): continue
            s = states[str(beneficiary)]
            s["dorsalgia"] = max(s["dorsalgia"], int(str(cid.at[i]).startswith("M54")) if pd.notna(cid.at[i]) else 0)
            date = dates.at[i]
            if pd.notna(date): s["dates"].add(date.date()); s["last_date"] = max(s["last_date"], date) if pd.notna(s["last_date"]) else date
            for source, target in [("CD_PROCEDIMENTO", "procedures"), ("DESC_ESPECIALIDADE", "specialties"),
                                   ("UF_CNES_PREST_HOSPITALAR", "states"),
                                   ("TIPO_UNIDADE_PREST_HOSPITALAR", "units")]:
                val = cleaned[source].at[i]
                if pd.notna(val): s[target].add(str(val))
            s["internado"] |= int(interned.at[i]); s["uti"] |= int(uti.at[i])
            if pd.notna(costs.at[i]): s["costs"].append(float(costs.at[i]))
            if pd.notna(births.at[i]): s["birth"][births.at[i]] += 1
            for val, target in [(cleaned["SEXO_BENEFICIARIO"].at[i], "sex"),
                                (cleaned["TIPO_BENEFICIARIO"].at[i], "beneficiary_type"),
                                (anesthesia.at[i], "anesthesia"),
                                (cleaned["DESC_ESPECIALIDADE"].at[i], "specialty_count"),
                                (cleaned["UF_CNES_PREST_HOSPITALAR"].at[i], "state_count"),
                                (cleaned["TIPO_UNIDADE_PREST_HOSPITALAR"].at[i], "unit_count")]:
                if pd.notna(val): s[target][str(val)] += 1
            s["cetipo"][cets.at[i]] += 1
        print(f"Bloque {number}: {len(states):,} beneficiarios", flush=True)

    print("Lectura terminada. Consolidando variables de cada beneficiario...", flush=True)
    rows = []
    total_beneficiaries = len(states)
    for position, (beneficiary, s) in enumerate(states.items(), start=1):
        costs = np.asarray(s["costs"], dtype=float); birth = most(s["birth"])
        if pd.notna(birth) and pd.notna(s["last_date"]):
            age_value = (s["last_date"] - birth).days / 365.2425
            age = round(age_value, 1) if 0 <= age_value <= 110 else np.nan
        else:
            age = np.nan
        rows.append({"CHAVE_FUNCIONAL": beneficiary, "dorsalgia": s["dorsalgia"], "edad": age,
            "sexo": most(s["sex"]), "tipo_beneficiario": most(s["beneficiary_type"]),
            "numero_utilizaciones": len(s["dates"]), "numero_procedimientos": len(s["procedures"]),
            "numero_dias_atencion": len(s["dates"]), "numero_especialidades": len(s["specialties"]),
            "numero_estados_atencion": len(s["states"]), "numero_unidades_hospitalarias": len(s["units"]),
            "tuvo_internacion": s["internado"], "tuvo_uti": s["uti"],
            "porte_anestesico_mas_frecuente": most(s["anesthesia"]),
            "costo_total": costs.sum() if costs.size else np.nan, "costo_promedio": costs.mean() if costs.size else np.nan,
            "costo_mediano": np.median(costs) if costs.size else np.nan, "costo_maximo": costs.max() if costs.size else np.nan,
            "n_consultas_C": s["cetipo"]["C"], "n_examenes_E": s["cetipo"]["E"],
            "n_terapias_T": s["cetipo"]["T"], "n_internaciones_I": s["cetipo"]["I"],
            "n_urgencias_P": s["cetipo"]["P"], "n_otros_O": s["cetipo"]["O"],
            "especialidad_mas_frecuente": most(s["specialty_count"]), "estado_mas_frecuente": most(s["state_count"]),
            "unidad_hospitalaria_mas_frecuente": most(s["unit_count"])})
        if position % 50_000 == 0 or position == total_beneficiaries:
            print(f"Consolidación: {position:,} de {total_beneficiaries:,} beneficiarios", flush=True)
    print("Construyendo y guardando base_beneficiario_dorsalgia.csv...", flush=True)
    base = pd.DataFrame(rows)
    save_csv(base, "base_beneficiario_dorsalgia.csv")
    print("Base principal guardada. Generando resumen y figuras...", flush=True)
    summary = base.describe(include="all").T.reset_index().rename(columns={"index": "variable"})
    save_csv(summary, "resumen_variables_modelo.csv")
    for col, filename in [("numero_utilizaciones", "distribucion_numero_utilizaciones.png"), ("costo_total", "distribucion_costo_total.png")]:
        values = base[col].dropna(); upper = values.quantile(.99) if len(values) else 1
        plt.figure(figsize=(8, 5)); plt.hist(values.clip(upper=upper), bins=50); plt.xlabel(f"{col} (p99)"); plt.ylabel("Beneficiarios")
        plt.tight_layout(); plt.savefig(FIGURES / filename, dpi=150); plt.close()
    print(f"Base de modelamiento creada: {len(base):,} beneficiarios, {base.shape[1]} columnas.", flush=True)


if __name__ == "__main__": main()
