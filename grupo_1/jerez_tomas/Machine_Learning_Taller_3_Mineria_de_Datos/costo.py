import os
import duckdb
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf

CSV_PATH    = "db_2026.csv"       
OUT_DIR     = "outputs"
N_THREADS   = 20
MEM_LIMIT   = "32GB"
GLM_SAMPLE  = 300_000             
NEG, POS    = "#9aa0a6", "#7b2d8b"

FIG_DIR = os.path.join(OUT_DIR, "figures")
TBL_DIR = os.path.join(OUT_DIR, "tablas")
os.makedirs(FIG_DIR, exist_ok=True)
os.makedirs(TBL_DIR, exist_ok=True)

SENTINELS = "('', '-', 'N/A', 'NA', 'NULL', 'NÃO INFORMADO', 'IGNORADO')"
CID_CLEAN = f"CASE WHEN UPPER(TRIM(COALESCE(CID, ''))) IN {SENTINELS} THEN NULL ELSE TRIM(CID) END"


def main():
    if not os.path.exists(CSV_PATH):
        raise FileNotFoundError(f"No encuentro '{CSV_PATH}'. Ajusta CSV_PATH.")
    parquet = os.path.join(OUT_DIR, "beneficiarios.parquet")
    if not os.path.exists(parquet):
        raise FileNotFoundError("Falta beneficiarios.parquet. Corre 01 primero.")

    con = duckdb.connect()
    con.execute(f"PRAGMA threads={N_THREADS}")
    con.execute(f"PRAGMA memory_limit='{MEM_LIMIT}'")
    con.execute(f"""
        CREATE VIEW tx AS
        SELECT
            CHAVE_FUNCIONAL                        AS id,
            {CID_CLEAN}                            AS cid,
            CETIPO                                 AS cetipo,
            TRY_CAST(UTI AS INTEGER)               AS uti,
            TRY_CAST(INTERNADO AS INTEGER)         AS internado,
            TRY_CAST(VALOR_UTILIZACAO AS DOUBLE)   AS valor,
            (regexp_matches(
                CASE WHEN UPPER(TRIM(COALESCE(CID,''))) IN {SENTINELS} THEN NULL ELSE TRIM(CID) END,
                '^I1[0-5]'))                       AS htn_line
        FROM read_csv_auto('{CSV_PATH}', all_varchar=true, header=true)
    """)
    con.execute(f"CREATE VIEW ben AS SELECT id, target FROM read_parquet('{parquet}')")

    a1 = con.execute("""
        SELECT COUNT(*) AS n, AVG(valor) AS media, median(valor) AS mediana,
               quantile_cont(valor,0.25) AS p25, quantile_cont(valor,0.75) AS p75
        FROM tx WHERE htn_line AND valor > 0
    """).df().iloc[0]

    a2 = con.execute("""
        SELECT b.target AS beneficiario_hipertenso,
               COUNT(*) AS n_transacciones,
               AVG(t.valor) AS costo_medio,
               median(t.valor) AS costo_mediano
        FROM tx t JOIN ben b USING(id)
        WHERE t.valor > 0
        GROUP BY b.target ORDER BY b.target
    """).df()

    a3 = con.execute("""
        WITH por_benef AS (
            SELECT t.id, b.target, SUM(t.valor) AS costo_benef
            FROM tx t JOIN ben b USING(id)
            WHERE t.valor > 0 GROUP BY t.id, b.target
        )
        SELECT target AS beneficiario_hipertenso,
               AVG(costo_benef) AS costo_total_medio,
               median(costo_benef) AS costo_total_mediano
        FROM por_benef GROUP BY target ORDER BY target
    """).df()

    desc = pd.DataFrame({
        "metrica": ["Utilizacion codificada I10-I15 (media)",
                    "Utilizacion codificada I10-I15 (mediana)"],
        "valor": [a1["media"], a1["mediana"]],
    })
    a2.to_csv(os.path.join(TBL_DIR, "costo_por_tipo_beneficiario.csv"), index=False)
    a3.to_csv(os.path.join(TBL_DIR, "costo_total_por_beneficiario.csv"), index=False)
    desc.to_csv(os.path.join(TBL_DIR, "costo_descriptivo.csv"), index=False)

    print(f"\n-- Utilizacion codificada como I10-I15 (costo > 0): n={int(a1['n']):,}")
    print(f"   media R$ {a1['media']:.2f} | mediana R$ {a1['mediana']:.2f} "
          f"| IQR [{a1['p25']:.2f}, {a1['p75']:.2f}]")
    print("\n-- Costo por transaccion (beneficiario hipertenso vs no):")
    print(a2.to_string(index=False))
    print("\n-- Costo total por beneficiario en la ventana:")
    print(a3.to_string(index=False))

    print(f"\n>>> GLM Gamma sobre muestra de {GLM_SAMPLE:,} transacciones...")
    smp = con.execute(f"""
        SELECT t.valor,
               t.cetipo,
               COALESCE(t.uti, 0)       AS uti,
               COALESCE(t.internado, 0) AS internado,
               CASE WHEN t.htn_line THEN 1 ELSE 0 END AS htn_line,
               b.target                 AS benef_htn
        FROM tx t JOIN ben b USING(id)
        WHERE t.valor > 0 AND t.cetipo IS NOT NULL
        USING SAMPLE {GLM_SAMPLE} ROWS
    """).df()
    con.close()

    smp["cetipo"] = smp["cetipo"].astype("category")
    model = smf.glm(
        "valor ~ C(cetipo) + htn_line + benef_htn + uti + internado",
        data=smp,
        family=sm.families.Gamma(link=sm.families.links.Log()),
    ).fit()

    coef = pd.DataFrame({
        "coef": model.params,
        "efecto_multiplicativo": np.exp(model.params),
        "p_value": model.pvalues,
    })
    coef.to_csv(os.path.join(TBL_DIR, "costo_glm.csv"))
    print("\n-- Efectos del GLM (exp(coef) = factor multiplicativo sobre el costo) --")
    print(coef.round(4).to_string())

    bytype = con2 = duckdb.connect()  
    con2.execute(f"PRAGMA threads={N_THREADS}")
    con2.execute(f"""
        CREATE VIEW tx AS SELECT CHAVE_FUNCIONAL AS id, CETIPO AS cetipo,
            TRY_CAST(VALOR_UTILIZACAO AS DOUBLE) AS valor
        FROM read_csv_auto('{CSV_PATH}', all_varchar=true, header=true)
    """)
    con2.execute(f"CREATE VIEW ben AS SELECT id, target FROM read_parquet('{parquet}')")
    fig_df = con2.execute("""
        SELECT t.cetipo, b.target AS htn, AVG(t.valor) AS costo_medio
        FROM tx t JOIN ben b USING(id)
        WHERE t.valor > 0 AND t.cetipo IS NOT NULL
        GROUP BY t.cetipo, b.target
    """).df()
    con2.close()

    piv = fig_df.pivot(index="cetipo", columns="htn", values="costo_medio")
    piv.columns = ["Sin", "Con"]
    piv = piv.sort_values("Con", ascending=False)
    fig, ax = plt.subplots(figsize=(9, 5))
    piv.plot(kind="bar", ax=ax, color=[NEG, POS])
    ax.set_title("Costo medio por tipo de utilización — beneficiario hipertenso vs no")
    ax.set_ylabel("Costo medio (R$)"); ax.set_xlabel("Tipo de utilización (CETIPO)")
    ax.tick_params(axis="x", rotation=30)
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/14_costo_asociado.png", dpi=130,
                                    bbox_inches="tight")
    plt.close(fig)



if __name__ == "__main__":
    main()