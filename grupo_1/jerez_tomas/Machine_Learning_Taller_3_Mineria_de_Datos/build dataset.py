import os
import textwrap
import duckdb
import pandas as pd

CSV_PATH  = "db_2026.csv"      
OUT_DIR   = "outputs"
N_THREADS = 20
MEM_LIMIT = "32GB"              


TARGET_REGEX = "^I1[0-5]"
TARGET_NAME  = "Hipertensión (I10-I15)"


TBL_DIR = os.path.join(OUT_DIR, "tablas")
os.makedirs(TBL_DIR, exist_ok=True)


SENTINELS = "('', '-', 'N/A', 'NA', 'NULL', 'NÃO INFORMADO', 'IGNORADO')"


def clean_cat(col):
    return f"CASE WHEN UPPER(TRIM(COALESCE({col}, ''))) IN {SENTINELS} THEN NULL ELSE {col} END"


CID_CLEAN  = f"CASE WHEN UPPER(TRIM(COALESCE(CID, ''))) IN {SENTINELS} THEN NULL ELSE TRIM(CID) END"
SEXO_CLEAN = "CASE WHEN SEXO_BENEFICIARIO IN ('F','M') THEN SEXO_BENEFICIARIO END"


def main():
    if not os.path.exists(CSV_PATH):
        raise FileNotFoundError(f"No encuentro '{CSV_PATH}'. Ajusta CSV_PATH.")

    con = duckdb.connect()
    con.execute(f"PRAGMA threads={N_THREADS}")
    con.execute(f"PRAGMA memory_limit='{MEM_LIMIT}'")

    con.execute(f"""
        CREATE VIEW tx AS
        SELECT
            CHAVE_FUNCIONAL                               AS id,
            {CID_CLEAN}                                   AS cid,
            TRY_CAST(UTI AS INTEGER)                      AS uti,
            TRY_CAST(INTERNADO AS INTEGER)                AS internado,
            TRY_CAST(DT_UTILIZACAO AS DATE)               AS dt_util,
            TRY_CAST(DT_NASCIMENTO_BENEFICIARIO AS DATE)  AS dob,
            {SEXO_CLEAN}                                  AS sexo,
            {clean_cat('TIPO_BENEFICIARIO')}              AS tipo_benef,
            CETIPO                                        AS cetipo,
            CD_PROCEDIMENTO                               AS cd_proc,
            {clean_cat('DESC_ESPECIALIDADE')}             AS esp,
            {clean_cat('UF_CNES_PREST_HOSPITALAR')}       AS uf,
            {clean_cat('TIPO_UNIDADE_PREST_HOSPITALAR')}  AS unidade,
            TRY_CAST(VALOR_UTILIZACAO AS DOUBLE)          AS valor
        FROM read_csv_auto('{CSV_PATH}', all_varchar=true, header=true)
    """)

    print(">>> Reporte de calidad (leyendo el CSV)...")
    q = con.execute("""
        SELECT
            COUNT(*)                                              AS n_filas,
            COUNT(DISTINCT id)                                    AS n_beneficiarios,
            COUNT(DISTINCT id || '|' || CAST(dt_util AS VARCHAR)) AS n_utilizaciones,
            COUNT(DISTINCT cd_proc)                               AS n_proc_distintos,
            AVG(CASE WHEN cid     IS NULL THEN 1.0 ELSE 0 END)    AS f_cid,
            AVG(CASE WHEN esp     IS NULL THEN 1.0 ELSE 0 END)    AS f_esp,
            AVG(CASE WHEN uf      IS NULL THEN 1.0 ELSE 0 END)    AS f_uf,
            AVG(CASE WHEN unidade IS NULL THEN 1.0 ELSE 0 END)    AS f_unidade,
            AVG(CASE WHEN sexo    IS NULL THEN 1.0 ELSE 0 END)    AS f_sexo,
            AVG(CASE WHEN valor   IS NULL THEN 1.0 ELSE 0 END)    AS f_valor,
            SUM(CASE WHEN valor < 0 THEN 1 ELSE 0 END)           AS n_neg,
            SUM(CASE WHEN valor = 0 THEN 1 ELSE 0 END)           AS n_cero,
            MIN(valor) AS v_min, MAX(valor) AS v_max, AVG(valor) AS v_mean,
            quantile_cont(valor, 0.50)  AS v_p50,
            quantile_cont(valor, 0.99)  AS v_p99,
            quantile_cont(valor, 0.999) AS v_p999,
            MIN(dt_util) AS fecha_min, MAX(dt_util) AS fecha_max
        FROM tx
    """).df().iloc[0]
    print(f"    Filas: {int(q['n_filas']):,}  |  Beneficiarios: {int(q['n_beneficiarios']):,}")

    ref = pd.Timestamp(q["fecha_max"]).strftime("%Y-%m-%d")
    print(f"    Fecha de referencia para edad: {ref}")

    print(f">>> Agregando a nivel de beneficiario (target = {TARGET_NAME})...")
    benef = con.execute(f"""
        SELECT
            id,
            mode(sexo)                                   AS sexo,
            COUNT(DISTINCT sexo)                         AS n_sexos_distintos,
            COUNT(DISTINCT dob)                          AS n_dob_distintos,
            mode(tipo_benef)                             AS tipo_benef,
            floor(date_diff('day', mode(dob), DATE '{ref}') / 365.25) AS edad,
            date_diff('day', min(dt_util), max(dt_util)) AS ventana_dias,
            COUNT(*)                                     AS n_procedimientos,
            COUNT(DISTINCT dt_util)                      AS n_utilizaciones,
            COUNT(DISTINCT cd_proc)                      AS n_proc_distintos,
            COUNT(DISTINCT esp)                          AS n_especialidades,
            COUNT(DISTINCT uf)                           AS n_estados,
            COUNT(DISTINCT unidade)                      AS n_unidades,
            COUNT(*) FILTER (WHERE cetipo = 'Exame')          AS n_exame,
            COUNT(*) FILTER (WHERE cetipo = 'Consulta')       AS n_consulta,
            COUNT(*) FILTER (WHERE cetipo = 'Internação')     AS n_internacao,
            COUNT(*) FILTER (WHERE cetipo = 'Terapia')        AS n_terapia,
            COUNT(*) FILTER (WHERE cetipo = 'Pronto Socorro') AS n_pronto_socorro,
            COUNT(*) FILTER (WHERE cetipo = 'Outros')         AS n_outros,
            COALESCE(SUM(internado), 0)                  AS n_internado,
            COALESCE(SUM(uti), 0)                        AS n_uti,
            CASE WHEN SUM(COALESCE(uti,0)) > 0 THEN 1 ELSE 0 END AS uso_uti,
            SUM(valor)                                   AS costo_total,
            SUM(valor) FILTER (WHERE valor > 0)          AS costo_total_pos,
            AVG(valor)                                   AS costo_promedio,
            median(valor)                                AS costo_mediano,
            MAX(valor)                                   AS costo_max,
            COUNT(*) FILTER (WHERE valor < 0)            AS n_valor_neg,
            MAX(CASE WHEN regexp_matches(cid, '{TARGET_REGEX}') THEN 1 ELSE 0 END) AS target
        FROM tx
        GROUP BY id
    """).df()
    con.close()
    print(f"    Beneficiarios: {benef.shape[0]:,}  |  Columnas: {benef.shape[1]}")

    benef["edad_invalida"] = ((benef["edad"] < 0) | (benef["edad"] > 110)).astype(int)
    benef["edad"] = benef["edad"].clip(lower=0, upper=110)
    benef["sexo"] = benef["sexo"].fillna("Desconocido")
    benef["tipo_benef"] = benef["tipo_benef"].fillna("Desconocido")
    benef["costo_total_pos"] = benef["costo_total_pos"].fillna(0.0)

    prevalencia = benef["target"].mean() * 100
    benef_multi_sexo = int((benef["n_sexos_distintos"] > 1).sum())
    benef_multi_dob  = int((benef["n_dob_distintos"] > 1).sum())

    out_parquet = os.path.join(OUT_DIR, "beneficiarios.parquet")
    benef.to_parquet(out_parquet, index=False)

    reporte = textwrap.dedent(f"""
    REPORTE
    Target: {TARGET_NAME}   (regex {TARGET_REGEX})
    Archivo:                     {CSV_PATH}
    Rango de fechas:             {q['fecha_min']}  ->  {q['fecha_max']}
    Fecha de referencia (edad):  {ref}

    --- Volumen ---
    Filas (procedimientos):      {int(q['n_filas']):,}
    Beneficiarios unicos:        {int(q['n_beneficiarios']):,}
    Utilizaciones (visita-dia):  {int(q['n_utilizaciones']):,}
    Procedimientos distintos:    {int(q['n_proc_distintos']):,}

    --- Variable objetivo ---
    Beneficiarios positivos:     {int(benef['target'].sum()):,}
    PREVALENCIA (target=1):      {prevalencia:.3f}%
    Balance (0/1):               {benef['target'].value_counts().to_dict()}

    --- Faltantes (a nivel transaccion; ahora N/A cuenta como faltante) ---
    CID faltante:                {q['f_cid']*100:.2f}%
    Especialidad faltante:       {q['f_esp']*100:.2f}%
    UF faltante:                 {q['f_uf']*100:.2f}%
    Tipo unidad faltante:        {q['f_unidade']*100:.2f}%
    Sexo faltante:               {q['f_sexo']*100:.2f}%
    Valor faltante:              {q['f_valor']*100:.2f}%

    --- Inconsistencias por beneficiario ---
    Con >1 sexo distinto:        {benef_multi_sexo:,}
    Con >1 fecha nac. distinta:  {benef_multi_dob:,}
    Edades imposibles (<0/>110): {int(benef['edad_invalida'].sum()):,}

    --- VALOR_UTILIZACAO ---
    Min: {q['v_min']:.2f}   Max: {q['v_max']:,.2f}   Media: {q['v_mean']:.2f}
    P50: {q['v_p50']:.2f}   P99: {q['v_p99']:.2f}   P99.9: {q['v_p999']:.2f}
    Negativos: {int(q['n_neg']):,}   Ceros: {int(q['n_cero']):,}
    """)
    with open(os.path.join(TBL_DIR, "reporte_calidad.txt"), "w", encoding="utf-8") as f:
        f.write(reporte)


if __name__ == "__main__":
    main()