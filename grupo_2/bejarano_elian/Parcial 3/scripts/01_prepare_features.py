from __future__ import annotations

import argparse
import os
from pathlib import Path

import duckdb
import pandas as pd


PROJECT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_INPUT = Path(os.environ.get("DB_2026_PATH", r"C:\Users\stive\Downloads\db_2026.csv"))


def sql_path(path: Path) -> str:
    return path.as_posix().replace("'", "''")


def write_df(df: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(path, index=False, encoding="utf-8")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Prepare Taller 3 features with DuckDB.")
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--project-dir", type=Path, default=PROJECT_DIR)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    project_dir = args.project_dir
    input_path = args.input
    out_data = project_dir / "data" / "processed"
    out_tables = project_dir / "output" / "tablas"
    out_data.mkdir(parents=True, exist_ok=True)
    out_tables.mkdir(parents=True, exist_ok=True)

    if not input_path.exists():
        raise FileNotFoundError(input_path)

    con = duckdb.connect()
    con.execute("PRAGMA threads=4")
    csv_path = sql_path(input_path)

    print(f"Input: {input_path}")
    print("Creating DuckDB views over the CSV...")

    con.execute(
        f"""
        CREATE OR REPLACE VIEW raw AS
        SELECT *
        FROM read_csv('{csv_path}', header=true, all_varchar=true, ignore_errors=true)
        """
    )

    missing = "('', '-', 'N/A', 'NA', 'NAN', 'NONE', 'NULL')"
    con.execute(
        f"""
        CREATE OR REPLACE VIEW base AS
        SELECT
            nullif(trim(CHAVE_FUNCIONAL), '') AS CHAVE_FUNCIONAL,
            CASE WHEN upper(trim(CID)) IN {missing} THEN NULL ELSE upper(trim(CID)) END AS cid_clean,
            regexp_matches(CASE WHEN upper(trim(CID)) IN {missing} THEN '' ELSE upper(trim(CID)) END, '^I2[0-5]') AS is_cardio,
            CASE WHEN upper(trim(DT_UTILIZACAO)) IN {missing} THEN NULL ELSE trim(DT_UTILIZACAO) END AS dt_utilizacao_clean,
            try_cast(CASE WHEN upper(trim(DT_UTILIZACAO)) IN {missing} THEN NULL ELSE trim(DT_UTILIZACAO) END AS DATE) AS dt_utilizacao_date,
            CASE WHEN upper(trim(DT_NASCIMENTO_BENEFICIARIO)) IN {missing} THEN NULL ELSE trim(DT_NASCIMENTO_BENEFICIARIO) END AS nascimento_clean,
            try_cast(CASE WHEN upper(trim(DT_NASCIMENTO_BENEFICIARIO)) IN {missing} THEN NULL ELSE trim(DT_NASCIMENTO_BENEFICIARIO) END AS DATE) AS nascimento_date,
            CASE WHEN upper(trim(SEXO_BENEFICIARIO)) IN {missing} THEN NULL ELSE trim(SEXO_BENEFICIARIO) END AS sexo_clean,
            CASE WHEN upper(trim(TIPO_BENEFICIARIO)) IN {missing} THEN NULL ELSE trim(TIPO_BENEFICIARIO) END AS tipo_beneficiario_clean,
            CASE WHEN upper(trim(DESC_ESPECIALIDADE)) IN {missing} THEN NULL ELSE trim(DESC_ESPECIALIDADE) END AS especialidade_clean,
            CASE WHEN upper(trim(TIPO_UNIDADE_PREST_HOSPITALAR)) IN {missing} THEN NULL ELSE trim(TIPO_UNIDADE_PREST_HOSPITALAR) END AS tipo_unidad_clean,
            CASE WHEN upper(trim(UF_CNES_PREST_HOSPITALAR)) IN {missing} THEN NULL ELSE trim(UF_CNES_PREST_HOSPITALAR) END AS estado_clean,
            CASE WHEN upper(trim(CETIPO)) IN {missing} THEN NULL ELSE trim(CETIPO) END AS cetipo_clean,
            CASE
                WHEN lower(trim(CETIPO)) = 'exame' THEN 'exame'
                WHEN lower(trim(CETIPO)) = 'consulta' THEN 'consulta'
                WHEN lower(trim(CETIPO)) = 'terapia' THEN 'terapia'
                WHEN lower(trim(CETIPO)) LIKE 'interna%' THEN 'internacao'
                WHEN lower(trim(CETIPO)) LIKE 'pronto%' THEN 'pronto_socorro'
                WHEN lower(trim(CETIPO)) = 'outros' THEN 'outros'
                ELSE 'desconocido'
            END AS cetipo_norm,
            CASE WHEN upper(trim(CD_PROCEDIMENTO)) IN {missing} THEN NULL ELSE trim(CD_PROCEDIMENTO) END AS procedimento_clean,
            try_cast(replace(trim(VALOR_UTILIZACAO), ',', '.') AS DOUBLE) AS valor_num,
            coalesce(try_cast(trim(UTI) AS INTEGER), 0) AS uti_num,
            coalesce(try_cast(trim(INTERNADO) AS INTEGER), 0) AS internado_num,
            PORTE_ANESTESICO,
            CID, UTI, INTERNADO, DT_UTILIZACAO, DT_NASCIMENTO_BENEFICIARIO,
            DESC_ESPECIALIDADE, TIPO_UNIDADE_PREST_HOSPITALAR, UF_CNES_PREST_HOSPITALAR,
            TIPO_BENEFICIARIO, SEXO_BENEFICIARIO, CETIPO, CD_PROCEDIMENTO, VALOR_UTILIZACAO
        FROM raw
        WHERE nullif(trim(CHAVE_FUNCIONAL), '') IS NOT NULL
        """
    )

    print("Building beneficiary-level features...")
    features_sql = """
        WITH all_benef AS (
            SELECT DISTINCT CHAVE_FUNCIONAL FROM base
        ),
        target AS (
            SELECT CHAVE_FUNCIONAL, max(CASE WHEN is_cardio THEN 1 ELSE 0 END) AS cardio_isquemica
            FROM base
            GROUP BY CHAVE_FUNCIONAL
        ),
        demo AS (
            SELECT
                CHAVE_FUNCIONAL,
                coalesce(mode(sexo_clean), 'Desconocido') AS sexo_moda,
                coalesce(mode(tipo_beneficiario_clean), 'Desconocido') AS tipo_beneficiario_moda,
                coalesce(mode(nascimento_clean), 'Desconocido') AS fecha_nacimiento_moda,
                count(DISTINCT sexo_clean) AS sexo_n_distintos,
                count(DISTINCT tipo_beneficiario_clean) AS tipo_beneficiario_n_distintos,
                count(DISTINCT nascimento_clean) AS fecha_nacimiento_n_distintas
            FROM base
            GROUP BY CHAVE_FUNCIONAL
        ),
        non_cardio AS (
            SELECT * FROM base WHERE NOT is_cardio
        ),
        agg AS (
            SELECT
                CHAVE_FUNCIONAL,
                count(*) AS n_registros_no_cardio,
                count(DISTINCT dt_utilizacao_clean) AS n_utilizaciones_no_cardio,
                count(DISTINCT procedimento_clean) AS n_procedimientos_distintos_no_cardio,
                count(DISTINCT especialidade_clean) AS n_especialidades_no_cardio,
                count(DISTINCT estado_clean) AS n_estados_no_cardio,
                count(DISTINCT tipo_unidad_clean) AS n_unidades_no_cardio,
                count(DISTINCT cetipo_norm) AS n_cetipos_no_cardio,
                coalesce(sum(valor_num), 0) AS costo_total_no_cardio,
                coalesce(avg(valor_num), 0) AS costo_promedio_no_cardio,
                coalesce(stddev_samp(valor_num), 0) AS costo_sd_no_cardio,
                coalesce(max(valor_num), 0) AS costo_max_no_cardio,
                coalesce(min(valor_num), 0) AS costo_min_no_cardio,
                coalesce(sum(uti_num), 0) AS n_uti_no_cardio,
                coalesce(max(uti_num), 0) AS paso_uti_no_cardio,
                coalesce(sum(internado_num), 0) AS n_internado_no_cardio,
                coalesce(max(internado_num), 0) AS paso_internado_no_cardio,
                coalesce(avg(uti_num), 0) AS tasa_uti_no_cardio,
                coalesce(avg(internado_num), 0) AS tasa_internado_no_cardio,
                coalesce(mode(estado_clean), 'Desconocido') AS estado_moda,
                coalesce(mode(especialidade_clean), 'Desconocido') AS especialidad_moda,
                coalesce(mode(tipo_unidad_clean), 'Desconocido') AS tipo_unidad_moda,
                coalesce(mode(cetipo_clean), 'Desconocido') AS cetipo_moda,
                sum(CASE WHEN cetipo_norm = 'consulta' THEN 1 ELSE 0 END) AS n_cetipo_consulta,
                sum(CASE WHEN cetipo_norm = 'exame' THEN 1 ELSE 0 END) AS n_cetipo_exame,
                sum(CASE WHEN cetipo_norm = 'terapia' THEN 1 ELSE 0 END) AS n_cetipo_terapia,
                sum(CASE WHEN cetipo_norm = 'internacao' THEN 1 ELSE 0 END) AS n_cetipo_internacao,
                sum(CASE WHEN cetipo_norm = 'pronto_socorro' THEN 1 ELSE 0 END) AS n_cetipo_pronto_socorro,
                sum(CASE WHEN cetipo_norm = 'outros' THEN 1 ELSE 0 END) AS n_cetipo_outros
            FROM non_cardio
            GROUP BY CHAVE_FUNCIONAL
        ),
        ref AS (
            SELECT max(dt_utilizacao_date) AS ref_date FROM base
        )
        SELECT
            b.CHAVE_FUNCIONAL,
            coalesce(t.cardio_isquemica, 0) AS cardio_isquemica,
            coalesce(a.n_registros_no_cardio, 0) AS n_registros_no_cardio,
            coalesce(a.n_utilizaciones_no_cardio, 0) AS n_utilizaciones_no_cardio,
            coalesce(a.n_procedimientos_distintos_no_cardio, 0) AS n_procedimientos_distintos_no_cardio,
            coalesce(a.n_especialidades_no_cardio, 0) AS n_especialidades_no_cardio,
            coalesce(a.n_estados_no_cardio, 0) AS n_estados_no_cardio,
            coalesce(a.n_unidades_no_cardio, 0) AS n_unidades_no_cardio,
            coalesce(a.n_cetipos_no_cardio, 0) AS n_cetipos_no_cardio,
            coalesce(a.costo_total_no_cardio, 0) AS costo_total_no_cardio,
            coalesce(a.costo_promedio_no_cardio, 0) AS costo_promedio_no_cardio,
            coalesce(a.costo_sd_no_cardio, 0) AS costo_sd_no_cardio,
            coalesce(a.costo_max_no_cardio, 0) AS costo_max_no_cardio,
            coalesce(a.costo_min_no_cardio, 0) AS costo_min_no_cardio,
            coalesce(a.n_uti_no_cardio, 0) AS n_uti_no_cardio,
            coalesce(a.paso_uti_no_cardio, 0) AS paso_uti_no_cardio,
            coalesce(a.n_internado_no_cardio, 0) AS n_internado_no_cardio,
            coalesce(a.paso_internado_no_cardio, 0) AS paso_internado_no_cardio,
            coalesce(a.tasa_uti_no_cardio, 0) AS tasa_uti_no_cardio,
            coalesce(a.tasa_internado_no_cardio, 0) AS tasa_internado_no_cardio,
            coalesce(a.n_cetipo_consulta, 0) AS n_cetipo_consulta,
            coalesce(a.n_cetipo_exame, 0) AS n_cetipo_exame,
            coalesce(a.n_cetipo_terapia, 0) AS n_cetipo_terapia,
            coalesce(a.n_cetipo_internacao, 0) AS n_cetipo_internacao,
            coalesce(a.n_cetipo_pronto_socorro, 0) AS n_cetipo_pronto_socorro,
            coalesce(a.n_cetipo_outros, 0) AS n_cetipo_outros,
            coalesce(d.sexo_moda, 'Desconocido') AS sexo_moda,
            coalesce(d.tipo_beneficiario_moda, 'Desconocido') AS tipo_beneficiario_moda,
            coalesce(d.fecha_nacimiento_moda, 'Desconocido') AS fecha_nacimiento_moda,
            coalesce(d.sexo_n_distintos, 0) AS sexo_n_distintos,
            coalesce(d.tipo_beneficiario_n_distintos, 0) AS tipo_beneficiario_n_distintos,
            coalesce(d.fecha_nacimiento_n_distintas, 0) AS fecha_nacimiento_n_distintas,
            CASE
                WHEN try_cast(d.fecha_nacimiento_moda AS DATE) IS NULL THEN NULL
                ELSE round(date_diff('day', try_cast(d.fecha_nacimiento_moda AS DATE), ref.ref_date) / 365.25, 2)
            END AS edad_referencia,
            coalesce(a.estado_moda, 'Desconocido') AS estado_moda,
            coalesce(a.especialidad_moda, 'Desconocido') AS especialidad_moda,
            coalesce(a.tipo_unidad_moda, 'Desconocido') AS tipo_unidad_moda,
            coalesce(a.cetipo_moda, 'Desconocido') AS cetipo_moda
        FROM all_benef b
        LEFT JOIN target t USING (CHAVE_FUNCIONAL)
        LEFT JOIN demo d USING (CHAVE_FUNCIONAL)
        LEFT JOIN agg a USING (CHAVE_FUNCIONAL)
        CROSS JOIN ref
    """

    con.execute(f"CREATE OR REPLACE TEMP TABLE features AS {features_sql}")
    features_path = out_data / "features_cardio_beneficiario.csv.gz"
    con.execute(f"COPY features TO '{sql_path(features_path)}' (HEADER, DELIMITER ',', COMPRESSION gzip)")

    print("Writing descriptive tables...")
    con.execute(
        f"""
        COPY (
            SELECT
                CHAVE_FUNCIONAL,
                dt_utilizacao_clean AS DT_UTILIZACAO,
                sum(valor_num) AS costo_utilizacion,
                count(*) AS n_procedimientos,
                max(uti_num) AS uti,
                max(internado_num) AS internado
            FROM base
            WHERE is_cardio
            GROUP BY CHAVE_FUNCIONAL, dt_utilizacao_clean
        ) TO '{sql_path(out_tables / "costos_utilizacion_cardio_i20_i25.csv")}' (HEADER, DELIMITER ',')
        """
    )

    resumen_base = con.execute(
        """
        SELECT 'n_registros' AS metrica, count(*)::VARCHAR AS valor FROM base
        UNION ALL SELECT 'n_beneficiarios', count(DISTINCT CHAVE_FUNCIONAL)::VARCHAR FROM base
        UNION ALL SELECT 'n_beneficiarios_cardio', sum(cardio_isquemica)::VARCHAR FROM features
        UNION ALL SELECT 'prevalencia_cardio', (avg(cardio_isquemica))::VARCHAR FROM features
        UNION ALL SELECT 'n_registros_cardio', sum(CASE WHEN is_cardio THEN 1 ELSE 0 END)::VARCHAR FROM base
        UNION ALL SELECT 'n_utilizaciones', count(DISTINCT CHAVE_FUNCIONAL || '|' || coalesce(dt_utilizacao_clean, ''))::VARCHAR FROM base
        UNION ALL SELECT 'n_utilizaciones_cardio', count(DISTINCT CHAVE_FUNCIONAL || '|' || coalesce(dt_utilizacao_clean, ''))::VARCHAR FROM base WHERE is_cardio
        UNION ALL SELECT 'fecha_min_utilizacion', min(dt_utilizacao_date)::VARCHAR FROM base
        UNION ALL SELECT 'fecha_max_utilizacion', max(dt_utilizacao_date)::VARCHAR FROM base
        """
    ).fetchdf()
    write_df(resumen_base, out_tables / "resumen_base.csv")

    con.execute(
        f"""
        COPY (
            SELECT cardio_isquemica, count(*) AS n_beneficiarios, count(*) / (SELECT count(*) FROM features) AS porcentaje
            FROM features
            GROUP BY cardio_isquemica
            ORDER BY cardio_isquemica
        ) TO '{sql_path(out_tables / "distribucion_objetivo.csv")}' (HEADER, DELIMITER ',')
        """
    )

    missing_wide = con.execute(
        """
        SELECT
            count(*) AS total,
            sum(CASE WHEN cid_clean IS NULL THEN 1 ELSE 0 END) AS CID,
            sum(CASE WHEN dt_utilizacao_clean IS NULL THEN 1 ELSE 0 END) AS DT_UTILIZACAO,
            sum(CASE WHEN nascimento_clean IS NULL THEN 1 ELSE 0 END) AS DT_NASCIMENTO_BENEFICIARIO,
            sum(CASE WHEN sexo_clean IS NULL THEN 1 ELSE 0 END) AS SEXO_BENEFICIARIO,
            sum(CASE WHEN tipo_beneficiario_clean IS NULL THEN 1 ELSE 0 END) AS TIPO_BENEFICIARIO,
            sum(CASE WHEN especialidade_clean IS NULL THEN 1 ELSE 0 END) AS DESC_ESPECIALIDADE,
            sum(CASE WHEN tipo_unidad_clean IS NULL THEN 1 ELSE 0 END) AS TIPO_UNIDADE_PREST_HOSPITALAR,
            sum(CASE WHEN estado_clean IS NULL THEN 1 ELSE 0 END) AS UF_CNES_PREST_HOSPITALAR,
            sum(CASE WHEN cetipo_clean IS NULL THEN 1 ELSE 0 END) AS CETIPO,
            sum(CASE WHEN procedimento_clean IS NULL THEN 1 ELSE 0 END) AS CD_PROCEDIMENTO,
            sum(CASE WHEN valor_num IS NULL THEN 1 ELSE 0 END) AS VALOR_UTILIZACAO
        FROM base
        """
    ).fetchdf().iloc[0]
    total_missing_base = missing_wide["total"]
    missing_df = pd.DataFrame(
        [
            {
                "variable": col,
                "n_faltantes": int(missing_wide[col]),
                "pct_faltantes": float(missing_wide[col] / total_missing_base),
            }
            for col in missing_wide.index
            if col != "total"
        ]
    )
    write_df(missing_df.sort_values("pct_faltantes", ascending=False), out_tables / "valores_faltantes.csv")

    date_quality = con.execute(
        """
        SELECT
            sum(CASE WHEN dt_utilizacao_clean IS NOT NULL AND dt_utilizacao_date IS NULL THEN 1 ELSE 0 END)::DOUBLE AS fechas_utilizacion_invalidas,
            avg(CASE WHEN dt_utilizacao_clean IS NOT NULL AND dt_utilizacao_date IS NULL THEN 1 ELSE 0 END) AS pct_fechas_utilizacion_invalidas,
            sum(CASE WHEN nascimento_clean IS NOT NULL AND nascimento_date IS NULL THEN 1 ELSE 0 END)::DOUBLE AS fechas_nacimiento_invalidas,
            avg(CASE WHEN nascimento_clean IS NOT NULL AND nascimento_date IS NULL THEN 1 ELSE 0 END) AS pct_fechas_nacimiento_invalidas
        FROM base
        """
    ).fetchdf().iloc[0]
    inconsistencias = pd.DataFrame(
        [
            {
                "problema": "fechas_utilizacion_invalidas",
                "n": date_quality["fechas_utilizacion_invalidas"],
                "pct": date_quality["pct_fechas_utilizacion_invalidas"],
            },
            {
                "problema": "fechas_nacimiento_invalidas",
                "n": date_quality["fechas_nacimiento_invalidas"],
                "pct": date_quality["pct_fechas_nacimiento_invalidas"],
            },
            {
                "problema": "beneficiarios_con_mas_de_un_sexo",
                "n": con.execute("SELECT sum(CASE WHEN sexo_n_distintos > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
                "pct": con.execute("SELECT avg(CASE WHEN sexo_n_distintos > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
            },
            {
                "problema": "beneficiarios_con_mas_de_un_tipo",
                "n": con.execute("SELECT sum(CASE WHEN tipo_beneficiario_n_distintos > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
                "pct": con.execute("SELECT avg(CASE WHEN tipo_beneficiario_n_distintos > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
            },
            {
                "problema": "beneficiarios_con_mas_de_una_fecha_nacimiento",
                "n": con.execute("SELECT sum(CASE WHEN fecha_nacimiento_n_distintas > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
                "pct": con.execute("SELECT avg(CASE WHEN fecha_nacimiento_n_distintas > 1 THEN 1 ELSE 0 END) FROM features").fetchone()[0],
            },
        ]
    )
    write_df(inconsistencias, out_tables / "inconsistencias_datos.csv")

    cost_summary = con.execute(
        """
        SELECT
            count(*) AS n_utilizaciones,
            avg(costo_utilizacion) AS media,
            stddev_samp(costo_utilizacion) AS desviacion,
            min(costo_utilizacion) AS minimo,
                approx_quantile(costo_utilizacion, 0.25) AS q1,
                median(costo_utilizacion) AS mediana,
                approx_quantile(costo_utilizacion, 0.75) AS q3,
                approx_quantile(costo_utilizacion, 0.90) AS p90,
                approx_quantile(costo_utilizacion, 0.95) AS p95,
            max(costo_utilizacion) AS maximo
        FROM (
            SELECT CHAVE_FUNCIONAL, dt_utilizacao_clean, sum(valor_num) AS costo_utilizacion
            FROM base
            WHERE is_cardio
            GROUP BY CHAVE_FUNCIONAL, dt_utilizacao_clean
        )
        """
    ).fetchdf()
    write_df(cost_summary, out_tables / "resumen_costos_cardio_i20_i25.csv")

    extremos = con.execute(
        """
        WITH stats AS (
            SELECT
                min(valor_num) AS costo_min,
                approx_quantile(valor_num, 0.25) AS q1,
                median(valor_num) AS mediana,
                approx_quantile(valor_num, 0.75) AS q3,
                max(valor_num) AS costo_max
            FROM base
            WHERE valor_num IS NOT NULL
        ),
        lim AS (
            SELECT *, q3 - q1 AS iqr, q3 + 1.5 * (q3 - q1) AS limite_superior, q1 - 1.5 * (q3 - q1) AS limite_inferior
            FROM stats
        )
        SELECT 'costo_min' AS metrica, costo_min AS valor FROM lim
        UNION ALL SELECT 'costo_q1', q1 FROM lim
        UNION ALL SELECT 'costo_mediana', mediana FROM lim
        UNION ALL SELECT 'costo_q3', q3 FROM lim
        UNION ALL SELECT 'costo_max', costo_max FROM lim
        UNION ALL SELECT 'limite_inferior_iqr', limite_inferior FROM lim
        UNION ALL SELECT 'limite_superior_iqr', limite_superior FROM lim
        UNION ALL SELECT 'n_costos_negativos', (SELECT count(*) FROM base WHERE valor_num < 0) FROM lim
        UNION ALL SELECT 'n_outliers_superiores_iqr', (SELECT count(*) FROM base, lim WHERE valor_num > limite_superior) FROM lim
        """
    ).fetchdf()
    write_df(extremos, out_tables / "valores_extremos_costo.csv")

    for col, filename in [
        ("sexo_moda", "distribucion_sexo_objetivo.csv"),
        ("tipo_beneficiario_moda", "distribucion_tipo_beneficiario_objetivo.csv"),
        ("estado_moda", "distribucion_estado_objetivo.csv"),
        ("especialidad_moda", "distribucion_especialidad_objetivo.csv"),
    ]:
        con.execute(
            f"""
            COPY (
                SELECT {col}, cardio_isquemica, count(*) AS n_beneficiarios
                FROM features
                GROUP BY {col}, cardio_isquemica
                ORDER BY n_beneficiarios DESC
            ) TO '{sql_path(out_tables / filename)}' (HEADER, DELIMITER ',')
            """
        )

    print(con.execute("SELECT count(*) AS n_beneficiarios, sum(cardio_isquemica) AS positivos FROM features").fetchdf())
    print(f"Outputs written to: {project_dir}")


if __name__ == "__main__":
    main()
