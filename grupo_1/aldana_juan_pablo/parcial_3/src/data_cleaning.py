from __future__ import annotations

import numpy as np
import pandas as pd

# ------------------------------------------------------------------
# Tipos de datos objetivo. Se centralizan aquí para que la función de
# carga y cualquier código de validación posterior usen exactamente
# la misma definición.
# ------------------------------------------------------------------
COLUMNAS_TEXTO_CATEGORICO = [
    "SEXO_BENEFICIARIO",
    "TIPO_BENEFICIARIO",
    "UF_CNES_PREST_HOSPITALAR",
    "DESC_ESPECIALIDADE",
    "TIPO_UNIDADE_PREST_HOSPITALAR",
    "CETIPO",
]

COLUMNAS_TEXTO_LIBRE = [
    "CID",
    "CD_PROCEDIMENTO",
    "DESCRICAO_PROCEDIMENTO",
    "CHAVE_FUNCIONAL",
]

DTYPES_LECTURA = {
    "CID": "string",
    "UTI": "int8",
    "INTERNADO": "int8",
    "PORTE_ANESTESICO": "string",
    "DESC_ESPECIALIDADE": "string",
    "TIPO_UNIDADE_PREST_HOSPITALAR": "string",
    "UF_CNES_PREST_HOSPITALAR": "string",
    "TIPO_BENEFICIARIO": "string",
    "SEXO_BENEFICIARIO": "string",
    "CETIPO": "string",
    "CD_PROCEDIMENTO": "string",
    "DESCRICAO_PROCEDIMENTO": "string",
    "VALOR_UTILIZACAO": "float32",
    "CHAVE_FUNCIONAL": "string",
}

COLUMNAS_FECHA = ["DT_UTILIZACAO", "DT_NASCIMENTO_BENEFICIARIO"]


def cargar_base(ruta_csv: str, nrows: int | None = None) -> pd.DataFrame:
    
    df = pd.read_csv(
        ruta_csv,
        dtype=DTYPES_LECTURA,
        parse_dates=COLUMNAS_FECHA,
        low_memory=False,
        nrows=nrows,
    )
    df.columns = df.columns.str.strip()
    return df


def limpiar_texto(serie: pd.Series, mapa_equivalencias: dict | None = None) -> pd.Series:
    
    limpio = serie.astype("string").str.strip().str.upper()
    if mapa_equivalencias:
        limpio = limpio.replace(mapa_equivalencias)
    return limpio


def limpiar_base(df: pd.DataFrame) -> pd.DataFrame:
    
    df = df.copy()

    # --- CID ---
    df["CID"] = limpiar_texto(df["CID"])

    # --- Sexo: homologar inconsistencia detectada ---
    df["SEXO_BENEFICIARIO"] = limpiar_texto(
        df["SEXO_BENEFICIARIO"],
        mapa_equivalencias={"MASCULINO": "M", "FEMININO": "F"},
    )

    # --- Tipo de beneficiario ---
    df["TIPO_BENEFICIARIO"] = limpiar_texto(df["TIPO_BENEFICIARIO"])

    # --- Estado del prestador: "-" es "no informado", no un NaN real ---
    df["UF_CNES_PREST_HOSPITALAR"] = limpiar_texto(
        df["UF_CNES_PREST_HOSPITALAR"],
        mapa_equivalencias={"-": "NO INFORMADO"},
    )

    # --- Especialidad: unificar NaN y "-" como "NO INFORMADO" ---
    especialidad = df["DESC_ESPECIALIDADE"].astype("string").str.strip()
    df["DESC_ESPECIALIDADE"] = especialidad.fillna("NO INFORMADO").replace(
        "-", "NO INFORMADO"
    )

    # --- Tipo de unidad hospitalaria ---
    unidad = df["TIPO_UNIDADE_PREST_HOSPITALAR"].astype("string").str.strip()
    df["TIPO_UNIDADE_PREST_HOSPITALAR"] = unidad.replace("-", "NO INFORMADO")

    # --- Indicadores de calidad del costo (no se elimina nada aún) ---
    df["COSTO_NEGATIVO"] = (df["VALOR_UTILIZACAO"] < 0).astype("int8")

    q1 = df["VALOR_UTILIZACAO"].quantile(0.25)
    q3 = df["VALOR_UTILIZACAO"].quantile(0.75)
    iqr = q3 - q1
    limite_inferior = q1 - 1.5 * iqr
    limite_superior = q3 + 1.5 * iqr
    df["COSTO_EXTREMO_IQR"] = (
        (df["VALOR_UTILIZACAO"] < limite_inferior)
        | (df["VALOR_UTILIZACAO"] > limite_superior)
    ).astype("int8")

    # --- Conversión final a category para columnas de baja cardinalidad ---
    for columna in COLUMNAS_TEXTO_CATEGORICO:
        df[columna] = df[columna].astype("category")

    return df


def reporte_calidad_datos(df: pd.DataFrame) -> pd.DataFrame:
    
    faltantes = pd.DataFrame(
        {
            "numero_faltantes": df.isna().sum(),
            "porcentaje_faltantes": (df.isna().mean() * 100).round(2),
        }
    ).sort_values("porcentaje_faltantes", ascending=False)
    return faltantes


def detectar_inconsistencias_beneficiario(df: pd.DataFrame) -> pd.DataFrame:
    
    consistencia = df.groupby("CHAVE_FUNCIONAL", observed=True).agg(
        n_sexos=("SEXO_BENEFICIARIO", lambda x: x.nunique(dropna=True)),
        n_fechas_nacimiento=(
            "DT_NASCIMENTO_BENEFICIARIO",
            lambda x: x.nunique(dropna=True),
        ),
        n_tipos_beneficiario=(
            "TIPO_BENEFICIARIO",
            lambda x: x.nunique(dropna=True),
        ),
    )
    return consistencia
