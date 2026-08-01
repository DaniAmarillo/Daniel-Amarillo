from __future__ import annotations

import numpy as np
import pandas as pd

CODIGO_CID_ENFERMEDAD = "A09"  # Diarrea y gastroenteritis de presunto origen infeccioso


def moda_segura(serie: pd.Series):
    """Devuelve el valor más frecuente de una serie, ignorando NaN."""
    valores = serie.dropna()
    if valores.empty:
        return pd.NA
    return valores.mode().iloc[0]


def construir_tabla_demografica(df: pd.DataFrame) -> pd.DataFrame:
    grp = df.groupby("CHAVE_FUNCIONAL", sort=False, observed=True)

    sexo = grp["SEXO_BENEFICIARIO"].agg(moda_segura).rename("SEXO_BENEFICIARIO")
    nacimiento = (
        grp["DT_NASCIMENTO_BENEFICIARIO"].first().rename("DT_NASCIMENTO_BENEFICIARIO")
    )

    idx_ultima = df.groupby("CHAVE_FUNCIONAL", sort=False)["DT_UTILIZACAO"].idxmax()
    tipo_reciente = (
        df.loc[idx_ultima, ["CHAVE_FUNCIONAL", "TIPO_BENEFICIARIO"]]
        .set_index("CHAVE_FUNCIONAL")["TIPO_BENEFICIARIO"]
        .rename("TIPO_BENEFICIARIO")
    )

    beneficiarios = pd.concat([sexo, nacimiento, tipo_reciente], axis=1).reset_index()
    return beneficiarios


def construir_target(
    df: pd.DataFrame, beneficiarios: pd.DataFrame, codigo_cid: str = CODIGO_CID_ENFERMEDAD
) -> pd.DataFrame:
    beneficiarios = beneficiarios.copy()
    registro_evento = df["CID"].str.startswith(codigo_cid, na=False).astype("int8")

    target = (
        df.assign(_registro_evento=registro_evento)
        .groupby("CHAVE_FUNCIONAL", sort=False)["_registro_evento"]
        .max()
        .rename(f"TARGET_{codigo_cid}")
    )

    beneficiarios = beneficiarios.merge(
        target, on="CHAVE_FUNCIONAL", how="left", validate="one_to_one"
    )
    beneficiarios[f"TARGET_{codigo_cid}"] = (
        beneficiarios[f"TARGET_{codigo_cid}"].fillna(0).astype("int8")
    )
    return beneficiarios


def construir_variables_utilizacion(df: pd.DataFrame) -> pd.DataFrame:
    grp = df.groupby("CHAVE_FUNCIONAL", sort=False, observed=True)

    # --- Utilizaciones (evitando doble conteo de procedimientos) ---
    n_utilizaciones = (
        df[["CHAVE_FUNCIONAL", "DT_UTILIZACAO"]]
        .drop_duplicates()
        .groupby("CHAVE_FUNCIONAL", sort=False)
        .size()
        .rename("N_UTILIZACIONES")
    )

    n_procedimientos = (
        df[["CHAVE_FUNCIONAL", "DT_UTILIZACAO", "CD_PROCEDIMENTO"]]
        .drop_duplicates()
        .groupby("CHAVE_FUNCIONAL", sort=False)
        .size()
        .rename("N_PROCEDIMIENTOS")
    )

    # --- Diversidad de especialidades y de estados ---
    n_especialidades = grp["DESC_ESPECIALIDADE"].nunique().rename("N_ESPECIALIDADES")
    n_estados = grp["UF_CNES_PREST_HOSPITALAR"].nunique().rename("N_ESTADOS")

    # --- Internación / UCI (ver advertencia de leakage arriba) ---
    tuvo_internacion = grp["INTERNADO"].max().rename("TUVO_INTERNACION")
    tuvo_uti = grp["UTI"].max().rename("TUVO_UTI")

    # --- Costos ---
    resumen_costos = grp["VALOR_UTILIZACAO"].agg(
        COSTO_TOTAL="sum",
        COSTO_PROMEDIO="mean",
        COSTO_MEDIANO="median",
        COSTO_MAXIMO="max",
        COSTO_DESVEST="std",
    )

    # --- Categorías principales (moda) ---
    estado_principal = grp["UF_CNES_PREST_HOSPITALAR"].agg(moda_segura).rename(
        "ESTADO_PRINCIPAL"
    )
    unidad_principal = grp["TIPO_UNIDADE_PREST_HOSPITALAR"].agg(moda_segura).rename(
        "UNIDAD_PRINCIPAL"
    )
    especialidad_principal = grp["DESC_ESPECIALIDADE"].agg(moda_segura).rename(
        "ESPECIALIDAD_PRINCIPAL"
    )

    variables = pd.concat(
        [
            n_utilizaciones,
            n_procedimientos,
            n_especialidades,
            n_estados,
            tuvo_internacion,
            tuvo_uti,
            resumen_costos,
            estado_principal,
            unidad_principal,
            especialidad_principal,
        ],
        axis=1,
    ).reset_index()

    # Rellenar NaN estructurales: COSTO_DESVEST es NaN cuando el
    # beneficiario tiene una sola transacción (desviación no definida).
    variables["COSTO_DESVEST"] = variables["COSTO_DESVEST"].fillna(0.0)

    return variables


def calcular_edad(
    beneficiarios: pd.DataFrame,
    fecha_referencia: pd.Timestamp,
    columna_nacimiento: str = "DT_NASCIMENTO_BENEFICIARIO",
) -> pd.Series:
   
    edad = (
        (fecha_referencia - beneficiarios[columna_nacimiento]).dt.days / 365.25
    )
    edad = np.floor(edad)
    edad = edad.where((edad >= 0) & (edad <= 110), np.nan)
    return edad


def agrupar_categoria_poco_frecuente(
    serie: pd.Series, umbral_minimo: int, etiqueta_otros: str = "OTROS"
) -> pd.Series:
    
    conteos = serie.value_counts()
    categorias_frecuentes = conteos[conteos >= umbral_minimo].index

    
    serie_object = serie.astype("object")
    resultado = serie_object.where(serie_object.isin(categorias_frecuentes), etiqueta_otros)
    return resultado.astype("string")
