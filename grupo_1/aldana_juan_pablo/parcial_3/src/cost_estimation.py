from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder


def construir_tabla_utilizaciones(df: pd.DataFrame, codigo_cid: str = "A09") -> pd.DataFrame:
    
    columnas_duplicado = ["CHAVE_FUNCIONAL", "DT_UTILIZACAO", "CD_PROCEDIMENTO", "VALOR_UTILIZACAO"]
    df_costos = df.drop_duplicates(subset=columnas_duplicado)

    registro_evento = df["CID"].str.startswith(codigo_cid, na=False).astype("int8")
    target_utilizacion = (
        df.assign(_evento=registro_evento)
        .groupby(["CHAVE_FUNCIONAL", "DT_UTILIZACAO"], sort=False)["_evento"]
        .max()
        .rename(f"UTILIZACION_{codigo_cid}")
    )

    def moda_segura(serie):
        valores = serie.dropna()
        return valores.mode().iloc[0] if not valores.empty else pd.NA

    resumen_costo = df_costos.groupby(
        ["CHAVE_FUNCIONAL", "DT_UTILIZACAO"], sort=False
    ).agg(
        COSTO_UTILIZACION=("VALOR_UTILIZACAO", "sum"),
        N_PROCEDIMIENTOS_UTILIZACION=("CD_PROCEDIMENTO", "nunique"),
    )

    resumen_clinico = df.groupby(["CHAVE_FUNCIONAL", "DT_UTILIZACAO"], sort=False).agg(
        INTERNADO_UTILIZACION=("INTERNADO", "max"),
        UTI_UTILIZACION=("UTI", "max"),
        N_ESPECIALIDADES_UTILIZACION=("DESC_ESPECIALIDADE", "nunique"),
        ESTADO_UTILIZACION=("UF_CNES_PREST_HOSPITALAR", moda_segura),
        UNIDAD_UTILIZACION=("TIPO_UNIDADE_PREST_HOSPITALAR", moda_segura),
        ESPECIALIDAD_UTILIZACION=("DESC_ESPECIALIDADE", moda_segura),
    )

    utilizaciones = (
        resumen_costo.join(resumen_clinico, how="left")
        .join(target_utilizacion, how="left")
        .reset_index()
    )

    return utilizaciones


def enriquecer_con_edad_sexo(
    utilizaciones: pd.DataFrame, beneficiarios: pd.DataFrame
) -> pd.DataFrame:
    """Agrega edad (en la fecha de la utilización) y sexo del beneficiario."""
    datos_personales = beneficiarios[
        ["CHAVE_FUNCIONAL", "DT_NASCIMENTO_BENEFICIARIO", "SEXO_MODELO"]
    ]
    utilizaciones = utilizaciones.merge(
        datos_personales, on="CHAVE_FUNCIONAL", how="left", validate="many_to_one"
    )

    edad = (
        utilizaciones["DT_UTILIZACAO"] - utilizaciones["DT_NASCIMENTO_BENEFICIARIO"]
    ).dt.days / 365.25
    edad = np.floor(edad)
    utilizaciones["EDAD_UTILIZACION"] = edad.where((edad >= 0) & (edad <= 110), np.nan)

    return utilizaciones


def resumen_descriptivo_costo(
    utilizaciones: pd.DataFrame, columna_target: str
) -> pd.DataFrame:
    
    resumen = utilizaciones.groupby(columna_target)["COSTO_UTILIZACION"].agg(
        N="count",
        MEDIA="mean",
        MEDIANA="median",
        DESV_ESTANDAR="std",
        P25=lambda x: x.quantile(0.25),
        P75=lambda x: x.quantile(0.75),
        P95=lambda x: x.quantile(0.95),
        MAXIMO="max",
    )
    resumen["COEF_VARIACION"] = resumen["DESV_ESTANDAR"] / resumen["MEDIA"]
    return resumen.round(2)


COSTO_VARIABLES_NUMERICAS = [
    "EDAD_UTILIZACION",
    "N_PROCEDIMIENTOS_UTILIZACION",
    "N_ESPECIALIDADES_UTILIZACION",
]
COSTO_VARIABLES_CATEGORICAS = [
    "SEXO_MODELO",
    "INTERNADO_UTILIZACION",
    "UTI_UTILIZACION",
    "ESTADO_UTILIZACION",
    "UNIDAD_UTILIZACION",
    "ESPECIALIDAD_UTILIZACION",
]


def entrenar_modelo_costo(
    utilizaciones: pd.DataFrame,
    columna_target_enfermedad: str,
    seed: int = 42,
):
    
    datos_enfermedad = utilizaciones.loc[
        utilizaciones[columna_target_enfermedad] == 1
    ].copy()

    datos_enfermedad["LOG_COSTO"] = np.log1p(datos_enfermedad["COSTO_UTILIZACION"])

    variables = COSTO_VARIABLES_NUMERICAS + COSTO_VARIABLES_CATEGORICAS
    X = datos_enfermedad[variables]
    y = datos_enfermedad["LOG_COSTO"]
    y_costo_real = datos_enfermedad["COSTO_UTILIZACION"]

    X_train, X_test, y_train, y_test, y_costo_train, y_costo_test = train_test_split(
        X, y, y_costo_real, test_size=0.25, random_state=seed
    )

    preprocesador = ColumnTransformer(
        transformers=[
            (
                "num",
                Pipeline([("imputador", SimpleImputer(strategy="median"))]),
                COSTO_VARIABLES_NUMERICAS,
            ),
            (
                "cat",
                Pipeline(
                    [
                        ("imputador", SimpleImputer(strategy="most_frequent")),
                        ("onehot", OneHotEncoder(handle_unknown="ignore")),
                    ]
                ),
                COSTO_VARIABLES_CATEGORICAS,
            ),
        ]
    )

    modelos = {
        "REGRESION_LINEAL": LinearRegression(),
        "GRADIENT_BOOSTING": GradientBoostingRegressor(
            n_estimators=200, max_depth=3, learning_rate=0.05, random_state=seed
        ),
    }

    resultados = {}
    for nombre, estimador in modelos.items():
        pipeline = Pipeline(
            steps=[("preprocesamiento", preprocesador), ("regresor", estimador)]
        )
        pipeline.fit(X_train, y_train)

        pred_log = pipeline.predict(X_test)
        pred_costo = np.expm1(pred_log)  # se revierte la transformación log1p

        metricas = {
            "MAE_LOG": mean_absolute_error(y_test, pred_log),
            "RMSE_LOG": np.sqrt(mean_squared_error(y_test, pred_log)),
            "R2_LOG": r2_score(y_test, pred_log),
            "MAE_COSTO_REAL": mean_absolute_error(y_costo_test, pred_costo),
            "RMSE_COSTO_REAL": np.sqrt(mean_squared_error(y_costo_test, pred_costo)),
        }

        resultados[nombre] = {
            "pipeline": pipeline,
            "metricas": metricas,
            "predicciones_test": pred_costo,
            "y_test_real": y_costo_test,
        }

    return {
        "resultados": resultados,
        "n_observaciones": len(datos_enfermedad),
        "X_train": X_train,
        "X_test": X_test,
    }
