from __future__ import annotations

import pandas as pd
from sklearn.base import ClassifierMixin
from sklearn.pipeline import Pipeline

from src.evaluation import encontrar_umbral_f1, evaluar_modelo
from src.modeling_prep import crear_preprocesador


def entrenar_y_evaluar(
    nombre_modelo: str,
    nombre_configuracion: str,
    estimador: ClassifierMixin,
    beneficiarios: pd.DataFrame,
    columna_target: str,
    variables_numericas: list[str],
    variables_categoricas: list[str],
    indices: dict,
    escalar: bool,
) -> dict:
    
    variables = variables_numericas + variables_categoricas
    X = beneficiarios[variables]
    y = beneficiarios[columna_target].astype("int8")

    X_train, X_val, X_test = (
        X.iloc[indices["train"]],
        X.iloc[indices["val"]],
        X.iloc[indices["test"]],
    )
    y_train, y_val, y_test = (
        y.iloc[indices["train"]],
        y.iloc[indices["val"]],
        y.iloc[indices["test"]],
    )

    preprocesador = crear_preprocesador(variables_numericas, variables_categoricas, escalar=escalar)
    modelo = Pipeline(steps=[("preprocesamiento", preprocesador), ("clasificador", estimador)])

    print(f"Entrenando {nombre_modelo} - {nombre_configuracion}")
    modelo.fit(X_train, y_train)

    prob_val = modelo.predict_proba(X_val)[:, 1]
    mejor_umbral = encontrar_umbral_f1(y_val, prob_val)

    prob_test = modelo.predict_proba(X_test)[:, 1]
    resultados_test = evaluar_modelo(y_test, prob_test, umbral=mejor_umbral["UMBRAL"])
    resultados_test["MODELO"] = nombre_modelo
    resultados_test["ESCENARIO"] = nombre_configuracion

    return {
        "modelo": modelo,
        "umbral": mejor_umbral,
        "resultados": resultados_test,
        "prob_val": prob_val,
        "prob_test": prob_test,
        "y_val": y_val,
        "y_test": y_test,
    }


def entrenar_todas_configuraciones(
    nombre_modelo: str,
    crear_estimador_fn,
    beneficiarios: pd.DataFrame,
    columna_target: str,
    configuraciones: dict,
    indices: dict,
    escalar: bool,
) -> dict:
    
    resultados = {}
    for nombre_config, config in configuraciones.items():
        resultado = entrenar_y_evaluar(
            nombre_modelo=nombre_modelo,
            nombre_configuracion=nombre_config,
            estimador=crear_estimador_fn(),
            beneficiarios=beneficiarios,
            columna_target=columna_target,
            variables_numericas=config["numericas"],
            variables_categoricas=config["categoricas"],
            indices=indices,
            escalar=escalar,
        )
        resultados[nombre_config] = resultado
    return resultados


def tabla_resumen(resultados: dict) -> pd.DataFrame:
    """Convierte un diccionario de resultados por configuración en una tabla comparativa."""
    columnas_metricas = [
        "MODELO",
        "ESCENARIO",
        "UMBRAL",
        "PR_AUC",
        "ROC_AUC",
        "F1",
        "PRECISION",
        "RECALL",
        "ESPECIFICIDAD",
        "ACCURACY",
        "TP",
        "FP",
        "FN",
        "TN",
    ]
    tabla = pd.DataFrame([r["resultados"] for r in resultados.values()])
    return tabla[columnas_metricas].sort_values("PR_AUC", ascending=False)
