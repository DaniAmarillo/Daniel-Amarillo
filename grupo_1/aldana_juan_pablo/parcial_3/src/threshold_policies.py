from __future__ import annotations

import numpy as np
import pandas as pd

from src.evaluation import evaluar_modelo


def calcular_tabla_umbrales(y_val: np.ndarray, prob_val: np.ndarray) -> pd.DataFrame:
    """Genera la curva completa precision/recall/F1 en función del umbral, sobre validación."""
    from sklearn.metrics import precision_recall_curve

    precision, recall, thresholds = precision_recall_curve(y_val, prob_val)
    tabla = pd.DataFrame(
        {"UMBRAL": thresholds, "PRECISION": precision[:-1], "RECALL": recall[:-1]}
    )
    tabla["F1"] = (
        2 * tabla["PRECISION"] * tabla["RECALL"] / (tabla["PRECISION"] + tabla["RECALL"] + 1e-12)
    )
    return tabla


def seleccionar_umbrales_por_politica(tabla_umbrales: pd.DataFrame) -> dict[str, float]:
    
    politicas = {}

    fila_f1 = tabla_umbrales.sort_values("F1", ascending=False).iloc[0]
    politicas["MAX_F1"] = fila_f1["UMBRAL"]

    for nombre, recall_minimo in [("RECALL_50", 0.50), ("RECALL_80", 0.80)]:
        candidatos = tabla_umbrales.loc[tabla_umbrales["RECALL"] >= recall_minimo]
        if len(candidatos) > 0:
            mejor = candidatos.sort_values("PRECISION", ascending=False).iloc[0]
            politicas[nombre] = mejor["UMBRAL"]
        else:
            # Ningún umbral alcanza ese recall en validación: se usa el
            # umbral mínimo observado, que maximiza el recall posible.
            politicas[nombre] = tabla_umbrales["UMBRAL"].min()

    return politicas


def comparar_politicas_umbral(
    y_val: np.ndarray, prob_val: np.ndarray, y_test: np.ndarray, prob_test: np.ndarray
) -> pd.DataFrame:
   
    tabla_umbrales = calcular_tabla_umbrales(y_val, prob_val)
    politicas = seleccionar_umbrales_por_politica(tabla_umbrales)

    filas = []
    for nombre_politica, umbral in politicas.items():
        resultado = evaluar_modelo(y_test, prob_test, umbral=umbral)
        resultado["POLITICA"] = nombre_politica
        filas.append(resultado)

    columnas = [
        "POLITICA",
        "UMBRAL",
        "PR_AUC",
        "ROC_AUC",
        "F1",
        "PRECISION",
        "RECALL",
        "ESPECIFICIDAD",
        "TP",
        "FP",
        "FN",
        "TN",
    ]
    return pd.DataFrame(filas)[columnas]
