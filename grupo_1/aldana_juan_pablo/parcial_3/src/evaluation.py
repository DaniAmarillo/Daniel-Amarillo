from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    confusion_matrix,
    f1_score,
    precision_recall_curve,
    precision_score,
    recall_score,
    roc_auc_score,
)

SEED = 42


def evaluar_modelo(y_real: np.ndarray, probabilidades: np.ndarray, umbral: float = 0.50) -> dict:
    
    predicciones = (probabilidades >= umbral).astype(int)

    tn, fp, fn, tp = confusion_matrix(y_real, predicciones, labels=[0, 1]).ravel()
    especificidad = tn / (tn + fp) if (tn + fp) > 0 else np.nan

    return {
        "UMBRAL": umbral,
        "ACCURACY": accuracy_score(y_real, predicciones),
        "PRECISION": precision_score(y_real, predicciones, zero_division=0),
        "RECALL": recall_score(y_real, predicciones, zero_division=0),
        "ESPECIFICIDAD": especificidad,
        "F1": f1_score(y_real, predicciones, zero_division=0),
        "ROC_AUC": roc_auc_score(y_real, probabilidades),
        "PR_AUC": average_precision_score(y_real, probabilidades),
        "TN": tn,
        "FP": fp,
        "FN": fn,
        "TP": tp,
    }


def encontrar_umbral_f1(y_real: np.ndarray, probabilidades: np.ndarray) -> dict:
    """Encuentra el umbral que maximiza F1 sobre la curva precision-recall."""
    precision, recall, thresholds = precision_recall_curve(y_real, probabilidades)
    precision, recall = precision[:-1], recall[:-1]
    f1 = 2 * precision * recall / (precision + recall + 1e-12)
    mejor_indice = np.argmax(f1)
    return {
        "UMBRAL": thresholds[mejor_indice],
        "PRECISION": precision[mejor_indice],
        "RECALL": recall[mejor_indice],
        "F1": f1[mejor_indice],
    }


def intervalo_confianza_bootstrap(
    y_real: np.ndarray,
    probabilidades: np.ndarray,
    umbral: float,
    n_iteraciones: int = 1000,
    nivel_confianza: float = 0.95,
    seed: int = SEED,
) -> pd.DataFrame:
    
    rng = np.random.default_rng(seed)
    n = len(y_real)
    y_real = np.asarray(y_real)
    probabilidades = np.asarray(probabilidades)

    metricas_boot = {"RECALL": [], "PRECISION": [], "F1": [], "PR_AUC": []}

    for _ in range(n_iteraciones):
        idx_muestra = rng.integers(0, n, size=n)
        y_muestra = y_real[idx_muestra]
        prob_muestra = probabilidades[idx_muestra]

        
        if y_muestra.sum() == 0 or y_muestra.sum() == len(y_muestra):
            continue

        pred_muestra = (prob_muestra >= umbral).astype(int)
        metricas_boot["RECALL"].append(recall_score(y_muestra, pred_muestra, zero_division=0))
        metricas_boot["PRECISION"].append(
            precision_score(y_muestra, pred_muestra, zero_division=0)
        )
        metricas_boot["F1"].append(f1_score(y_muestra, pred_muestra, zero_division=0))
        metricas_boot["PR_AUC"].append(average_precision_score(y_muestra, prob_muestra))

    alpha = 1 - nivel_confianza
    filas = []
    for nombre, valores in metricas_boot.items():
        valores = np.array(valores)
        filas.append(
            {
                "METRICA": nombre,
                "MEDIA_BOOTSTRAP": valores.mean(),
                f"IC_{int(nivel_confianza*100)}_INFERIOR": np.quantile(valores, alpha / 2),
                f"IC_{int(nivel_confianza*100)}_SUPERIOR": np.quantile(valores, 1 - alpha / 2),
                "N_ITERACIONES_VALIDAS": len(valores),
            }
        )
    return pd.DataFrame(filas)
