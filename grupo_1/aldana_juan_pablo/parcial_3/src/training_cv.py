from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.base import clone
from sklearn.pipeline import Pipeline

from src.evaluation import encontrar_umbral_f1, evaluar_modelo
from src.modeling_prep import crear_preprocesador
from src.threshold_policies import calcular_tabla_umbrales, seleccionar_umbrales_por_politica


def entrenar_con_kfold(
    nombre_modelo: str,
    nombre_configuracion: str,
    crear_estimador_fn,
    beneficiarios: pd.DataFrame,
    columna_target: str,
    variables_numericas: list[str],
    variables_categoricas: list[str],
    particion_kfold: dict,
    escalar: bool,
    politica_umbral: str = "RECALL_80",
) -> dict:
    
    variables = variables_numericas + variables_categoricas
    X_desarrollo = beneficiarios.iloc[particion_kfold["idx_desarrollo"]][variables].reset_index(
        drop=True
    )
    y_desarrollo = (
        beneficiarios.iloc[particion_kfold["idx_desarrollo"]][columna_target]
        .astype("int8")
        .reset_index(drop=True)
    )

    metricas_por_fold = []
    umbrales_por_fold = []

    for fold_num, (idx_train_fold, idx_val_fold) in enumerate(particion_kfold["folds"], start=1):
        X_train_fold = X_desarrollo.iloc[idx_train_fold]
        y_train_fold = y_desarrollo.iloc[idx_train_fold]
        X_val_fold = X_desarrollo.iloc[idx_val_fold]
        y_val_fold = y_desarrollo.iloc[idx_val_fold]

        preprocesador = crear_preprocesador(variables_numericas, variables_categoricas, escalar=escalar)
        modelo_fold = Pipeline(
            steps=[("preprocesamiento", preprocesador), ("clasificador", crear_estimador_fn())]
        )
        modelo_fold.fit(X_train_fold, y_train_fold)

        prob_val_fold = modelo_fold.predict_proba(X_val_fold)[:, 1]

        tabla_umbrales = calcular_tabla_umbrales(y_val_fold, prob_val_fold)
        umbrales_politica = seleccionar_umbrales_por_politica(tabla_umbrales)
        umbral_fold = umbrales_politica[politica_umbral]
        umbrales_por_fold.append(umbral_fold)

        resultado_fold = evaluar_modelo(y_val_fold, prob_val_fold, umbral=umbral_fold)
        resultado_fold["FOLD"] = fold_num
        metricas_por_fold.append(resultado_fold)

        print(
            f"  Fold {fold_num}: PR_AUC={resultado_fold['PR_AUC']:.4f} "
            f"Recall={resultado_fold['RECALL']:.3f} Precision={resultado_fold['PRECISION']:.3f}"
        )

    tabla_folds = pd.DataFrame(metricas_por_fold)

    # --- Entrenamiento final sobre todo el conjunto de desarrollo ---
    preprocesador_final = crear_preprocesador(variables_numericas, variables_categoricas, escalar=escalar)
    modelo_final = Pipeline(
        steps=[("preprocesamiento", preprocesador_final), ("clasificador", crear_estimador_fn())]
    )
    modelo_final.fit(X_desarrollo, y_desarrollo)

    umbral_final = float(np.mean(umbrales_por_fold))

    X_test = beneficiarios.iloc[particion_kfold["idx_test"]][variables]
    y_test = beneficiarios.iloc[particion_kfold["idx_test"]][columna_target].astype("int8")
    prob_test = modelo_final.predict_proba(X_test)[:, 1]

    resultado_test = evaluar_modelo(y_test, prob_test, umbral=umbral_final)
    resultado_test["MODELO"] = nombre_modelo
    resultado_test["ESCENARIO"] = nombre_configuracion
    resultado_test["POLITICA_UMBRAL"] = politica_umbral

    return {
        "modelo_final": modelo_final,
        "tabla_folds": tabla_folds,
        "resumen_folds": tabla_folds[
            ["PR_AUC", "ROC_AUC", "F1", "PRECISION", "RECALL", "ESPECIFICIDAD"]
        ].agg(["mean", "std"]),
        "umbral_final": umbral_final,
        "resultado_test": resultado_test,
        "prob_test": prob_test,
        "y_test": y_test,
    }


def tabla_resumen_cv(resultados: dict) -> pd.DataFrame:
    """Convierte un diccionario {config: resultado_kfold} en una tabla comparativa final (test held-out)."""
    columnas = [
        "MODELO",
        "ESCENARIO",
        "POLITICA_UMBRAL",
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
    tabla = pd.DataFrame([r["resultado_test"] for r in resultados.values()])
    return tabla[columnas].sort_values("PR_AUC", ascending=False)
