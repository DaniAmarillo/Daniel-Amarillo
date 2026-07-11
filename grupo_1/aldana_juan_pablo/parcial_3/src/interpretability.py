from __future__ import annotations

import numpy as np
import pandas as pd
import shap
from sklearn.inspection import permutation_importance


def importancia_nativa(modelo_pipeline, nombres_variables_originales: list[str]) -> pd.DataFrame:

    preprocesador = modelo_pipeline.named_steps["preprocesamiento"]
    clasificador = modelo_pipeline.named_steps["clasificador"]

    nombres_transformados = preprocesador.get_feature_names_out()
    importancias = clasificador.feature_importances_

    def variable_original(nombre_transformado: str) -> str:
        nombre = nombre_transformado.replace("num__", "").replace("cat__", "")
        candidatos = sorted(nombres_variables_originales, key=len, reverse=True)
        for variable in candidatos:
            if nombre.startswith(variable):
                return variable
        return nombre

    tabla = pd.DataFrame({"VARIABLE": nombres_transformados, "IMPORTANCIA": importancias})
    tabla["VARIABLE_ORIGINAL"] = tabla["VARIABLE"].apply(variable_original)

    agregada = (
        tabla.groupby("VARIABLE_ORIGINAL")["IMPORTANCIA"]
        .sum()
        .sort_values(ascending=False)
        .reset_index()
    )
    return agregada


def calcular_permutation_importance(
    modelo_pipeline,
    X_test: pd.DataFrame,
    y_test: pd.Series,
    n_repeats: int = 5,
    max_negativos_muestra: int = 10_000,
    seed: int = 42,
) -> pd.DataFrame:
    rng = np.random.default_rng(seed)
    idx_pos = np.where(y_test.values == 1)[0]
    idx_neg = np.where(y_test.values == 0)[0]
    idx_neg_muestra = rng.choice(
        idx_neg, size=min(max_negativos_muestra, len(idx_neg)), replace=False
    )
    idx_muestra = np.concatenate([idx_pos, idx_neg_muestra])

    X_muestra = X_test.iloc[idx_muestra]
    y_muestra = y_test.iloc[idx_muestra]

    resultado = permutation_importance(
    modelo_pipeline,
    X_muestra,
    y_muestra,
    scoring="average_precision",
    n_repeats=n_repeats,
    random_state=seed,
    n_jobs=1,
)

    tabla = pd.DataFrame(
        {
            "VARIABLE": X_test.columns,
            "IMPORTANCIA_MEDIA": resultado.importances_mean,
            "DESVIACION": resultado.importances_std,
        }
    ).sort_values("IMPORTANCIA_MEDIA", ascending=False).reset_index(drop=True)

    return tabla


def calcular_shap_values(
    modelo_pipeline, X_muestra: pd.DataFrame, max_muestras: int = 2000, seed: int = 42
):
    
    preprocesador = modelo_pipeline.named_steps["preprocesamiento"]
    clasificador = modelo_pipeline.named_steps["clasificador"]

    if len(X_muestra) > max_muestras:
        X_muestra = X_muestra.sample(max_muestras, random_state=seed)

    X_trans = preprocesador.transform(X_muestra)
    if hasattr(X_trans, "toarray"):
        X_trans = X_trans.toarray()

    nombres_variables = preprocesador.get_feature_names_out()
    X_trans_df = pd.DataFrame(X_trans, columns=nombres_variables, index=X_muestra.index)

    explicador = shap.TreeExplainer(clasificador)
    shap_values = explicador(X_trans_df)

    return shap_values, X_trans_df, nombres_variables
