"""Importancias, coeficientes y análisis de errores del conjunto de prueba."""
import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.inspection import permutation_importance

from common import FIGURES, MODELS, TABLES, ensure_directories, get_score, save_csv


def feature_names(pipeline) -> np.ndarray:
    return pipeline.named_steps["preprocesamiento"].get_feature_names_out()


def main() -> None:
    ensure_directories()
    required = [TABLES/"base_beneficiario_dorsalgia.csv", TABLES/"test_ids.csv",
                TABLES/"variables_seleccionadas.csv",
                TABLES/"metricas_modelos.csv", TABLES/"mejor_modelo.csv"]
    missing = [str(path) for path in required if not path.exists()]
    if missing: raise FileNotFoundError("Faltan insumos; ejecute primero 04, 05 y 06:\n" + "\n".join(missing))
    data = pd.read_csv(TABLES/"base_beneficiario_dorsalgia.csv", low_memory=False); ids = set(pd.read_csv(TABLES/"test_ids.csv")["CHAVE_FUNCIONAL"].astype(str))
    test = data[data["CHAVE_FUNCIONAL"].astype(str).isin(ids)].copy()
    selected = pd.read_csv(TABLES/"variables_seleccionadas.csv")["variable"].tolist()
    X=test[selected]; y=test["dorsalgia"].astype(int)
    metrics=pd.read_csv(TABLES/"metricas_modelos.csv"); best_name=pd.read_csv(TABLES/"mejor_modelo.csv").iloc[0]["modelo"]
    tree_names = [name for name in ["modelo_arbol_decision", "modelo_random_forest",
                                    "modelo_gradient_boosting", "modelo_xgboost"]
                  if (MODELS/f"{name}.joblib").exists() and name in set(metrics["modelo"])]
    importance_parts = []
    for name in tree_names:
        pipeline = joblib.load(MODELS/f"{name}.joblib")
        estimator = pipeline.named_steps["modelo"]
        if hasattr(estimator, "feature_importances_"):
            importance_parts.append(pd.DataFrame({"modelo": name,
                "variable": feature_names(pipeline),
                "importancia": estimator.feature_importances_, "metodo": "importancia_interna"}))
    # Respaldo general: si el mejor modelo no expone importancia, se calcula
    # permutación sobre una muestra acotada del test y con PR-AUC.
    if best_name not in tree_names and best_name != "modelo_regresion_logistica":
        best_for_importance = joblib.load(MODELS/f"{best_name}.joblib")
        sample = X.sample(min(5000, len(X)), random_state=42)
        result = permutation_importance(best_for_importance, sample, y.loc[sample.index],
                                        scoring="average_precision", n_repeats=5,
                                        random_state=42, n_jobs=-1)
        importance_parts.append(pd.DataFrame({"modelo": best_name, "variable": sample.columns,
            "importancia": result.importances_mean, "metodo": "permutacion_PR_AUC"}))
    importance = pd.concat(importance_parts, ignore_index=True).sort_values(
        ["modelo", "importancia"], ascending=[True, False])
    save_csv(importance,"importancia_variables.csv")
    plot_model = metrics[metrics["modelo"].isin(tree_names)].sort_values("pr_auc", ascending=False).iloc[0]["modelo"]
    top=importance[importance["modelo"].eq(plot_model)].nlargest(20,"importancia").sort_values("importancia")
    plt.figure(figsize=(9,7)); plt.barh(top["variable"],top["importancia"])
    plt.title(f"Importancia de variables: {plot_model}"); plt.tight_layout(); plt.savefig(FIGURES/"importancia_variables.png",dpi=150); plt.close()
    logistic=joblib.load(MODELS/"modelo_regresion_logistica.joblib"); coefficients=pd.DataFrame({"variable":feature_names(logistic),"coeficiente":logistic.named_steps["modelo"].coef_[0]})
    coefficients["odds_ratio"]=np.exp(coefficients["coeficiente"].clip(-30,30)); coefficients=coefficients.sort_values("coeficiente")
    save_csv(coefficients,"coeficientes_regresion_logistica.csv")
    extremes=pd.concat([coefficients.head(10),coefficients.tail(10)]).sort_values("coeficiente")
    plt.figure(figsize=(9,7)); plt.barh(extremes["variable"],extremes["coeficiente"]); plt.axvline(0,color="black",linewidth=.8)
    plt.tight_layout(); plt.savefig(FIGURES/"top_coeficientes_logisticos.png",dpi=150); plt.close()
    best=joblib.load(MODELS/f"{best_name}.joblib"); pred=best.predict(X); score=get_score(best,X)
    labels=np.select([(y.eq(1)&(pred==1)),(y.eq(0)&(pred==0)),(y.eq(0)&(pred==1))],["verdadero_positivo","verdadero_negativo","falso_positivo"],default="falso_negativo")
    errors=test[["CHAVE_FUNCIONAL","dorsalgia"]].copy(); errors["prediccion"]=pred; errors["probabilidad"]=score; errors["grupo_error"]=labels
    detail_cols=[c for c in ["edad","sexo","tipo_beneficiario","tuvo_internacion","tuvo_uti","costo_total","numero_utilizaciones"] if c in test]
    errors=errors.join(test[detail_cols]); save_csv(errors,"analisis_errores.csv")
    numeric=[c for c in ["edad","tuvo_internacion","tuvo_uti","costo_total","numero_utilizaciones"] if c in errors]
    summary=errors.groupby("grupo_error").agg(n=("grupo_error","size"),**{f"promedio_{c}":(c,"mean") for c in numeric}).reset_index()
    # Composición de sexo y tipo completa el resumen de sesgo por grupo.
    for col in [c for c in ["sexo","tipo_beneficiario"] if c in errors]:
        modes=errors.groupby("grupo_error")[col].agg(lambda s: s.mode().iloc[0] if not s.mode().empty else pd.NA)
        summary[f"{col}_mas_frecuente"]=summary["grupo_error"].map(modes)
    save_csv(summary,"resumen_errores_por_grupo.csv")
    print("Interpretación descriptiva: las importancias son asociaciones predictivas, no efectos causales. Revise proxies de intensidad de uso y costos, posibles sesgos de acceso/subregistro y falsos negativos antes de uso clínico.")


if __name__ == "__main__": main()
