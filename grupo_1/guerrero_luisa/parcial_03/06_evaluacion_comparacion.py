"""Comparación en el mismo test; PR-AUC y sensibilidad son métricas principales."""
import joblib
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import (PrecisionRecallDisplay, RocCurveDisplay, accuracy_score,
                             average_precision_score, confusion_matrix, f1_score,
                             precision_score, recall_score, roc_auc_score)

from common import FIGURES, MODELS, TABLES, ensure_directories, get_score, save_csv

REQUIRED_NAMES = ["modelo_regresion_logistica", "modelo_arbol_decision",
                  "modelo_random_forest", "modelo_gradient_boosting"]


def available_models() -> list[str]:
    registry = TABLES / "modelos_entrenados.csv"
    if registry.exists():
        names = pd.read_csv(registry)["modelo"].astype(str).tolist()
        if all(name in names for name in REQUIRED_NAMES):
            return names
    return REQUIRED_NAMES.copy()


def main() -> None:
    ensure_directories()
    names = available_models()
    required_files = [TABLES / "base_beneficiario_dorsalgia.csv", TABLES / "test_ids.csv",
                      TABLES / "variables_seleccionadas.csv"] + [MODELS / f"{name}.joblib" for name in REQUIRED_NAMES]
    missing = [str(path) for path in required_files if not path.exists()]
    if missing: raise FileNotFoundError("Faltan insumos; ejecute primero 04 y 05:\n" + "\n".join(missing))
    data = pd.read_csv(TABLES / "base_beneficiario_dorsalgia.csv", low_memory=False)
    test_ids = set(pd.read_csv(TABLES / "test_ids.csv")["CHAVE_FUNCIONAL"].astype(str))
    test = data[data["CHAVE_FUNCIONAL"].astype(str).isin(test_ids)].copy()
    selected = pd.read_csv(TABLES / "variables_seleccionadas.csv")["variable"].tolist()
    missing_columns = sorted(set(selected) - set(test.columns))
    if missing_columns: raise ValueError(f"Faltan variables seleccionadas: {missing_columns}")
    X = test[selected]; y = test["dorsalgia"].astype(int)
    if test.empty or y.nunique() < 2:
        raise ValueError("El test persistido está vacío o no contiene ambas clases; vuelva a ejecutar 05.")
    metrics, matrices, predictions = [], [], {}
    fig_roc, ax_roc = plt.subplots(figsize=(7,6)); fig_pr, ax_pr = plt.subplots(figsize=(7,6))
    for name in names:
        model = joblib.load(MODELS / f"{name}.joblib"); score = get_score(model, X); pred = model.predict(X)
        predictions[name] = (pred, score); tn, fp, fn, tp = confusion_matrix(y, pred, labels=[0,1]).ravel()
        # Accuracy puede ser engañosa ante una clase positiva minoritaria.
        metrics.append({"modelo": name, "accuracy": accuracy_score(y,pred), "precision": precision_score(y,pred,zero_division=0),
            "recall_sensibilidad": recall_score(y,pred,zero_division=0), "especificidad": tn/max(tn+fp,1),
            "f1": f1_score(y,pred,zero_division=0), "roc_auc": roc_auc_score(y,score), "pr_auc": average_precision_score(y,score)})
        matrices.extend([{"modelo": name, "real": r, "predicho": p, "n": n} for r,p,n in [(0,0,tn),(0,1,fp),(1,0,fn),(1,1,tp)]])
        RocCurveDisplay.from_predictions(y, score, name=name, ax=ax_roc); PrecisionRecallDisplay.from_predictions(y, score, name=name, ax=ax_pr)
    table = pd.DataFrame(metrics)
    # Diferencias menores a 0.0001 en PR-AUC se consideran desempeño similar;
    # en ese caso se priorizan sensibilidad y F1, tal como exige el taller.
    ranking = table.assign(pr_auc_criterio=table["pr_auc"].round(4)).sort_values(
        ["pr_auc_criterio", "recall_sensibilidad", "f1", "pr_auc"], ascending=False)
    table = ranking.drop(columns="pr_auc_criterio")
    save_csv(table, "metricas_modelos.csv"); save_csv(pd.DataFrame(matrices), "matrices_confusion.csv")
    best = table.iloc[[0]].copy(); best["criterio_seleccion"] = "Mayor PR-AUC; sensibilidad y F1 como desempates. Accuracy no basta ante desbalance extremo."
    save_csv(best, "mejor_modelo.csv")
    fig_roc.tight_layout(); fig_roc.savefig(FIGURES/"curva_roc_modelos.png", dpi=150); plt.close(fig_roc)
    fig_pr.tight_layout(); fig_pr.savefig(FIGURES/"curva_precision_recall_modelos.png", dpi=150); plt.close(fig_pr)
    best_name = best.iloc[0]["modelo"]; pred = predictions[best_name][0]; cm = confusion_matrix(y,pred,labels=[0,1])
    fig, ax = plt.subplots(); image=ax.imshow(cm); fig.colorbar(image); ax.set(xticks=[0,1],yticks=[0,1],xlabel="Predicho",ylabel="Real",title=best_name)
    for i in range(2):
        for j in range(2): ax.text(j,i,cm[i,j],ha="center",va="center")
    fig.tight_layout(); fig.savefig(FIGURES/"matriz_confusion_mejor_modelo.png", dpi=150); plt.close(fig)
    print(table.to_string(index=False))


if __name__ == "__main__": main()
