import os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from sklearn.model_selection import (train_test_split, RepeatedStratifiedKFold,
                                     StratifiedKFold, cross_validate, cross_val_predict)
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, HistGradientBoostingClassifier
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.impute import SimpleImputer
from sklearn.metrics import (roc_auc_score, average_precision_score, roc_curve,
                             precision_recall_curve, confusion_matrix, f1_score,
                             precision_score, recall_score, balanced_accuracy_score)
import joblib


OUT_DIR      = "outputs"
TARGET_LABEL = "Hipertensión (I10-I15)"
TEST_SIZE    = 0.25
RANDOM_STATE = 42
CV_REPEATS   = 2      
RF_TREES     = 300
HGB_ITERS    = 300
NEG, POS     = "#9aa0a6", "#7b2d8b"
DROP = {"id", "target", "n_sexos_distintos", "n_dob_distintos", "edad_invalida"}
CAT_COLS = ["sexo", "tipo_benef"]


FIG_DIR = os.path.join(OUT_DIR, "figures")
TBL_DIR = os.path.join(OUT_DIR, "tablas")
MOD_DIR = os.path.join(OUT_DIR, "modelos")
for d in (FIG_DIR, TBL_DIR, MOD_DIR):
    os.makedirs(d, exist_ok=True)


def build_preprocessor(num_cols, cat_cols):
    num = Pipeline([("imp", SimpleImputer(strategy="median")),
                    ("sc", StandardScaler())])
    cat = Pipeline([("imp", SimpleImputer(strategy="most_frequent")),
                    ("oh", OneHotEncoder(handle_unknown="ignore"))])
    return ColumnTransformer([("num", num, num_cols), ("cat", cat, cat_cols)])


def youden_threshold(y_true, proba):
    fpr, tpr, thr = roc_curve(y_true, proba)
    j = tpr - fpr
    i = int(np.argmax(j))
    return float(thr[i])


def metrics_at_threshold(y_true, proba, thr):
    yhat = (proba >= thr).astype(int)
    tn, fp, fn, tp = confusion_matrix(y_true, yhat, labels=[0, 1]).ravel()
    spec = tn / (tn + fp) if (tn + fp) else np.nan
    return {
        "precision": precision_score(y_true, yhat, zero_division=0),
        "recall_sensibilidad": recall_score(y_true, yhat, zero_division=0),
        "especificidad": spec,
        "f1": f1_score(y_true, yhat, zero_division=0),
        "balanced_acc": balanced_accuracy_score(y_true, yhat),
        "TN": tn, "FP": fp, "FN": fn, "TP": tp,
    }


def main():
    df = pd.read_parquet(os.path.join(OUT_DIR, "beneficiarios.parquet"))
    y = df["target"].astype(int).values
    feat_cols = [c for c in df.columns if c not in DROP]
    cat_cols = [c for c in CAT_COLS if c in feat_cols]
    num_cols = [c for c in feat_cols if c not in cat_cols]
    X = df[feat_cols]

    prev = y.mean()
    print(f">>> n={len(y):,} | positivos={y.sum():,} | prevalencia={prev*100:.3f}%")
    print(f">>> features: {len(num_cols)} numericas + {len(cat_cols)} categoricas\n")

    Xtr, Xte, ytr, yte = train_test_split(
        X, y, test_size=TEST_SIZE, stratify=y, random_state=RANDOM_STATE)

    pre = build_preprocessor(num_cols, cat_cols)
    models = {
        "LogReg": Pipeline([("pre", pre),
            ("clf", LogisticRegression(class_weight="balanced", max_iter=3000))]),
        "RandomForest": Pipeline([("pre", pre),
            ("clf", RandomForestClassifier(n_estimators=RF_TREES, min_samples_leaf=5,
                    max_samples=0.5, class_weight="balanced_subsample", n_jobs=-1,
                    random_state=RANDOM_STATE))]),
        "HistGB": Pipeline([("pre", pre),
            ("clf", HistGradientBoostingClassifier(class_weight="balanced",
                    learning_rate=0.05, max_iter=HGB_ITERS, l2_regularization=1.0,
                    random_state=RANDOM_STATE))]),
    }

    cv_score = RepeatedStratifiedKFold(n_splits=5, n_repeats=CV_REPEATS, random_state=RANDOM_STATE)
    cv_thr   = StratifiedKFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)

    rows, proba_test, thresholds = [], {}, {}
    for name, pipe in models.items():
        print(f">>> {name}: CV repetida (roc_auc, average_precision)...")
        cvres = cross_validate(pipe, Xtr, ytr, cv=cv_score,
                               scoring=["roc_auc", "average_precision"], n_jobs=-1)
        auc = cvres["test_roc_auc"]
        ap  = cvres["test_average_precision"]

        oof = cross_val_predict(pipe, Xtr, ytr, cv=cv_thr,
                                method="predict_proba", n_jobs=-1)[:, 1]
        thr = youden_threshold(ytr, oof)
        thresholds[name] = thr

        pipe.fit(Xtr, ytr)
        p_te = pipe.predict_proba(Xte)[:, 1]
        proba_test[name] = p_te
        m = metrics_at_threshold(yte, p_te, thr)

        rows.append({
            "modelo": name,
            "cv_roc_auc": auc.mean(), "cv_roc_auc_sd": auc.std(),
            "cv_pr_auc": ap.mean(), "cv_pr_auc_sd": ap.std(),
            "test_roc_auc": roc_auc_score(yte, p_te),
            "test_pr_auc": average_precision_score(yte, p_te),
            "umbral": thr, **m,
        })

    res = pd.DataFrame(rows).set_index("modelo")
    res.to_csv(os.path.join(TBL_DIR, "metricas_modelos.csv"))

    pd.set_option("display.width", 200, "display.max_columns", None)
    print("\n" + "=" * 70)
    print(f"RESULTADOS — {TARGET_LABEL}   (PR-AUC base = prevalencia = {prev:.4f})")
    print("=" * 70)
    print(res[["cv_roc_auc", "cv_roc_auc_sd", "cv_pr_auc", "test_roc_auc",
               "test_pr_auc", "umbral", "recall_sensibilidad", "especificidad",
               "f1", "balanced_acc"]].round(4).to_string())

    best = res["cv_pr_auc"].idxmax()   
    print(f"\n>>> Mejor por CV ROC-AUC: {res['cv_roc_auc'].idxmax()}"
          f"  |  Mejor por CV PR-AUC (campeon): {best}")
    joblib.dump(models[best], os.path.join(MOD_DIR, "mejor_modelo.joblib"))

    fig, ax = plt.subplots(figsize=(6.5, 6))
    for name, p in proba_test.items():
        fpr, tpr, _ = roc_curve(yte, p)
        ax.plot(fpr, tpr, label=f"{name} (AUC={roc_auc_score(yte, p):.3f})")
    ax.plot([0, 1], [0, 1], "k--", alpha=.4)
    ax.set_xlabel("1 - Especificidad"); ax.set_ylabel("Sensibilidad")
    ax.set_title(f"Curvas ROC — {TARGET_LABEL}"); ax.legend()
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/07_roc.png", dpi=130); plt.close(fig)

    fig, ax = plt.subplots(figsize=(6.5, 6))
    for name, p in proba_test.items():
        pr, rc, _ = precision_recall_curve(yte, p)
        ax.plot(rc, pr, label=f"{name} (AP={average_precision_score(yte, p):.3f})")
    ax.axhline(prev, ls="--", color="k", alpha=.4, label=f"Base ({prev:.4f})")
    ax.set_xlabel("Sensibilidad (recall)"); ax.set_ylabel("Precisión")
    ax.set_title(f"Curvas Precisión-Recall — {TARGET_LABEL}"); ax.legend()
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/08_pr.png", dpi=130); plt.close(fig)

    yhat = (proba_test[best] >= thresholds[best]).astype(int)
    cm = confusion_matrix(yte, yhat, labels=[0, 1])
    fig, ax = plt.subplots(figsize=(5, 4.5))
    im = ax.imshow(cm, cmap="Purples")
    for i in range(2):
        for j in range(2):
            ax.text(j, i, f"{cm[i, j]:,}", ha="center", va="center",
                    color="white" if cm[i, j] > cm.max() / 2 else "black", fontsize=12)
    ax.set_xticks([0, 1]); ax.set_xticklabels(["Pred. Sin", "Pred. Con"])
    ax.set_yticks([0, 1]); ax.set_yticklabels(["Real Sin", "Real Con"])
    ax.set_title(f"Matriz de confusión — {best}\n(umbral={thresholds[best]:.4f})")
    fig.colorbar(im, shrink=.7)
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/09_matriz_confusion.png", dpi=130); plt.close(fig)


if __name__ == "__main__":
    main()