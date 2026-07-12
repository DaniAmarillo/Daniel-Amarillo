import os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.inspection import permutation_importance, PartialDependenceDisplay
import shap

OUT_DIR      = "outputs"
TARGET_LABEL = "Hipertensión (I10-I15)"
RANDOM_STATE = 42
SHAP_SAMPLE  = 4000     
RF_TREES     = 300
DROP = {"id", "target", "n_sexos_distintos", "n_dob_distintos", "edad_invalida"}
CAT_COLS = ["sexo", "tipo_benef"]

FIG_DIR = os.path.join(OUT_DIR, "figures")
TBL_DIR = os.path.join(OUT_DIR, "tablas")
os.makedirs(FIG_DIR, exist_ok=True)
os.makedirs(TBL_DIR, exist_ok=True)


def main():
    df = pd.read_parquet(os.path.join(OUT_DIR, "beneficiarios.parquet"))
    y = df["target"].astype(int).values
    feat_cols = [c for c in df.columns if c not in DROP]
    cat_cols = [c for c in CAT_COLS if c in feat_cols]

    X = pd.get_dummies(df[feat_cols], columns=cat_cols, dummy_na=False)
    X = X.fillna(X.median(numeric_only=True)).astype("float64")  
    print(f">>> n={len(y):,} | positivos={y.sum():,} | features={X.shape[1]}")

    Xtr, Xte, ytr, yte = train_test_split(
        X, y, test_size=0.25, stratify=y, random_state=RANDOM_STATE)

    rf = RandomForestClassifier(n_estimators=RF_TREES, min_samples_leaf=5,
                                max_samples=0.5, class_weight="balanced_subsample",
                                n_jobs=-1, random_state=RANDOM_STATE)
    rf.fit(Xtr, ytr)

    pi = permutation_importance(rf, Xte, yte, scoring="roc_auc",
                                n_repeats=5, random_state=RANDOM_STATE, n_jobs=-1)
    imp = (pd.Series(pi.importances_mean, index=X.columns)
             .sort_values(ascending=False))
    imp.to_csv(os.path.join(TBL_DIR, "importancia.csv"))
    print(imp.head(15).round(4).to_string(), "\n")

    top = imp.head(15).iloc[::-1]
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.barh(top.index, top.values, color="#7b2d8b")
    ax.set_title(f"Importancia por permutación (ROC-AUC) — {TARGET_LABEL}")
    ax.set_xlabel("Caída en ROC-AUC al permutar")
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/10_importancia_permutacion.png", dpi=130)
    plt.close(fig)

    pos_idx = np.where(yte == 1)[0]
    neg_idx = np.where(yte == 0)[0]
    n_neg = min(len(neg_idx), max(0, SHAP_SAMPLE - len(pos_idx)))
    rng = np.random.default_rng(RANDOM_STATE)
    sel = np.concatenate([pos_idx, rng.choice(neg_idx, n_neg, replace=False)])
    Xs = Xte.iloc[sel]

    explainer = shap.TreeExplainer(rf)
    raw = explainer.shap_values(Xs)
    if isinstance(raw, list):          
        sv = raw[1]
    elif getattr(raw, "ndim", 2) == 3:  
        sv = raw[:, :, 1]
    else:
        sv = raw

    shap.summary_plot(sv, Xs, plot_type="bar", show=False, max_display=15)
    fig = plt.gcf(); fig.suptitle("SHAP — importancia media", y=1.02)
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/11_shap_bar.png", dpi=130, bbox_inches="tight")
    plt.close(fig)

    shap.summary_plot(sv, Xs, show=False, max_display=15)
    fig = plt.gcf(); fig.suptitle("SHAP — efecto por feature", y=1.02)
    fig.tight_layout(); fig.savefig(f"{FIG_DIR}/12_shap_beeswarm.png", dpi=130, bbox_inches="tight")
    plt.close(fig)

    top_num = [f for f in imp.index if f in df.columns][:4]  
    if top_num:
        print(f">>> PDP para: {top_num}")
        Xpdp = Xtr.sample(min(20000, len(Xtr)), random_state=RANDOM_STATE)
        fig, ax = plt.subplots(figsize=(11, 7))
        PartialDependenceDisplay.from_estimator(
            rf, Xpdp, top_num, ax=ax, n_jobs=-1, grid_resolution=25)
        fig.suptitle(f"Partial Dependence — {TARGET_LABEL}", y=1.02)
        fig.tight_layout(); fig.savefig(f"{FIG_DIR}/13_pdp.png", dpi=130, bbox_inches="tight")
        plt.close(fig)

if __name__ == "__main__":
    main()