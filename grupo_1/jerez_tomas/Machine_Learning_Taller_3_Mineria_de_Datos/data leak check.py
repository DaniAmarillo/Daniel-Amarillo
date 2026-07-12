import os
import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedKFold, cross_val_score
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import SimpleImputer

OUT_DIR      = "outputs"
RANDOM_STATE = 42
N_SHUFFLES   = 3
DROP = {"id", "target", "n_sexos_distintos", "n_dob_distintos", "edad_invalida"}
CAT_COLS = ["sexo", "tipo_benef"]


def make_model(num, cat):
    pre = ColumnTransformer([
        ("num", SimpleImputer(strategy="median"), num),
        ("cat", Pipeline([("i", SimpleImputer(strategy="most_frequent")),
                          ("o", OneHotEncoder(handle_unknown="ignore",
                                              sparse_output=False))]), cat),
    ])
    clf = HistGradientBoostingClassifier(class_weight="balanced", max_iter=200,
                                         random_state=RANDOM_STATE)
    return Pipeline([("pre", pre), ("clf", clf)])


def auc_cv(X, y, num, cat):
    cv = StratifiedKFold(5, shuffle=True, random_state=RANDOM_STATE)
    return cross_val_score(make_model(num, cat), X, y, cv=cv,
                           scoring="roc_auc", n_jobs=-1)


def main():
    df = pd.read_parquet(os.path.join(OUT_DIR, "beneficiarios.parquet"))
    y = df["target"].astype(int).values
    feat = [c for c in df.columns if c not in DROP]
    cat = [c for c in CAT_COLS if c in feat]
    num = [c for c in feat if c not in cat]
    X = df[feat]

    print("=" * 60)
    print("1) TEST DE PERMUTACION DE ETIQUETAS")
    print("=" * 60)
    real = auc_cv(X, y, num, cat)
    print(f"AUC con etiquetas REALES:     {real.mean():.4f} +- {real.std():.4f}")
    rng = np.random.default_rng(RANDOM_STATE)
    shuf = []
    for k in range(N_SHUFFLES):
        yp = rng.permutation(y)
        a = auc_cv(X, yp, num, cat).mean()
        shuf.append(a)
        print(f"AUC con etiquetas BARAJADAS #{k+1}: {a:.4f}")
    print(f"\n>>> Barajadas (promedio): {np.mean(shuf):.4f}")
    print(">>> Si esto ~0.50 => NO hay fuga. La senal real viene de features legitimas.\n")

    print("=" * 60)
    print("2) ABLACION (que features impulsan la senal)")
    print("=" * 60)
    cost = [c for c in num if c.startswith("costo")]
    groups = {
        "Solo demografia (edad, sexo, tipo)": (["edad"], cat),
        "Solo costo":                         (cost, []),
        "Sin costo":                          ([c for c in num if c not in cost], cat),
        "Todas las features":                 (num, cat),
    }
    for name, (nn, cc) in groups.items():
        a = auc_cv(X[nn + cc], y, nn, cc)
        print(f"{name:38s} AUC = {a.mean():.4f}")


if __name__ == "__main__":
    main()