from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import HistGradientBoostingClassifier, RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.inspection import permutation_importance
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    average_precision_score,
    confusion_matrix,
    f1_score,
    precision_recall_curve,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import StratifiedKFold, cross_val_predict, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.tree import DecisionTreeClassifier


PROJECT_DIR = Path(__file__).resolve().parents[1]
TARGET_NAME = "cardio_isquemica"
ID_COL = "CHAVE_FUNCIONAL"
RANDOM_STATE = 42
NEGATIVE_RATIO = 50


def specificity_score(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    tn, fp, fn, tp = confusion_matrix(y_true, y_pred, labels=[0, 1]).ravel()
    return tn / (tn + fp) if (tn + fp) else 0.0


def best_f1_threshold(y_true: np.ndarray, proba: np.ndarray) -> tuple[float, float]:
    precision, recall, thresholds = precision_recall_curve(y_true, proba)
    if thresholds.size == 0:
        return 0.5, 0.0
    f1 = 2 * precision[:-1] * recall[:-1] / np.maximum(precision[:-1] + recall[:-1], 1e-12)
    best_idx = int(np.nanargmax(f1))
    return float(thresholds[best_idx]), float(f1[best_idx])


def metric_row(model_name: str, split: str, y_true: np.ndarray, proba: np.ndarray, threshold: float) -> dict:
    pred = (proba >= threshold).astype(int)
    return {
        "modelo": model_name,
        "split": split,
        "threshold": threshold,
        "pr_auc": average_precision_score(y_true, proba),
        "roc_auc": roc_auc_score(y_true, proba),
        "f1": f1_score(y_true, pred, zero_division=0),
        "precision": precision_score(y_true, pred, zero_division=0),
        "recall_sensibilidad": recall_score(y_true, pred, zero_division=0),
        "especificidad": specificity_score(y_true, pred),
        "positivos_predichos": int(pred.sum()),
    }


def build_preprocessor(X: pd.DataFrame) -> tuple[ColumnTransformer, list[str], list[str]]:
    numeric_cols = X.select_dtypes(include=["number", "bool"]).columns.tolist()
    categorical_cols = [c for c in X.columns if c not in numeric_cols]
    numeric_pipe = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
        ]
    )
    categorical_pipe = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="constant", fill_value="Desconocido")),
            (
                "onehot",
                OneHotEncoder(
                    handle_unknown="infrequent_if_exist",
                    min_frequency=20,
                    sparse_output=False,
                ),
            ),
        ]
    )
    preprocessor = ColumnTransformer(
        transformers=[
            ("num", numeric_pipe, numeric_cols),
            ("cat", categorical_pipe, categorical_cols),
        ],
        remainder="drop",
        verbose_feature_names_out=False,
    )
    return preprocessor, numeric_cols, categorical_cols


def make_models(preprocessor: ColumnTransformer) -> dict[str, Pipeline]:
    return {
        "Regresion logistica": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                (
                    "model",
                    LogisticRegression(
                        max_iter=3000,
                        class_weight="balanced",
                        solver="lbfgs",
                        random_state=RANDOM_STATE,
                    ),
                ),
            ]
        ),
        "Arbol de decision": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                (
                    "model",
                    DecisionTreeClassifier(
                        max_depth=5,
                        min_samples_leaf=20,
                        class_weight="balanced",
                        random_state=RANDOM_STATE,
                    ),
                ),
            ]
        ),
        "Random Forest": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                (
                    "model",
                    RandomForestClassifier(
                        n_estimators=300,
                        max_features="sqrt",
                        min_samples_leaf=5,
                        class_weight="balanced_subsample",
                        random_state=RANDOM_STATE,
                        n_jobs=-1,
                    ),
                ),
            ]
        ),
        "Gradient Boosting": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                (
                    "model",
                    HistGradientBoostingClassifier(
                        max_iter=300,
                        learning_rate=0.05,
                        max_leaf_nodes=31,
                        l2_regularization=0.1,
                        early_stopping=True,
                        class_weight="balanced",
                        random_state=RANDOM_STATE,
                    ),
                ),
            ]
        ),
    }


def get_feature_names(pipe: Pipeline) -> np.ndarray:
    return pipe.named_steps["preprocess"].get_feature_names_out()


def save_model_importance(model_name: str, pipe: Pipeline, X_test: pd.DataFrame, y_test: pd.Series, out_tables: Path) -> None:
    feature_names = get_feature_names(pipe)
    estimator = pipe.named_steps["model"]

    if hasattr(estimator, "coef_"):
        coefs = estimator.coef_.ravel()
        imp = pd.DataFrame({"feature": feature_names, "coeficiente": coefs})
        imp["abs_coeficiente"] = imp["coeficiente"].abs()
        imp.sort_values("abs_coeficiente", ascending=False).head(40).to_csv(
            out_tables / "importancia_regresion_logistica.csv", index=False, encoding="utf-8"
        )

    if hasattr(estimator, "feature_importances_"):
        imp = pd.DataFrame({"feature": feature_names, "importancia": estimator.feature_importances_})
        safe = model_name.lower().replace(" ", "_")
        imp.sort_values("importancia", ascending=False).head(40).to_csv(
            out_tables / f"importancia_{safe}.csv", index=False, encoding="utf-8"
        )

    if len(X_test) > 30_000:
        rng = np.random.default_rng(RANDOM_STATE)
        pos_idx = y_test[y_test == 1].index.to_numpy()
        neg_idx = y_test[y_test == 0].index.to_numpy()
        n_neg = min(len(neg_idx), max(30_000 - len(pos_idx), 1))
        sample_idx = np.concatenate([pos_idx, rng.choice(neg_idx, size=n_neg, replace=False)])
        X_perm = X_test.loc[sample_idx]
        y_perm = y_test.loc[sample_idx]
    else:
        X_perm = X_test
        y_perm = y_test

    perm = permutation_importance(
        pipe,
        X_perm,
        y_perm,
        scoring="average_precision",
        n_repeats=5,
        random_state=RANDOM_STATE,
        n_jobs=1,
    )
    perm_df = pd.DataFrame(
        {
            "feature_original": X_test.columns,
            "importancia_media_pr_auc": perm.importances_mean,
            "importancia_sd": perm.importances_std,
        }
    )
    safe = model_name.lower().replace(" ", "_")
    perm_df.sort_values("importancia_media_pr_auc", ascending=False).head(40).to_csv(
        out_tables / f"permutation_importance_{safe}.csv", index=False, encoding="utf-8"
    )


def make_plots(metrics_test: pd.DataFrame, best_name: str, best_cm: np.ndarray, out_figs: Path) -> None:
    sns.set_theme(style="whitegrid")

    plot_df = metrics_test.melt(
        id_vars=["modelo"],
        value_vars=["pr_auc", "f1", "recall_sensibilidad", "especificidad"],
        var_name="metrica",
        value_name="valor",
    )
    plt.figure(figsize=(10, 5))
    sns.barplot(data=plot_df, x="modelo", y="valor", hue="metrica")
    plt.xticks(rotation=20, ha="right")
    plt.ylim(0, 1)
    plt.title("Comparacion de modelos en prueba")
    plt.tight_layout()
    plt.savefig(out_figs / "comparacion_modelos_test.png", dpi=180)
    plt.close()

    plt.figure(figsize=(5, 4))
    sns.heatmap(best_cm, annot=True, fmt="d", cmap="Blues", cbar=False, xticklabels=["No", "Si"], yticklabels=["No", "Si"])
    plt.xlabel("Prediccion")
    plt.ylabel("Real")
    plt.title(f"Matriz de confusion - {best_name}")
    plt.tight_layout()
    plt.savefig(out_figs / "matriz_confusion_mejor_modelo.png", dpi=180)
    plt.close()


def main() -> None:
    data_path = PROJECT_DIR / "data" / "processed" / "features_cardio_beneficiario.csv.gz"
    if not data_path.exists():
        data_path = PROJECT_DIR / "data" / "processed" / "features_cardio_beneficiario.csv"
    out_tables = PROJECT_DIR / "output" / "tablas"
    out_figs = PROJECT_DIR / "output" / "figuras"
    out_tables.mkdir(parents=True, exist_ok=True)
    out_figs.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(data_path)
    y = df[TARGET_NAME].astype(int)
    drop_cols = [ID_COL, TARGET_NAME, "fecha_nacimiento_moda"]
    X = df.drop(columns=[c for c in drop_cols if c in df.columns])

    X_train, X_test, y_train, y_test = train_test_split(
        X,
        y,
        test_size=0.2,
        stratify=y,
        random_state=RANDOM_STATE,
    )

    rng = np.random.default_rng(RANDOM_STATE)
    train_pos_idx = y_train[y_train == 1].index.to_numpy()
    train_neg_idx = y_train[y_train == 0].index.to_numpy()
    n_neg_sample = min(len(train_neg_idx), max(len(train_pos_idx) * NEGATIVE_RATIO, 1))
    sampled_neg_idx = rng.choice(train_neg_idx, size=n_neg_sample, replace=False)
    fit_idx = np.concatenate([train_pos_idx, sampled_neg_idx])
    rng.shuffle(fit_idx)
    X_fit = X_train.loc[fit_idx]
    y_fit = y_train.loc[fit_idx]

    preprocessor, numeric_cols, categorical_cols = build_preprocessor(X)
    models = make_models(preprocessor)
    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)

    cv_rows = []
    test_rows = []
    threshold_rows = []
    confusion_rows = []
    fitted_models = {}

    for name, pipe in models.items():
        print(f"Training/evaluating: {name}")
        cv_proba = cross_val_predict(pipe, X_fit, y_fit, cv=cv, method="predict_proba", n_jobs=1)[:, 1]
        threshold, best_cv_f1 = best_f1_threshold(y_fit.to_numpy(), cv_proba)
        threshold_rows.append({"modelo": name, "threshold": threshold, "f1_oof_train": best_cv_f1})
        cv_rows.append(metric_row(name, "cv_oof_train_subsample", y_fit.to_numpy(), cv_proba, threshold))

        pipe.fit(X_fit, y_fit)
        test_proba = pipe.predict_proba(X_test)[:, 1]
        test_rows.append(metric_row(name, "test", y_test.to_numpy(), test_proba, threshold))
        test_pred = (test_proba >= threshold).astype(int)
        cm = confusion_matrix(y_test, test_pred, labels=[0, 1])
        for real_idx, real_label in enumerate([0, 1]):
            for pred_idx, pred_label in enumerate([0, 1]):
                confusion_rows.append(
                    {
                        "modelo": name,
                        "real": real_label,
                        "predicho": pred_label,
                        "n": int(cm[real_idx, pred_idx]),
                    }
                )
        fitted_models[name] = (pipe, cm)
        save_model_importance(name, pipe, X_test, y_test, out_tables)

    metrics_cv = pd.DataFrame(cv_rows).sort_values("pr_auc", ascending=False)
    metrics_test = pd.DataFrame(test_rows).sort_values("pr_auc", ascending=False)
    thresholds = pd.DataFrame(threshold_rows)
    confusion = pd.DataFrame(confusion_rows)

    metrics_cv.to_csv(out_tables / "metricas_modelos_cv.csv", index=False, encoding="utf-8")
    metrics_test.to_csv(out_tables / "metricas_modelos_test.csv", index=False, encoding="utf-8")
    thresholds.to_csv(out_tables / "umbrales_modelos.csv", index=False, encoding="utf-8")
    confusion.to_csv(out_tables / "matrices_confusion.csv", index=False, encoding="utf-8")

    best_name = metrics_test.iloc[0]["modelo"]
    best_cm = fitted_models[best_name][1]
    make_plots(metrics_test, best_name, best_cm, out_figs)

    pd.DataFrame(
        [
            {"tipo": "numericas", "n": len(numeric_cols), "variables": ", ".join(numeric_cols)},
            {"tipo": "categoricas", "n": len(categorical_cols), "variables": ", ".join(categorical_cols)},
            {"tipo": "train_completo", "n": len(X_train), "positivos": int(y_train.sum())},
            {"tipo": "train_usado_modelos", "n": len(X_fit), "positivos": int(y_fit.sum())},
            {"tipo": "test", "n": len(X_test), "positivos": int(y_test.sum())},
        ]
    ).to_csv(out_tables / "resumen_modelamiento.csv", index=False, encoding="utf-8")

    print("Model metrics written to output/tablas")
    print(metrics_test.to_string(index=False))


if __name__ == "__main__":
    main()
