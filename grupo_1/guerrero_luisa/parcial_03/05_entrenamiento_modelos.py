"""Entrenamiento de modelos de clasificación trabajados en clase."""
import importlib.util

import joblib
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.tree import DecisionTreeClassifier
from sklearn.utils.class_weight import compute_sample_weight

from common import MODELS, RANDOM_STATE, TABLES, ensure_directories, save_csv

MAX_MISSING = 0.95
MAX_DOMINANCE = 0.99
MAX_CORRELATION = 0.95


def audit_and_select_features(X_train: pd.DataFrame) -> tuple[list[str], pd.DataFrame]:
    """Audita y selecciona predictores usando exclusivamente entrenamiento."""
    rows: list[dict] = []
    excluded: set[str] = set()
    for column in X_train.columns:
        series = X_train[column]
        missing = float(series.isna().mean())
        unique = int(series.nunique(dropna=True))
        valid = series.dropna()
        dominance = float(valid.value_counts(normalize=True).iloc[0]) if len(valid) else 1.0
        reason = ""
        if missing >= MAX_MISSING:
            reason = f"faltantes >= {MAX_MISSING:.0%}"
        elif unique <= 1:
            reason = "sin variabilidad (constante o totalmente nula)"
        elif dominance >= MAX_DOMINANCE:
            reason = f"casi constante (dominancia >= {MAX_DOMINANCE:.0%})"
        if reason:
            excluded.add(column)
        rows.append({"variable": column, "tipo": str(series.dtype),
                     "porcentaje_faltantes": 100 * missing, "valores_unicos": unique,
                     "proporcion_categoria_dominante": dominance,
                     "decision": "excluir" if reason else "conservar", "motivo": reason})

    candidates = [c for c in X_train.columns if c not in excluded]
    for position, left in enumerate(candidates):
        if left in excluded:
            continue
        for right in candidates[position + 1:]:
            if right not in excluded and X_train[left].equals(X_train[right]):
                excluded.add(right)
                next(row for row in rows if row["variable"] == right).update(
                    decision="excluir", motivo=f"duplicada de {left}")

    numeric = [c for c in X_train.select_dtypes(include="number").columns if c not in excluded]
    correlation = X_train[numeric].corr().abs() if numeric else pd.DataFrame()
    for right_position, right in enumerate(numeric):
        if right in excluded:
            continue
        for left in numeric[:right_position]:
            value = correlation.loc[left, right]
            if left not in excluded and pd.notna(value) and value > MAX_CORRELATION:
                excluded.add(right)
                next(row for row in rows if row["variable"] == right).update(
                    decision="excluir", motivo=f"correlación |r|={value:.3f} con {left}")
                break
    selected = [c for c in X_train.columns if c not in excluded]
    if not selected:
        raise ValueError("La auditoría excluyó todas las variables predictoras.")
    return selected, pd.DataFrame(rows)


def preprocessor(numeric: list[str], categorical: list[str], scale: bool) -> ColumnTransformer:
    numeric_steps = [("imputador", SimpleImputer(strategy="median"))]
    if scale: numeric_steps.append(("escalador", StandardScaler()))
    return ColumnTransformer([
        ("numericas", Pipeline(numeric_steps), numeric),
        ("categoricas", Pipeline([("imputador", SimpleImputer(strategy="most_frequent")),
                                   ("onehot", OneHotEncoder(handle_unknown="ignore"))]), categorical),
    ], remainder="drop")


def main() -> None:
    print("Iniciando 05_entrenamiento_modelos.py...", flush=True)
    ensure_directories(); path = TABLES / "base_beneficiario_dorsalgia.csv"
    if not path.exists(): raise FileNotFoundError("Ejecute primero 04_construccion_variables.py.")
    print("Cargando la base a nivel de beneficiario...", flush=True)
    data = pd.read_csv(path, low_memory=False); required = {"CHAVE_FUNCIONAL", "dorsalgia"}
    if not required.issubset(data): raise ValueError(f"Faltan columnas: {required-set(data.columns)}")
    ids, y = data["CHAVE_FUNCIONAL"].astype(str), data["dorsalgia"].astype(int)
    X = data.drop(columns=["CHAVE_FUNCIONAL", "dorsalgia"])
    if y.nunique() < 2: raise ValueError("La variable objetivo necesita ambas clases para entrenar.")
    forbidden = [c for c in X.columns if "CID" in c.upper() or c.upper() in
                 {"DESCRICAO_PROCEDIMENTO", "CD_PROCEDIMENTO"}]
    if forbidden:
        raise ValueError(f"Posible fuga de información: predictores prohibidos {forbidden}")
    if y.value_counts().min() < 2:
        raise ValueError("Cada clase necesita al menos dos beneficiarios para un corte estratificado.")
    print("Creando división estratificada de entrenamiento y prueba...", flush=True)
    train_idx, test_idx = train_test_split(data.index, test_size=.2, stratify=y, random_state=RANDOM_STATE)
    if y.loc[test_idx].nunique() < 2:
        raise ValueError("El conjunto de prueba no contiene ambas clases; aumente la muestra o ajuste test_size.")
    print("Auditando variables redundantes, incompletas o sin variabilidad...", flush=True)
    selected, audit = audit_and_select_features(X.loc[train_idx])
    X = X[selected]
    save_csv(audit, "evaluacion_variables_predictoras.csv")
    save_csv(audit[audit["decision"].eq("excluir")], "variables_excluidas.csv")
    save_csv(pd.DataFrame({"variable": selected}), "variables_seleccionadas.csv")
    print(f"Auditoría: {len(selected)} variables conservadas y {len(audit)-len(selected)} excluidas.", flush=True)
    save_csv(pd.DataFrame({"CHAVE_FUNCIONAL": ids.loc[train_idx]}), "train_ids.csv")
    save_csv(pd.DataFrame({"CHAVE_FUNCIONAL": ids.loc[test_idx]}), "test_ids.csv")
    numeric = X.select_dtypes(include="number").columns.tolist(); categorical = X.select_dtypes(exclude="number").columns.tolist()
    save_csv(pd.DataFrame([{"variable": c, "tipo": "numerica"} for c in numeric] +
                          [{"variable": c, "tipo": "categorica"} for c in categorical]), "columnas_modelo.csv")
    models = {
        "modelo_regresion_logistica": Pipeline([("preprocesamiento", preprocessor(numeric, categorical, True)),
            ("modelo", LogisticRegression(max_iter=1500, class_weight="balanced", random_state=RANDOM_STATE))]),
        "modelo_arbol_decision": Pipeline([("preprocesamiento", preprocessor(numeric, categorical, False)),
            ("modelo", DecisionTreeClassifier(max_depth=8, min_samples_leaf=20,
                                               class_weight="balanced", random_state=RANDOM_STATE))]),
        "modelo_random_forest": Pipeline([("preprocesamiento", preprocessor(numeric, categorical, False)),
            ("modelo", RandomForestClassifier(n_estimators=300, max_depth=20, min_samples_leaf=5, class_weight="balanced",
                                               n_jobs=-1, random_state=RANDOM_STATE))]),
        "modelo_gradient_boosting": Pipeline([("preprocesamiento", preprocessor(numeric, categorical, False)),
            ("modelo", GradientBoostingClassifier(n_estimators=150, learning_rate=.05, random_state=RANDOM_STATE))]),
    }
    fit_parameters = {
        "modelo_gradient_boosting": {
            "modelo__sample_weight": compute_sample_weight("balanced", y.loc[train_idx])
        }
    }

    # XGBoost es opcional y no forma parte de requirements.txt. Su ausencia no falla.
    xgb_classifier = None
    if importlib.util.find_spec("xgboost") is not None:
        try:
            from xgboost import XGBClassifier
            xgb_classifier = XGBClassifier
        except (ImportError, OSError) as error:
            print(f"XGBoost fue detectado pero no pudo importarse; se omite: {error}", flush=True)
    if xgb_classifier is not None:
        class_counts = y.loc[train_idx].value_counts()
        scale_pos_weight = class_counts.get(0, 0) / max(class_counts.get(1, 0), 1)
        models["modelo_xgboost"] = Pipeline([
            ("preprocesamiento", preprocessor(numeric, categorical, False)),
            ("modelo", xgb_classifier(n_estimators=300, max_depth=6, learning_rate=.05,
                                     subsample=.8, colsample_bytree=.8,
                                     scale_pos_weight=scale_pos_weight,
                                     objective="binary:logistic", eval_metric="logloss",
                                     tree_method="hist", n_jobs=-1,
                                     random_state=RANDOM_STATE)),
        ])
        print("XGBoost detectado: se incluirá como modelo opcional.", flush=True)
    elif importlib.util.find_spec("xgboost") is None:
        print("XGBoost no está instalado: se omite sin afectar los modelos obligatorios.", flush=True)

    trained_names = []
    for name, model in models.items():
        print(f"Entrenando {name}...", flush=True)
        try:
            model.fit(X.loc[train_idx], y.loc[train_idx], **fit_parameters.get(name, {}))
        except Exception as error:
            if name == "modelo_xgboost":
                print(f"XGBoost opcional no pudo entrenarse y se omite: {error}", flush=True)
                continue
            raise
        joblib.dump(model, MODELS / f"{name}.joblib")
        trained_names.append(name)
        print(f"Guardado: {name}.joblib", flush=True)
    save_csv(pd.DataFrame({"modelo": trained_names}), "modelos_entrenados.csv")
    print(f"Modelos guardados. Train={len(train_idx):,}; test={len(test_idx):,}", flush=True)


if __name__ == "__main__": main()
