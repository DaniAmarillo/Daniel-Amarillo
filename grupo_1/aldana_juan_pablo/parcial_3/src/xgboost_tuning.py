from __future__ import annotations

import numpy as np
import optuna
import pandas as pd
from sklearn.metrics import average_precision_score
from xgboost import XGBClassifier

from src.modeling_prep import crear_preprocesador
from sklearn.pipeline import Pipeline

optuna.logging.set_verbosity(optuna.logging.WARNING)


def optimizar_xgboost(
    beneficiarios: pd.DataFrame,
    columna_target: str,
    variables_numericas: list[str],
    variables_categoricas: list[str],
    indices: dict,
    n_trials: int = 40,
    seed: int = 42,
) -> optuna.Study:
    
    variables = variables_numericas + variables_categoricas
    X = beneficiarios[variables]
    y = beneficiarios[columna_target].astype("int8")

    X_train, X_val = X.iloc[indices["train"]], X.iloc[indices["val"]]
    y_train, y_val = y.iloc[indices["train"]], y.iloc[indices["val"]]

    preprocesador = crear_preprocesador(variables_numericas, variables_categoricas, escalar=False)
    X_train_proc = preprocesador.fit_transform(X_train)
    X_val_proc = preprocesador.transform(X_val)

    ratio_desbalance = (y_train == 0).sum() / max((y_train == 1).sum(), 1)

    def objetivo(trial: optuna.Trial) -> float:
        parametros = {
            "n_estimators": trial.suggest_int("n_estimators", 100, 500, step=50),
            "max_depth": trial.suggest_int("max_depth", 2, 8),
            "learning_rate": trial.suggest_float("learning_rate", 0.01, 0.3, log=True),
            "subsample": trial.suggest_float("subsample", 0.6, 1.0),
            "colsample_bytree": trial.suggest_float("colsample_bytree", 0.6, 1.0),
            "min_child_weight": trial.suggest_int("min_child_weight", 1, 10),
            "reg_alpha": trial.suggest_float("reg_alpha", 1e-3, 10.0, log=True),
            "reg_lambda": trial.suggest_float("reg_lambda", 1e-3, 10.0, log=True),
            "scale_pos_weight": trial.suggest_float(
                "scale_pos_weight", ratio_desbalance * 0.5, ratio_desbalance * 2.0
            ),
        }

        modelo = XGBClassifier(
            **parametros,
            random_state=seed,
            n_jobs=-1,
            eval_metric="aucpr",
        )
        modelo.fit(X_train_proc, y_train)
        prob_val = modelo.predict_proba(X_val_proc)[:, 1]
        return average_precision_score(y_val, prob_val)

    study = optuna.create_study(direction="maximize", sampler=optuna.samplers.TPESampler(seed=seed))
    study.optimize(objetivo, n_trials=n_trials, show_progress_bar=False)

    return study


def crear_xgboost_desde_estudio(study: optuna.Study, seed: int = 42) -> XGBClassifier:
    """Instancia un XGBClassifier con los mejores hiperparámetros encontrados por Optuna."""
    return XGBClassifier(
        **study.best_params,
        random_state=seed,
        n_jobs=-1,
        eval_metric="aucpr",
    )
