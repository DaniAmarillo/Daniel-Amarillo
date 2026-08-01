from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

SEED = 42


def particionar_train_val_test(
    beneficiarios: pd.DataFrame, columna_target: str, seed: int = SEED
) -> dict[str, np.ndarray]:
    
    y = beneficiarios[columna_target].astype("int8")
    indices = np.arange(len(beneficiarios))

    idx_train, idx_temp = train_test_split(
        indices, test_size=0.30, random_state=seed, stratify=y
    )
    idx_val, idx_test = train_test_split(
        idx_temp, test_size=0.50, random_state=seed, stratify=y.iloc[idx_temp]
    )

    for nombre, idx in [("train", idx_train), ("val", idx_val), ("test", idx_test)]:
        y_sub = y.iloc[idx]
        print(
            f"{nombre.upper():5s} | n={len(idx):6,} | "
            f"positivos={int(y_sub.sum()):3d} | "
            f"prevalencia={y_sub.mean() * 100:.3f}%"
        )

    return {"train": idx_train, "val": idx_val, "test": idx_test}


def particionar_kfold_estratificado(
    beneficiarios: pd.DataFrame,
    columna_target: str,
    n_splits: int = 5,
    test_size: float = 0.15,
    seed: int = SEED,
) -> dict:
    
    from sklearn.model_selection import StratifiedKFold

    y = beneficiarios[columna_target].astype("int8")
    indices = np.arange(len(beneficiarios))

    idx_desarrollo, idx_test = train_test_split(
        indices, test_size=test_size, random_state=seed, stratify=y
    )

    y_desarrollo = y.iloc[idx_desarrollo].reset_index(drop=True)
    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=seed)

    folds = []
    for fold_num, (idx_train_fold, idx_val_fold) in enumerate(
        skf.split(np.zeros(len(idx_desarrollo)), y_desarrollo), start=1
    ):
        n_pos_train = y_desarrollo.iloc[idx_train_fold].sum()
        n_pos_val = y_desarrollo.iloc[idx_val_fold].sum()
        print(
            f"FOLD {fold_num} | train: n={len(idx_train_fold):,} pos={n_pos_train} | "
            f"val: n={len(idx_val_fold):,} pos={n_pos_val}"
        )
        folds.append((idx_train_fold, idx_val_fold))

    n_pos_test = y.iloc[idx_test].sum()
    print(f"TEST (held-out, fuera de todos los folds) | n={len(idx_test):,} pos={n_pos_test}")

    return {"idx_test": idx_test, "idx_desarrollo": idx_desarrollo, "folds": folds}


def crear_preprocesador(
    variables_numericas: list[str], variables_categoricas: list[str], escalar: bool = True
) -> ColumnTransformer:
    
    pasos_numericos = [("imputador", SimpleImputer(strategy="median"))]
    if escalar:
        pasos_numericos.append(("escalador", StandardScaler()))

    pipeline_numerico = Pipeline(steps=pasos_numericos)

    pipeline_categorico = Pipeline(
        steps=[
            ("imputador", SimpleImputer(strategy="most_frequent")),
            ("onehot", OneHotEncoder(handle_unknown="ignore", sparse_output=True)),
        ]
    )

    preprocesador = ColumnTransformer(
        transformers=[
            ("num", pipeline_numerico, variables_numericas),
            ("cat", pipeline_categorico, variables_categoricas),
        ]
    )
    return preprocesador



CONFIGURACIONES_VARIABLES = {
    "A_ESTRUCTURAL": {
        "numericas": ["EDAD"],
        "categoricas": ["SEXO_MODELO", "TIPO_BENEFICIARIO_AGRUPADO", "ESTADO_AGRUPADO"],
    },
    "B_UTILIZACION": {
        "numericas": [
            "EDAD",
            "N_UTILIZACIONES",
            "N_PROCEDIMIENTOS",
            "N_ESPECIALIDADES",
            "N_ESTADOS",
            "COSTO_PROMEDIO",
            "COSTO_DESVEST",
        ],
        "categoricas": [
            "SEXO_MODELO",
            "TIPO_BENEFICIARIO_AGRUPADO",
            "ESTADO_AGRUPADO",
            "UNIDAD_AGRUPADA",
        ],
    },
    "C_AMPLIADO": {
        "numericas": [
            "EDAD",
            "N_UTILIZACIONES",
            "N_PROCEDIMIENTOS",
            "N_ESPECIALIDADES",
            "N_ESTADOS",
            "COSTO_PROMEDIO",
            "COSTO_DESVEST",
            "TUVO_INTERNACION",  # advertencia de leakage: ver feature_engineering.py
            "TUVO_UTI",  # advertencia de leakage: ver feature_engineering.py
        ],
        "categoricas": [
            "SEXO_MODELO",
            "TIPO_BENEFICIARIO_AGRUPADO",
            "ESTADO_AGRUPADO",
            "UNIDAD_AGRUPADA",
            "ESPECIALIDAD_AGRUPADA",
        ],
    },
}
