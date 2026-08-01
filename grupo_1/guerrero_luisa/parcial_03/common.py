"""Utilidades compartidas del Taller 3 (rutas, limpieza y validación)."""
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
DATA = ROOT / "data" / "db_2026.csv"
OUTPUTS = ROOT / "outputs"
TABLES = OUTPUTS / "tablas"
FIGURES = OUTPUTS / "figuras"
MODELS = OUTPUTS / "modelos"
CHUNKSIZE = 250_000
RANDOM_STATE = 42

EXPECTED_COLUMNS = [
    "CID", "UTI", "INTERNADO", "PORTE_ANESTESICO", "DT_UTILIZACAO",
    "DESC_ESPECIALIDADE", "TIPO_UNIDADE_PREST_HOSPITALAR",
    "UF_CNES_PREST_HOSPITALAR", "DT_NASCIMENTO_BENEFICIARIO",
    "TIPO_BENEFICIARIO", "SEXO_BENEFICIARIO", "CETIPO",
    "CD_PROCEDIMENTO", "DESCRICAO_PROCEDIMENTO", "VALOR_UTILIZACAO",
    "CHAVE_FUNCIONAL",
]
MISSING_MARKERS = {"", "-", "N/A", "NA", "NAN", "NONE", "NULL", "SEM INFORMACAO"}


def ensure_directories() -> None:
    for directory in (TABLES, FIGURES, MODELS):
        directory.mkdir(parents=True, exist_ok=True)


def check_data(columns: Iterable[str] | None = None) -> list[str]:
    ensure_directories()
    if not DATA.exists():
        raise FileNotFoundError(
            f"No se encontró {DATA.relative_to(ROOT)}. Ubique allí la base antes de ejecutar."
        )
    available = pd.read_csv(DATA, nrows=0).columns.tolist()
    required = list(columns or [])
    missing = sorted(set(required) - set(available))
    if missing:
        raise ValueError(f"Faltan columnas requeridas en la base: {', '.join(missing)}")
    return available


def clean_text(series: pd.Series) -> pd.Series:
    result = series.astype("string").str.strip().str.upper()
    return result.mask(result.isin(MISSING_MARKERS), pd.NA)


def clean_id(series: pd.Series) -> pd.Series:
    return clean_text(series)


def clean_cid(series: pd.Series) -> pd.Series:
    # Se conserva el punto: M54.5 y M545 son ambos casos válidos mediante startswith.
    return clean_text(series).str.replace(r"\s+", "", regex=True)


def clean_cost(series: pd.Series) -> pd.Series:
    text = series.astype("string").str.strip()
    both = text.str.contains(",", na=False) & text.str.contains(r"\.", na=False)
    text.loc[both] = text.loc[both].str.replace(".", "", regex=False).str.replace(",", ".", regex=False)
    comma_only = text.str.contains(",", na=False) & ~text.str.contains(r"\.", na=False)
    text.loc[comma_only] = text.loc[comma_only].str.replace(",", ".", regex=False)
    return pd.to_numeric(text, errors="coerce")


def is_positive(series: pd.Series) -> pd.Series:
    values = clean_text(series)
    return values.isin({"1", "S", "SIM", "SI", "TRUE", "Y"}) | (pd.to_numeric(values, errors="coerce") > 0)


def mode_or_missing(series: pd.Series):
    valid = series.dropna()
    if valid.empty:
        return pd.NA
    modes = valid.mode()
    return modes.iloc[0] if not modes.empty else valid.iloc[0]


def safe_age(birth: pd.Series, reference: pd.Series) -> pd.Series:
    age = (reference - birth).dt.days / 365.2425
    return age.where(age.between(0, 110)).round(1)


def cetipo_category(series: pd.Series) -> pd.Series:
    """Normaliza códigos o descripciones CETIPO a C/E/T/I/P/O."""
    value = clean_text(series).fillna("")
    first = value.str[0]
    valid = first.where(first.isin(list("CETIP")), "O")
    return valid


def save_csv(frame: pd.DataFrame, name: str) -> None:
    ensure_directories()
    frame.to_csv(TABLES / name, index=False, encoding="utf-8-sig")


def get_score(model, X) -> np.ndarray:
    if hasattr(model, "predict_proba"):
        return model.predict_proba(X)[:, 1]
    score = model.decision_function(X)
    return 1 / (1 + np.exp(-score))
