"""
entrenar.py
===========
Script de ENTRENAMIENTO (pesado). Se corre UNA VEZ desde la terminal:

    python entrenar.py

Carga el CSV, hace todo el análisis de calidad de datos, el descriptivo,
entrena todos los modelos de clasificación, hace el tuning de Random Forest,
la validación cruzada repetida, la importancia de variables y la regresión
de costo -- y guarda TODO (tablas, figuras, métricas, el modelo final) en la
carpeta ./artifacts/.

El archivo taller3_cancer_mama.qmd NO entrena nada: solo lee lo que este
script dejó en ./artifacts/ y arma el reporte. Por eso renderizar el .qmd
toma segundos en vez de horas, y puedes cambiar texto/estilo del reporte
sin tener que re-entrenar.

NOTA SOBRE EL FIX DE `n_jobs` (la causa del TerminatedWorkerError):
Cuando envuelves un modelo con `n_jobs=-1` dentro de otra herramienta que
también usa `n_jobs=-1` (cross_val_score, RandomizedSearchCV,
permutation_importance), terminas con procesos que a su vez lanzan más
procesos/threads -> sobre-suscripción de CPU -> en Windows esto mata los
workers de joblib. Regla aplicada en todo este script:
  - Si el modelo se entrena SOLO (una llamada .fit() suelta) -> n_jobs=-1 en el modelo, está bien.
  - Si el modelo va DENTRO de cross_val_score / RandomizedSearchCV / permutation_importance
    (que ya paralelizan por fuera) -> el modelo va con n_jobs=1, y el paralelismo
    se deja únicamente en la herramienta que envuelve (n_jobs=-1 ahí).
"""

import json
import time
import warnings
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")  # sin GUI, más rápido y estable para guardar PNGs
import matplotlib.pyplot as plt

warnings.filterwarnings("ignore")

SEED = 42
PATH = r"C:\Users\johan\Downloads\db_2026.csv"   

# --- Tuning de Random Forest -------------------------------------------------
# El RandomizedSearchCV (40 configs x 5 folds = 200 fits) ya se corrió una vez
# y tardó ~4 horas. Los hiperparámetros óptimos encontrados en esa corrida ya
# se conocen, así que por defecto NO se vuelve a correr la búsqueda: se usan
# esos parámetros directamente para entrenar el modelo final (toma segundos).
#
# Solo si cambias las variables/features, el CSV de entrada, o quieres
# verificar/refrescar el tuning, pon RUN_TUNING = True para repetir la
# búsqueda completa (el código de RandomizedSearchCV sigue documentado más
# abajo, en la sección 9, no se borró).

RUN_TUNING = False

MEJORES_PARAMS_RF = {
    "n_estimators": 300,
    "max_depth": None,
    "min_samples_leaf": 4,
    "min_samples_split": 10,
    "max_features": 0.3,
    "class_weight": "balanced_subsample",
}
MEJOR_PR_AUC_CV_PREVIO = 0.6858  # PR-AUC en CV interna reportado por la búsqueda original

OUT = Path("artifacts")
FIG = OUT / "figuras"
OUT.mkdir(exist_ok=True)
FIG.mkdir(parents=True, exist_ok=True)


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)



# 1. CARGA Y NORMALIZACIÓN

log("1. Cargando y normalizando datos...")


def load_csv(path):
    for enc in ["utf-8", "latin1"]:
        try:
            return pd.read_csv(path, encoding=enc, dtype=str)
        except UnicodeDecodeError:
            continue
    return pd.read_csv(path, encoding="latin1", dtype=str, encoding_errors="replace")


df = load_csv(PATH)

MISSING = {"", "-", "--", ".", "nan", "N/A", "NA", "NAN", "NONE", "NULL", "NAT", "?",
           "SIN INFORMACION", "SEM INFORMACAO",
           "NAO INFORMADO", "NAO INFORMADA", "IGNORADO", "IGNORADA"}


def fold(s):
    return (s.astype("string")
             .str.normalize("NFKD")
             .str.encode("ascii", "ignore").str.decode("ascii"))


def norm_cat(s):
    s = fold(s).str.strip().str.upper().str.replace(r"\s+", " ", regex=True)
    return s.mask(s.isin(MISSING), pd.NA)


cat_cols = ["UTI", "INTERNADO", "PORTE_ANESTESICO", "DESC_ESPECIALIDADE",
            "TIPO_UNIDADE_PREST_HOSPITALAR", "UF_CNES_PREST_HOSPITALAR",
            "TIPO_BENEFICIARIO", "SEXO_BENEFICIARIO", "CETIPO",
            "CD_PROCEDIMENTO", "DESCRICAO_PROCEDIMENTO"]
for c in cat_cols:
    if c in df.columns:
        df[c] = norm_cat(df[c])

sexo_map = {"MASCULINO": "M", "FEMININO": "F", "MASC": "M", "FEM": "F",
            "HOMEM": "M", "MULHER": "F", "MALE": "M", "FEMALE": "F"}
df["SEXO_BENEFICIARIO"] = df["SEXO_BENEFICIARIO"].replace(sexo_map)

df["CID"] = (fold(df["CID"]).str.upper().str.replace(r"[\s\.]", "", regex=True))
df["CID"] = df["CID"].mask(df["CID"].isin(MISSING), pd.NA)

for c in ["DT_UTILIZACAO", "DT_NASCIMENTO_BENEFICIARIO"]:
    df[c] = pd.to_datetime(df[c], errors="coerce", dayfirst=True)

df["VALOR_UTILIZACAO"] = pd.to_numeric(df["VALOR_UTILIZACAO"], errors="coerce")

log(f"   Filas x columnas: {df.shape}")


# 2. CALIDAD DE DATOS
log("2. Calidad de datos...")

# 2.1 Faltantes
miss = (df.isna().mean() * 100).round(1).sort_values(ascending=False)
miss.rename("pct_faltante").reset_index().rename(columns={"index": "columna"}).to_csv(
    OUT / "faltantes.csv", index=False
)

fig, ax = plt.subplots(figsize=(8, 5))
m = miss[miss > 0]
ax.barh(m.index[::-1], m.values[::-1], color="#534AB7")
ax.set_xlabel("% de valores faltantes")
ax.set_title("Faltantes por columna (nivel fila)")
for i, v in enumerate(m.values[::-1]):
    ax.text(v + 1, i, f"{v:.1f}%", va="center", fontsize=9)
ax.set_xlim(0, 100)
fig.tight_layout(); fig.savefig(FIG / "faltantes.png", dpi=130); plt.close(fig)

# 2.2 Inconsistencias
sx = df.groupby("CHAVE_FUNCIONAL")["SEXO_BENEFICIARIO"].nunique(dropna=True)
nb = df.groupby("CHAVE_FUNCIONAL")["DT_NASCIMENTO_BENEFICIARIO"].nunique(dropna=True)
n_multisexo = int((sx > 1).sum())
n_multinac = int((nb > 1).sum())
antes_nac = (df["DT_UTILIZACAO"] < df["DT_NASCIMENTO_BENEFICIARIO"])

# 2.3 Valores extremos
v = df["VALOR_UTILIZACAO"]
n_neg = int((v < 0).sum())
n_cero = int((v == 0).sum())
df["VALOR_LIMPIO"] = v.where(v > 0)
x = df["VALOR_LIMPIO"].dropna()
xmin, xmax = float(x.min()), float(x.max())
media, mediana = float(x.mean()), float(x.median())

fig = plt.figure(figsize=(12, 6))
bins = np.logspace(np.log10(xmin), np.log10(xmax), 50)
plt.hist(x, bins=bins, color="#7EC8E3", edgecolor="white", linewidth=0.8, alpha=0.9)
plt.xscale("log")
plt.axvline(xmin, color="#1B4F72", linestyle=":", linewidth=2, label=f"Mínimo = {xmin:,.2f}")
plt.axvline(media, color="#0E7490", linestyle="--", linewidth=2.2, label=f"Media = {media:,.2f}")
plt.axvline(mediana, color="#D97706", linestyle="-", linewidth=2.5, label=f"Mediana = {mediana:,.2f}")
plt.axvline(xmax, color="#7F1D1D", linestyle=":", linewidth=2, label=f"Máximo = {xmax:,.2f}")
plt.title("Distribución de VALOR_UTILIZACAO (escala logarítmica)", fontsize=15, weight="bold")
plt.xlabel("VALOR_UTILIZACAO (escala log10)"); plt.ylabel("Frecuencia")
plt.grid(True, which="both", linestyle="--", alpha=0.25)
plt.legend(frameon=True); plt.tight_layout()
plt.savefig(FIG / "valor_extremos.png", dpi=130); plt.close(fig)

calidad_datos = {
    "n_multisexo": n_multisexo,
    "n_multinac": n_multinac,
    "n_antes_nacimiento": int(antes_nac.sum()),
    "n_valores_negativos": n_neg,
    "n_valores_cero": n_cero,
    "valor_min": xmin, "valor_max": xmax, "valor_media": media, "valor_mediana": mediana,
}
with open(OUT / "calidad_datos.json", "w", encoding="utf-8") as f:
    json.dump(calidad_datos, f, indent=2, ensure_ascii=False)


# 3. VARIABLE OBJETIVO
log("3. Construyendo variable objetivo...")


def is_mama(cid):
    if pd.isna(cid):
        return False
    return cid.startswith("C50")


df["is_mama_row"] = df["CID"].map(is_mama)
benef_ok = df.dropna(subset=["CHAVE_FUNCIONAL"]).copy()

label = (
    benef_ok.groupby("CHAVE_FUNCIONAL")["is_mama_row"]
    .any().astype(int).rename("MAMA_CANCER")
)

# 4. TABLA A NIVEL BENEFICIARIO
log("4. Construyendo tabla a nivel beneficiario...")

util = benef_ok.copy()

CODIGOS_MAMOGRAFIA = ["40808033", "40808041"]
CODIGOS_USG_MAMA = ["40901114"]
CODIGOS_RM_MAMA = ["41101162"]
CODIGOS_BIOPSIA_PUNCAO = ["40808092", "40808173", "40808220", "40808084", "30602017", "30602181"]
CODIGOS_PATOLOGIA = ["40601110", "40601170"]

util["p_mamografia"] = util["CD_PROCEDIMENTO"].isin(CODIGOS_MAMOGRAFIA)
util["p_usg_mama"] = util["CD_PROCEDIMENTO"].isin(CODIGOS_USG_MAMA)
util["p_rm_mama"] = util["CD_PROCEDIMENTO"].isin(CODIGOS_RM_MAMA)
util["p_biopsias"] = util["CD_PROCEDIMENTO"].isin(CODIGOS_BIOPSIA_PUNCAO)
util["p_patologia"] = util["CD_PROCEDIMENTO"].isin(CODIGOS_PATOLOGIA)

util["p_cualquier_diag_mama"] = (
    util["p_mamografia"] | util["p_usg_mama"] | util["p_rm_mama"] | util["p_biopsias"]
)

util["UTIL_KEY"] = (util["CHAVE_FUNCIONAL"].astype(str) + "|" + util["DT_UTILIZACAO"].astype(str))

esp = util["DESC_ESPECIALIDADE"].astype("string")
util["f_intern"] = util["INTERNADO"].astype("string").str.upper().eq("S")
util["f_uti"] = util["UTI"].astype("string").str.upper().eq("S")
util["f_urg"] = util["CETIPO"].astype("string").str.upper().eq("P")
util["f_onco"] = esp.str.contains("ONCOLOG|MASTOLOG|RADIOTERAP|QUIMIOTERAP|GINECOLOG", case=False, na=False)
util["vpos"] = util["VALOR_UTILIZACAO"].where(util["VALOR_UTILIZACAO"] > 0)

for c in ["DESC_ESPECIALIDADE", "SEXO_BENEFICIARIO", "UF_CNES_PREST_HOSPITALAR", "TIPO_BENEFICIARIO"]:
    util[c] = util[c].astype("category")


def fast_mode(frame, key_col, val_col):
    counts = (frame.groupby([key_col, val_col], observed=True).size()
                    .rename("n").reset_index())
    counts = counts.sort_values("n", ascending=False)
    return counts.drop_duplicates(subset=key_col).set_index(key_col)[val_col]


g = util.groupby("CHAVE_FUNCIONAL")

benef = g.agg(
    n_registros=("CID", "size"),
    n_utilizaciones=("UTIL_KEY", "nunique"),
    n_procedimientos=("CD_PROCEDIMENTO", lambda s: s.notna().sum()),
    n_especialidades_distintas=("DESC_ESPECIALIDADE", lambda s: s.dropna().nunique()),
    n_cids=("CID", lambda s: s.dropna().nunique()),
    n_internaciones=("f_intern", "sum"),
    alguna_internacion=("f_intern", "any"),
    uso_uti=("f_uti", "any"),
    prop_oncologia=("f_onco", "mean"),
    costo_total=("vpos", "sum"),
    costo_promedio=("vpos", "mean"),
    costo_max=("vpos", "max"),
    f_prim=("DT_UTILIZACAO", "min"),
    f_ult=("DT_UTILIZACAO", "max"),
    n_mamografias=("p_mamografia", "sum"),
    tiene_mamografia=("p_mamografia", "any"),
    n_usg_mamas=("p_usg_mama", "sum"),
    n_biopsias=("p_biopsias", "sum"),
    tiene_biopsia=("p_biopsias", "any"),
    n_patologias=("p_patologia", "sum"),
    n_diag_mama_total=("p_cualquier_diag_mama", "sum"),
)

benef["especialidad_principal"] = fast_mode(util, "CHAVE_FUNCIONAL", "DESC_ESPECIALIDADE")
benef["sexo"] = fast_mode(util, "CHAVE_FUNCIONAL", "SEXO_BENEFICIARIO")
benef["uf"] = fast_mode(util, "CHAVE_FUNCIONAL", "UF_CNES_PREST_HOSPITALAR")
benef["tipo_benef"] = fast_mode(util, "CHAVE_FUNCIONAL", "TIPO_BENEFICIARIO")
benef["nacimiento"] = fast_mode(util, "CHAVE_FUNCIONAL", "DT_NASCIMENTO_BENEFICIARIO")

dur_dias = (benef["f_ult"] - benef["f_prim"]).dt.days
benef["duracion_seguimiento_dias"] = dur_dias
benef["dur_meses"] = (dur_dias / 30.44).clip(lower=1)
benef["util_por_mes"] = benef["n_utilizaciones"] / benef["dur_meses"]
benef["proced_por_mes"] = benef["n_procedimientos"] / benef["dur_meses"]

ref = df["DT_UTILIZACAO"].max()
edad = (ref - benef["nacimiento"]).dt.days / 365.25
benef["edad"] = edad.where((edad >= 0) & (edad <= 120))

edad_prim = (benef["f_prim"] - benef["nacimiento"]).dt.days / 365.25
benef["edad_primera_utilizacion"] = edad_prim.where((edad_prim >= 0) & (edad_prim <= 120))

sx2 = util.groupby("CHAVE_FUNCIONAL")["SEXO_BENEFICIARIO"].nunique()
benef["conflicto_sexo"] = (sx2 > 1).reindex(benef.index).fillna(False)

benef = benef.join(label)
benef["MAMA_CANCER"] = benef["MAMA_CANCER"].fillna(0).astype(int)

benef.reset_index().to_parquet(OUT / "benef_modelo.parquet", index=False)
log(f"   benef: {benef.shape}")


# 5. DESCRIPTIVO


log("5. Descriptivo...")

n_benef = benef.shape[0]
n_util = int(benef["n_utilizaciones"].sum())
n_proced = int(benef["n_procedimientos"].sum())
pos = int(benef["MAMA_CANCER"].sum())
prev = pos / n_benef
ratio = (n_benef - pos) / max(pos, 1)

conteos = {
    "n_benef": n_benef, "n_util": n_util, "n_proced": n_proced,
    "positivos": pos, "prevalencia_pct": prev * 100, "ratio_desbalance": ratio,
}
with open(OUT / "conteos.json", "w", encoding="utf-8") as f:
    json.dump(conteos, f, indent=2, ensure_ascii=False)


# 6. COHORTE VS BASE
log("6. Cohorte vs. base general...")


def compara(col, top=8):
    base = benef[col].value_counts(normalize=True, dropna=False) * 100
    coh = benef.loc[benef.MAMA_CANCER == 1, col].value_counts(normalize=True, dropna=False) * 100
    out = pd.concat([base.rename("base_%"), coh.rename("cohorte_%")], axis=1).fillna(0)
    out["lift"] = (out["cohorte_%"] / out["base_%"].replace(0, np.nan)).round(2)
    return out.sort_values("base_%", ascending=False).head(top)


def plot_compara(col, top, fname):
    t = compara(col, top).copy()
    t.index = t.index.astype(str)
    xpos = np.arange(len(t)); w = 0.4
    fig, ax = plt.subplots(figsize=(8, 4.5))
    ax.bar(xpos - w / 2, t["base_%"], w, label="base", color="#B4B2A9")
    ax.bar(xpos + w / 2, t["cohorte_%"], w, label="cohorte cáncer de mama", color="#534AB7")
    ax.set_xticks(xpos); ax.set_xticklabels(t.index, rotation=30, ha="right")
    ax.set_ylabel("%"); ax.set_title(f"{col}: cohorte vs base")
    ax.legend(); fig.tight_layout()
    fig.savefig(FIG / fname, dpi=130); plt.close(fig)
    return t


for col, top, fname in [
    ("sexo", 4, "cmp_sexo.png"),
    ("uf", 6, "cmp_uf.png"),
    ("tipo_benef", 5, "cmp_tipo.png"),
    ("especialidad_principal", 8, "cmp_especialidad.png"),
]:
    tabla = plot_compara(col, top, fname)
    tabla.reset_index().to_csv(OUT / f"compara_{col}.csv", index=False)

# Edad
ok = benef["edad"].notna()
base_e = benef.loc[ok, "edad"]
coh_e = benef.loc[ok & (benef.MAMA_CANCER == 1), "edad"]

fig, ax = plt.subplots(figsize=(8, 4.5))
bins = np.arange(0, 100, 5)
ax.hist(base_e, bins=bins, density=True, alpha=.5, label="base", color="#B4B2A9")
if len(coh_e) > 0:
    ax.hist(coh_e, bins=bins, density=True, alpha=.6, label="cohorte", color="#534AB7")
ax.set_xlabel("edad"); ax.set_ylabel("densidad"); ax.set_title("Edad: cohorte vs base")
ax.legend(); fig.tight_layout(); fig.savefig(FIG / "edad.png", dpi=130); plt.close(fig)

edad_resumen = {
    "pct_disponible": float(ok.mean() * 100),
    "mediana_base": float(base_e.median()),
    "mediana_cohorte": float(coh_e.median()) if len(coh_e) else None,
    "n_cohorte": int(len(coh_e)),
}
with open(OUT / "edad_resumen.json", "w", encoding="utf-8") as f:
    json.dump(edad_resumen, f, indent=2, ensure_ascii=False)

# Valor
vp = df["VALOR_LIMPIO"].dropna()
q = vp.quantile([.5, .9, .99, .999]).round(2)
q.rename("valor").reset_index().rename(columns={"index": "cuantil"}).to_csv(
    OUT / "valor_quantiles.csv", index=False
)

fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
axes[0].hist(np.log10(vp), bins=60, color="#534AB7")
axes[0].set_xlabel("log10(VALOR)"); axes[0].set_ylabel("frecuencia")
axes[0].set_title("VALOR_UTILIZACAO (log10, >0)")
axes[1].boxplot(vp, vert=True, showfliers=False)
axes[1].set_yscale("log"); axes[1].set_ylabel("VALOR (log)")
axes[1].set_title("Boxplot (escala log, sin fliers)")
fig.tight_layout(); fig.savefig(FIG / "valor.png", dpi=130); plt.close(fig)


# 8. MODELAMIENTO PREDICTIVO
log("8. Modelamiento predictivo (esto toma varios minutos)...")

from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier, StackingClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    average_precision_score, roc_auc_score, f1_score,
    recall_score, precision_score, accuracy_score, precision_recall_curve,
)
import xgboost as xgb
import lightgbm as lgb
from catboost import CatBoostClassifier

modelado = benef.copy()
cols_bool = ["alguna_internacion", "uso_uti", "conflicto_sexo", "tiene_mamografia", "tiene_biopsia"]
for col in cols_bool:
    if col in modelado.columns:
        modelado[col] = modelado[col].astype(int)

FEATURES_BASE = [
    "edad", "edad_primera_utilizacion", "conflicto_sexo", "sexo", "uf", "tipo_benef",
    "duracion_seguimiento_dias", "dur_meses", "util_por_mes", "proced_por_mes",
    "n_registros", "n_utilizaciones", "n_procedimientos", "n_especialidades_distintas",
    "n_cids", "n_internaciones", "alguna_internacion", "uso_uti",
    "costo_total", "costo_promedio", "costo_max",
    "n_mamografias", "tiene_mamografia", "n_usg_mamas",
    "n_biopsias", "tiene_biopsia", "n_patologias", "n_diag_mama_total",
]
LEAKY = ["prop_oncologia"]
TARGET = "MAMA_CANCER"
CAT_COLS = ["sexo", "uf", "tipo_benef"]
NUM_COLS = [c for c in FEATURES_BASE if c not in CAT_COLS]

modelado = modelado.reset_index()
y = modelado[TARGET].astype(int).to_numpy()

train_idx, test_idx = train_test_split(
    np.arange(len(modelado)), test_size=0.3, stratify=y, random_state=SEED
)
train, test = modelado.iloc[train_idx], modelado.iloc[test_idx]
y_train, y_test = train[TARGET], test[TARGET]
log(f"   Train: {train.shape} | positivos: {int(y_train.sum())}")
log(f"   Test:  {test.shape} | positivos: {int(y_test.sum())}")


def make_preprocessor(incluir_leakage: bool):
    num_cols = NUM_COLS + (LEAKY if incluir_leakage else [])
    num_pipe = Pipeline([
        ("imputer", SimpleImputer(strategy="median")),
        ("scaler", StandardScaler()),
    ])
    cat_pipe = Pipeline([
        ("imputer", SimpleImputer(strategy="most_frequent")),
        ("onehot", OneHotEncoder(handle_unknown="ignore")),
    ])
    return ColumnTransformer([("num", num_pipe, num_cols), ("cat", cat_pipe, CAT_COLS)])


pos_train = train[train[TARGET] == 1]
neg_train = train[train[TARGET] == 0].sample(n=8000, random_state=SEED)
train_small = pd.concat([pos_train, neg_train]).sample(frac=1, random_state=SEED)
y_train_small = train_small[TARGET]

scale_pos_weight = (y_train == 0).sum() / max((y_train == 1).sum(), 1)


def get_full_data_models():
    # Estos se entrenan con .fit() suelto (sin envoltura paralela externa),
    # asi que n_jobs=-1 en el propio modelo es seguro aqui.
    return {
        "RandomForest": RandomForestClassifier(
            n_estimators=400, max_depth=None, class_weight="balanced",
            n_jobs=-1, random_state=SEED
        ),
        "GradientBoosting": GradientBoostingClassifier(
            n_estimators=300, max_depth=3, learning_rate=0.05, random_state=SEED
        ),
        "XGBoost": xgb.XGBClassifier(
            n_estimators=400, max_depth=4, learning_rate=0.05,
            scale_pos_weight=scale_pos_weight, eval_metric="aucpr",
            n_jobs=-1, random_state=SEED
        ),
        "LightGBM": lgb.LGBMClassifier(
            n_estimators=400, max_depth=-1, learning_rate=0.05,
            class_weight="balanced", n_jobs=-1, random_state=SEED
        ),
        "CatBoost": CatBoostClassifier(
            iterations=400, depth=6, learning_rate=0.05,
            auto_class_weights="Balanced", verbose=False, random_state=SEED
        ),
    }


def get_stacking_model():
    base = [
        ("lgb", lgb.LGBMClassifier(n_estimators=300, class_weight="balanced", n_jobs=-1, random_state=SEED)),
        ("cat", CatBoostClassifier(iterations=300, auto_class_weights="Balanced", verbose=False, random_state=SEED)),
    ]
    return StackingClassifier(
        estimators=base,
        final_estimator=LogisticRegression(class_weight="balanced", max_iter=1000),
        stack_method="predict_proba",
        cv=3,
        n_jobs=1,  # evita doble paralelismo con los n_jobs=-1 de los base learners
    )


def get_small_data_models():
    return {
        "SVM_RBF": SVC(kernel="rbf", class_weight="balanced", probability=True, random_state=SEED),
        "KNN": KNeighborsClassifier(n_neighbors=15, weights="distance"),
    }


def evaluar(nombre, variante, proba, y_true, umbral=0.5):
    pred = (proba >= umbral).astype(int)
    return {
        "modelo": nombre, "variante": variante,
        "PR_AUC": round(average_precision_score(y_true, proba), 4),
        "ROC_AUC": round(roc_auc_score(y_true, proba), 4),
        "F1": round(f1_score(y_true, pred), 4),
        "Recall": round(recall_score(y_true, pred), 4),
        "Precision": round(precision_score(y_true, pred), 4),
        "Accuracy": round(accuracy_score(y_true, pred), 4),
    }


def correr_experimento(incluir_leakage: bool):
    variante = "con_prop_oncologia" if incluir_leakage else "sin_prop_oncologia"
    resultados = []

    prep_full = make_preprocessor(incluir_leakage)
    cols_full = NUM_COLS + (LEAKY if incluir_leakage else []) + CAT_COLS

    Xtr_full = prep_full.fit_transform(train[cols_full])
    Xte_full = prep_full.transform(test[cols_full])

    for nombre, modelo in get_full_data_models().items():
        modelo.fit(Xtr_full, y_train)
        proba = modelo.predict_proba(Xte_full)[:, 1]
        resultados.append(evaluar(nombre, variante, proba, y_test))

    stack = get_stacking_model()
    stack.fit(Xtr_full, y_train)
    proba_stack = stack.predict_proba(Xte_full)[:, 1]
    resultados.append(evaluar("Stacking_LGB_Cat", variante, proba_stack, y_test))

    prep_small = make_preprocessor(incluir_leakage)
    Xtr_small = prep_small.fit_transform(train_small[cols_full])
    Xte_small = prep_small.transform(test[cols_full])

    for nombre, modelo in get_small_data_models().items():
        modelo.fit(Xtr_small, y_train_small)
        proba = modelo.predict_proba(Xte_small)[:, 1]
        resultados.append(evaluar(nombre, variante, proba, y_test))

    return pd.DataFrame(resultados)


log("   Entrenando modelos SIN prop_oncologia...")
res_sin = correr_experimento(incluir_leakage=False)
log("   Entrenando modelos CON prop_oncologia...")
res_con = correr_experimento(incluir_leakage=True)

resultados = pd.concat([res_sin, res_con], ignore_index=True)
resultados = resultados.sort_values(["variante", "PR_AUC"], ascending=[True, False])
resultados.to_csv(OUT / "resultados_modelos_mama.csv", index=False)

# ------------------------------------------------------------
# 8.7 Validación cruzada repetida (n_jobs=1 en el modelo, -1 en cross_val_score)
# ------------------------------------------------------------

log("   Validación cruzada repetida (5x3)...")
from sklearn.model_selection import RepeatedStratifiedKFold, cross_val_score

cols_full_cv = NUM_COLS + CAT_COLS
prep_cv = make_preprocessor(incluir_leakage=False)
X_cv = prep_cv.fit_transform(modelado[cols_full_cv])
y_cv = modelado[TARGET]

cv = RepeatedStratifiedKFold(n_splits=5, n_repeats=3, random_state=SEED)
modelo_cv = lgb.LGBMClassifier(n_estimators=400, class_weight="balanced", n_jobs=1, random_state=SEED)

pr_auc_cv = cross_val_score(modelo_cv, X_cv, y_cv, scoring="average_precision", cv=cv, n_jobs=-1)
roc_auc_cv = cross_val_score(modelo_cv, X_cv, y_cv, scoring="roc_auc", cv=cv, n_jobs=-1)

cv_metrics = {
    "pr_auc_mean": float(pr_auc_cv.mean()), "pr_auc_std": float(pr_auc_cv.std()),
    "roc_auc_mean": float(roc_auc_cv.mean()), "roc_auc_std": float(roc_auc_cv.std()),
}
with open(OUT / "cv_metrics.json", "w", encoding="utf-8") as f:
    json.dump(cv_metrics, f, indent=2, ensure_ascii=False)


# 9. TUNING DEL MODELO FINAL (Random Forest)
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold
from sklearn.inspection import permutation_importance

FEATURES_FINAL_NUM = NUM_COLS
FEATURES_FINAL_CAT = CAT_COLS
FEATURES_FINAL = FEATURES_FINAL_NUM + FEATURES_FINAL_CAT

preprocesador_final = ColumnTransformer([
    ("num", Pipeline([
        ("imputer", SimpleImputer(strategy="median")),
        ("scaler", StandardScaler()),
    ]), FEATURES_FINAL_NUM),
    ("cat", Pipeline([
        ("imputer", SimpleImputer(strategy="most_frequent")),
        ("onehot", OneHotEncoder(handle_unknown="ignore")),
    ]), FEATURES_FINAL_CAT),
])

if RUN_TUNING:
    # ------------------------------------------------------------------
    # Búsqueda completa de hiperparámetros (documentada, ~4 horas la última
    # vez). Solo corre si RUN_TUNING = True en la sección 0 de este script.
    # ------------------------------------------------------------------
    log("9. Tuning de Random Forest (RandomizedSearchCV, RUN_TUNING=True: esto es lento)...")

    # n_jobs=1 en el clasificador MIENTRAS esta dentro de RandomizedSearchCV
    # (el paralelismo real esta en RandomizedSearchCV con n_jobs=-1)
    pipeline_search = Pipeline([
        ("prep", preprocesador_final),
        ("clf", RandomForestClassifier(class_weight="balanced", n_jobs=1, random_state=SEED)),
    ])

    param_dist = {
        "clf__n_estimators": [300, 500, 700, 1000],
        "clf__max_depth": [None, 6, 10, 14, 20],
        "clf__min_samples_leaf": [1, 2, 4, 8, 16],
        "clf__min_samples_split": [2, 5, 10],
        "clf__max_features": ["sqrt", "log2", 0.3, 0.5],
        "clf__class_weight": ["balanced", "balanced_subsample"],
    }

    cv_interno = StratifiedKFold(n_splits=5, shuffle=True, random_state=SEED)

    busqueda = RandomizedSearchCV(
        estimator=pipeline_search,
        param_distributions=param_dist,
        n_iter=40,
        scoring="average_precision",
        cv=cv_interno, n_jobs=-1, random_state=SEED,
        refit=True, verbose=1,
    )
    busqueda.fit(train[FEATURES_FINAL], y_train)

    mejor_score = float(busqueda.best_score_)
    mejores_params = {k.replace("clf__", ""): v for k, v in busqueda.best_params_.items()}
    modelo_final = busqueda.best_estimator_

    log(f"   Mejor PR-AUC en CV interna (nueva búsqueda): {mejor_score:.4f}")
    log(f"   Mejores hiperparámetros (nueva búsqueda): {mejores_params}")
    log("   Considera actualizar MEJORES_PARAMS_RF en la sección 0 con estos valores,")
    log("   así la próxima corrida no necesita RUN_TUNING=True de nuevo.")

else:
    # ------------------------------------------------------------------
    # Ruta rápida (default): NO se repite la búsqueda de 200 fits. Se usan
    # directamente los hiperparámetros óptimos ya encontrados en la corrida
    # documentada arriba (mismo param_dist, mismo cv_interno de 5 folds,
    # mismo scoring="average_precision", SEED=42), y solo se entrena el
    # modelo final con esos parámetros. Esto toma segundos, no horas.
    # ------------------------------------------------------------------
    log("9. Usando hiperparámetros óptimos ya conocidos (RUN_TUNING=False, sin re-buscar)...")

    mejor_score = MEJOR_PR_AUC_CV_PREVIO
    mejores_params = MEJORES_PARAMS_RF

    modelo_final = Pipeline([
        ("prep", preprocesador_final),
        ("clf", RandomForestClassifier(n_jobs=-1, random_state=SEED, **MEJORES_PARAMS_RF)),
    ])
    modelo_final.fit(train[FEATURES_FINAL], y_train)

    log(f"   Hiperparámetros usados: {mejores_params}")

tuning_rf = {
    "best_score": mejor_score,
    "best_params": mejores_params,
    "tuning_reejecutado": RUN_TUNING,
}
with open(OUT / "tuning_rf.json", "w", encoding="utf-8") as f:
    json.dump(tuning_rf, f, indent=2, ensure_ascii=False)

joblib.dump(modelo_final, OUT / "modelo_final.pkl")
log(f"   PR-AUC en CV interna (reportado): {mejor_score:.4f}")

# ------------------------------------------------------------
# 9.3 / 9.4 Evaluación final y ajuste de umbral
# ------------------------------------------------------------
log("   Evaluación final y ajuste de umbral...")


def evaluar_final(nombre, proba, y_true, umbral=0.5):
    pred = (proba >= umbral).astype(int)
    return {
        "modelo": nombre,
        "umbral": round(float(umbral), 4),
        "PR_AUC": round(average_precision_score(y_true, proba), 4),
        "ROC_AUC": round(roc_auc_score(y_true, proba), 4),
        "F1": round(f1_score(y_true, pred), 4),
        "Recall": round(recall_score(y_true, pred), 4),
        "Precision": round(precision_score(y_true, pred), 4),
        "Accuracy": round(accuracy_score(y_true, pred), 4),
    }


proba_test = modelo_final.predict_proba(test[FEATURES_FINAL])[:, 1]
eval_05 = evaluar_final("RandomForest_final_umbral_0.5", proba_test, y_test, umbral=0.5)

precisions, recalls, thresholds = precision_recall_curve(y_test, proba_test)
f1s = 2 * precisions * recalls / (precisions + recalls + 1e-12)
mejor_idx = np.nanargmax(f1s[:-1])
umbral_optimo_f1 = float(thresholds[mejor_idx])
eval_f1 = evaluar_final("RandomForest_final_umbral_optimo_F1", proba_test, y_test, umbral=umbral_optimo_f1)

recall_objetivo = 0.80
idx_candidatos = np.where(recalls[:-1] >= recall_objetivo)[0]
eval_recall80 = None
if len(idx_candidatos):
    idx = idx_candidatos[np.argmax(precisions[idx_candidatos])]
    eval_recall80 = evaluar_final(
        "RandomForest_final_recall_0.80", proba_test, y_test, umbral=float(thresholds[idx])
    )

eval_final = {"umbral_0.5": eval_05, "umbral_optimo_f1": eval_f1, "umbral_recall_0.80": eval_recall80}
with open(OUT / "eval_final.json", "w", encoding="utf-8") as f:
    json.dump(eval_final, f, indent=2, ensure_ascii=False)

# ------------------------------------------------------------
# 9.5 Importancia de variables (n_jobs=1 en el modelo dentro de permutation_importance)
# ------------------------------------------------------------
log("   Importancia de variables (permutation importance)...")
resultado_perm = permutation_importance(
    modelo_final, test[FEATURES_FINAL], y_test,
    scoring="average_precision", n_repeats=20, random_state=SEED, n_jobs=-1,
)
importancias = pd.Series(
    resultado_perm.importances_mean, index=FEATURES_FINAL
).sort_values(ascending=False)
importancias.rename("importancia").reset_index().rename(columns={"index": "variable"}).to_csv(
    OUT / "importancias.csv", index=False
)


# 10. ESTIMACIÓN DEL COSTO ESPERADO

log("10. Regresión de costo (GLM Gamma)...")

import statsmodels.api as sm
import statsmodels.formula.api as smf

mama_rows = df.loc[df["is_mama_row"] & df["VALOR_LIMPIO"].notna()].copy()

mama_rows["tipo_proc_mama"] = np.select(
    [mama_rows["CD_PROCEDIMENTO"].isin(CODIGOS_MAMOGRAFIA),
     mama_rows["CD_PROCEDIMENTO"].isin(CODIGOS_USG_MAMA),
     mama_rows["CD_PROCEDIMENTO"].isin(CODIGOS_RM_MAMA),
     mama_rows["CD_PROCEDIMENTO"].isin(CODIGOS_BIOPSIA_PUNCAO),
     mama_rows["CD_PROCEDIMENTO"].isin(CODIGOS_PATOLOGIA)],
    ["mamografia", "usg", "rm", "biopsia", "patologia"],
    default="otro_procedimiento"
)
mama_rows["internado_f"] = mama_rows["INTERNADO"].astype("string").str.upper().eq("S")
mama_rows["uti_f"] = mama_rows["UTI"].astype("string").str.upper().eq("S")
mama_rows["edad_evento"] = (
    (mama_rows["DT_UTILIZACAO"] - mama_rows["DT_NASCIMENTO_BENEFICIARIO"]).dt.days / 365.25
)
mama_rows["edad_evento"] = mama_rows["edad_evento"].where(
    (mama_rows["edad_evento"] >= 0) & (mama_rows["edad_evento"] <= 120)
)
mama_rows["grupo_edad"] = pd.cut(
    mama_rows["edad_evento"], bins=[0, 40, 50, 60, 70, 120],
    labels=["<40", "40-49", "50-59", "60-69", "70+"]
)

with open(OUT / "costo_prep.json", "w", encoding="utf-8") as f:
    json.dump({"n_filas": int(mama_rows.shape[0])}, f, indent=2, ensure_ascii=False)

# Tablas de resumen de costo (formato largo, una sola tabla)
filas_resumen = []
for c in ["tipo_proc_mama", "DESC_ESPECIALIDADE", "internado_f", "uti_f",
          "grupo_edad", "SEXO_BENEFICIARIO", "UF_CNES_PREST_HOSPITALAR"]:
    g = mama_rows.groupby(c, observed=True)["VALOR_LIMPIO"]
    out = g.agg(n="size", media="mean", mediana="median", sd="std").round(2)
    out = out.sort_values("media", ascending=False).head(6).reset_index()
    out.insert(0, "variable", c)
    out = out.rename(columns={c: "categoria"})
    filas_resumen.append(out)
costo_resumen = pd.concat(filas_resumen, ignore_index=True)
costo_resumen.to_csv(OUT / "costo_resumen.csv", index=False)

# GLM Gamma
cols_modelo = ["VALOR_LIMPIO", "DESC_ESPECIALIDADE", "TIPO_UNIDADE_PREST_HOSPITALAR",
               "edad_evento", "UF_CNES_PREST_HOSPITALAR"]
dm = mama_rows[cols_modelo].dropna(subset=["VALOR_LIMPIO", "edad_evento"]).copy()

p99 = dm["VALOR_LIMPIO"].quantile(0.99)
dm["VALOR_LIMPIO_W"] = dm["VALOR_LIMPIO"].clip(upper=p99)

for c in ["DESC_ESPECIALIDADE", "UF_CNES_PREST_HOSPITALAR", "TIPO_UNIDADE_PREST_HOSPITALAR"]:
    top = dm[c].value_counts().head(6).index
    dm[c] = dm[c].where(dm[c].isin(top), "OTRO")

modelo_costo = smf.glm(
    "VALOR_LIMPIO_W ~ DESC_ESPECIALIDADE + TIPO_UNIDADE_PREST_HOSPITALAR + "
    "edad_evento + UF_CNES_PREST_HOSPITALAR",
    data=dm, family=sm.families.Gamma(link=sm.families.links.Log())
).fit(method="bfgs", maxiter=500)

coef = np.exp(modelo_costo.params).sort_values(ascending=False)
coef.rename("factor_multiplicativo").reset_index().rename(columns={"index": "termino"}).to_csv(
    OUT / "costo_modelo_coef.csv", index=False
)

log("Listo. Todos los artefactos se guardaron en ./artifacts/")
log("Ahora puedes renderizar taller3_cancer_mama.qmd, que solo LEE estos archivos (segundos, no horas).")
