
import pandas as pd
import numpy as np

RUTA_ORIGEN = "/mnt/user-data/uploads/muestra_taller.csv"
RUTA_DESTINO = "data/muestra_beneficiarios.csv"
N_BENEFICIARIOS_OBJETIVO = 30_000  # ~14 filas/beneficiario en promedio => apunta a ~300-500k filas resultantes
SEED = 42

DTYPES = {
    "CID": "string",
    "UTI": "int8",
    "INTERNADO": "int8",
    "PORTE_ANESTESICO": "string",
    "DESC_ESPECIALIDADE": "string",
    "TIPO_UNIDADE_PREST_HOSPITALAR": "string",
    "UF_CNES_PREST_HOSPITALAR": "string",
    "TIPO_BENEFICIARIO": "string",
    "SEXO_BENEFICIARIO": "string",
    "CETIPO": "string",
    "CD_PROCEDIMENTO": "string",
    "DESCRICAO_PROCEDIMENTO": "string",
    "VALOR_UTILIZACAO": "float32",
    "CHAVE_FUNCIONAL": "string",
}

# Paso 1: recolectar el universo de CHAVE_FUNCIONAL únicos, identificando
# además cuáles son positivos para la enfermedad seleccionada (CID que
# comienza por "A09"). Dado que A09 tiene muy baja prevalencia, se
# GARANTIZA que todos los positivos queden en la submuestra: de lo
# contrario, el desarrollo del flujo sobre la muestra sería poco
# representativo del problema real.
print("Paso 1/3: identificando beneficiarios únicos y positivos A09...")
base_claves = pd.read_csv(
    RUTA_ORIGEN,
    usecols=["CHAVE_FUNCIONAL", "CID"],
    dtype={"CHAVE_FUNCIONAL": "string", "CID": "string"},
)

cid_limpio = base_claves["CID"].str.strip().str.upper()
es_a09 = cid_limpio.str.startswith("A09", na=False)

claves_positivas = base_claves.loc[es_a09, "CHAVE_FUNCIONAL"].unique()
claves_todas = base_claves["CHAVE_FUNCIONAL"].unique()
claves_negativas_pool = np.setdiff1d(claves_todas, claves_positivas)

print(f"Beneficiarios únicos en la base: {len(claves_todas):,}")
print(f"Beneficiarios positivos A09 (se incluyen TODOS): {len(claves_positivas):,}")

del base_claves, cid_limpio, es_a09

rng = np.random.default_rng(SEED)
n_negativos = min(
    N_BENEFICIARIOS_OBJETIVO - len(claves_positivas),
    len(claves_negativas_pool),
)
claves_negativas_seleccionadas = rng.choice(
    claves_negativas_pool, size=n_negativos, replace=False
)

claves_seleccionadas = set(claves_positivas) | set(claves_negativas_seleccionadas)
print(f"Beneficiarios seleccionados para la muestra: {len(claves_seleccionadas):,}")

# Paso 2: recorrer el archivo por chunks y quedarnos solo con las filas
# de esos beneficiarios.
print("Paso 2/3: filtrando filas por chunks...")
CHUNKSIZE = 300_000
partes = []
filas_leidas = 0

for chunk in pd.read_csv(
    RUTA_ORIGEN,
    dtype=DTYPES,
    parse_dates=["DT_UTILIZACAO", "DT_NASCIMENTO_BENEFICIARIO"],
    chunksize=CHUNKSIZE,
    low_memory=False,
):
    filas_leidas += len(chunk)
    filtro = chunk["CHAVE_FUNCIONAL"].isin(claves_seleccionadas)
    if filtro.any():
        partes.append(chunk.loc[filtro])

print(f"Filas totales leídas: {filas_leidas:,}")

muestra = pd.concat(partes, ignore_index=True)
del partes

print("Paso 3/3: guardando submuestra...")
print(f"Filas en la submuestra: {len(muestra):,}")
print(f"Beneficiarios en la submuestra: {muestra['CHAVE_FUNCIONAL'].nunique():,}")

muestra.to_csv(RUTA_DESTINO, index=False)
print(f"Guardado en: {RUTA_DESTINO}")
