import pandas as pd
from pathlib import Path


# ============================================================
# CONFIGURACIÓN
# ============================================================

RUTA_BASE = Path("data") / "db_2026.csv"
RUTA_SALIDA = Path("outputs") / "cids_disponibles.csv"

TAMANO_BLOQUE = 500_000


# ============================================================
# VERIFICAR QUE LA BASE EXISTA
# ============================================================

if not RUTA_BASE.exists():
    raise FileNotFoundError(
        f"No se encontró la base en:\n{RUTA_BASE.resolve()}"
    )

print("Base encontrada correctamente.")
print(f"Ruta: {RUTA_BASE.resolve()}\n")


# ============================================================
# LEER SOLO LAS VARIABLES NECESARIAS
# ============================================================

datos_cid = []
filas_procesadas = 0

print("Explorando códigos CID disponibles...\n")

for numero_bloque, bloque in enumerate(
    pd.read_csv(
        RUTA_BASE,
        usecols=["CID", "CHAVE_FUNCIONAL"],
        chunksize=TAMANO_BLOQUE,
        low_memory=False
    ),
    start=1
):

    # Conservar únicamente registros con CID y beneficiario
    bloque = bloque.dropna(
        subset=["CID", "CHAVE_FUNCIONAL"]
    ).copy()

    # Limpiar códigos CID
    bloque["CID"] = (
        bloque["CID"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    # Guardar solamente las dos columnas necesarias
    datos_cid.append(
        bloque[["CID", "CHAVE_FUNCIONAL"]]
    )

    filas_procesadas += len(bloque)

    print(
        f"Bloque {numero_bloque} procesado | "
        f"Filas válidas acumuladas: {filas_procesadas:,}"
    )


# ============================================================
# UNIR INFORMACIÓN
# ============================================================

print("\nUniendo resultados...")

datos_cid = pd.concat(
    datos_cid,
    ignore_index=True
)


# ============================================================
# TOTAL DE BENEFICIARIOS
# ============================================================

total_beneficiarios = (
    datos_cid["CHAVE_FUNCIONAL"]
    .nunique()
)

print(
    f"Total de beneficiarios con CID: "
    f"{total_beneficiarios:,}"
)


# ============================================================
# RESUMEN POR CÓDIGO CID
# ============================================================

tabla_cid = (
    datos_cid
    .groupby("CID")
    .agg(
        numero_registros=("CID", "size"),
        beneficiarios_unicos=(
            "CHAVE_FUNCIONAL",
            "nunique"
        )
    )
    .reset_index()
)


# ============================================================
# CALCULAR PORCENTAJE DE BENEFICIARIOS
# ============================================================

tabla_cid["porcentaje_beneficiarios"] = (
    tabla_cid["beneficiarios_unicos"]
    / total_beneficiarios
    * 100
)

tabla_cid["porcentaje_beneficiarios"] = (
    tabla_cid["porcentaje_beneficiarios"]
    .round(3)
)


# ============================================================
# ORDENAR
# ============================================================

tabla_cid = tabla_cid.sort_values(
    by="beneficiarios_unicos",
    ascending=False
).reset_index(drop=True)


# ============================================================
# MOSTRAR RESULTADOS
# ============================================================

print("\n")
print("=" * 70)
print("CÓDIGOS CID CON MÁS BENEFICIARIOS")
print("=" * 70)

print(
    tabla_cid.head(100).to_string(
        index=False
    )
)

print("\n")
print(
    f"Cantidad de códigos CID diferentes: "
    f"{len(tabla_cid):,}"
)


# ============================================================
# GUARDAR RESULTADOS
# ============================================================

tabla_cid.to_csv(
    RUTA_SALIDA,
    index=False,
    encoding="utf-8-sig"
)

print("\n")
print(
    f"Archivo guardado en:\n"
    f"{RUTA_SALIDA.resolve()}"
)

# ============================================================
# VERIFICAR FAMILIA M54
# ============================================================

beneficiarios_m54 = set()

for bloque in pd.read_csv(
    RUTA_BASE,
    usecols=["CID", "CHAVE_FUNCIONAL"],
    chunksize=TAMANO_BLOQUE,
    low_memory=False
):

    bloque = bloque.dropna(
        subset=["CID", "CHAVE_FUNCIONAL"]
    ).copy()

    bloque["CID"] = (
        bloque["CID"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    casos_m54 = bloque[
        bloque["CID"].str.startswith("M54")
    ]

    beneficiarios_m54.update(
        casos_m54["CHAVE_FUNCIONAL"].unique()
    )

print("\n" + "=" * 70)
print("RESULTADO PARA DORSALGIA - M54")
print("=" * 70)

print(
    f"Beneficiarios únicos con algún CID M54: "
    f"{len(beneficiarios_m54):,}"
)