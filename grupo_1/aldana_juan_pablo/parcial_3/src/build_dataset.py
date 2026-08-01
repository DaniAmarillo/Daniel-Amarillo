from __future__ import annotations

import sys

sys.path.insert(0, "/home/claude/proyecto")

import pandas as pd

from src.data_cleaning import cargar_base, limpiar_base
from src.feature_engineering import (
    CODIGO_CID_ENFERMEDAD,
    agrupar_categoria_poco_frecuente,
    calcular_edad,
    construir_target,
    construir_tabla_demografica,
    construir_variables_utilizacion,
)

RUTA_ORIGEN = "/data/db_2026.csv"
RUTA_SALIDA_BENEFICIARIOS = "/data/beneficiarios.parquet"
RUTA_SALIDA_TRANSACCIONAL = "/data/transaccional_limpia.parquet"

UMBRAL_ESTADO = 30
UMBRAL_UNIDAD = 30
UMBRAL_ESPECIALIDAD = 30


def ejecutar_pipeline(ruta_origen: str = RUTA_ORIGEN) -> tuple[pd.DataFrame, pd.DataFrame]:
    print("=" * 70)
    print("1. CARGA Y LIMPIEZA")
    print("=" * 70)
    df = cargar_base(ruta_origen)
    print(f"Filas cargadas: {len(df):,}")

    df = limpiar_base(df)
    print(f"Filas tras limpieza: {len(df):,}")

    print("\n" + "=" * 70)
    print("2. VARIABLE OBJETIVO")
    print("=" * 70)
    beneficiarios = construir_tabla_demografica(df)
    beneficiarios = construir_target(df, beneficiarios, codigo_cid=CODIGO_CID_ENFERMEDAD)
    target_col = f"TARGET_{CODIGO_CID_ENFERMEDAD}"
    print(beneficiarios[target_col].value_counts())
    print(
        f"Prevalencia en esta base de trabajo: "
        f"{beneficiarios[target_col].mean() * 100:.4f}% "
        f"(nota: esta base sobre-representa positivos por diseño de "
        f"muestreo; ver limitaciones en el notebook)"
    )

    print("\n" + "=" * 70)
    print("3. VARIABLES DE UTILIZACIÓN")
    print("=" * 70)
    variables_uso = construir_variables_utilizacion(df)
    beneficiarios = beneficiarios.merge(variables_uso, on="CHAVE_FUNCIONAL", how="left")
    print(f"Variables agregadas: {list(variables_uso.columns)}")

    print("\n" + "=" * 70)
    print("4. EDAD")
    print("=" * 70)
    fecha_referencia = df["DT_UTILIZACAO"].max()
    beneficiarios["EDAD"] = calcular_edad(beneficiarios, fecha_referencia)
    print(f"Fecha de referencia: {fecha_referencia}")
    print(f"Edades faltantes: {beneficiarios['EDAD'].isna().sum():,}")

    print("\n" + "=" * 70)
    print("5. AGRUPACIÓN DE CATEGORÍAS POCO FRECUENTES")
    print("=" * 70)
    beneficiarios["TIPO_BENEFICIARIO_AGRUPADO"] = agrupar_categoria_poco_frecuente(
        beneficiarios["TIPO_BENEFICIARIO"], UMBRAL_TIPO_BENEFICIARIO
    )
    beneficiarios["ESTADO_AGRUPADO"] = agrupar_categoria_poco_frecuente(
        beneficiarios["ESTADO_PRINCIPAL"], UMBRAL_ESTADO
    )
    beneficiarios["UNIDAD_AGRUPADA"] = agrupar_categoria_poco_frecuente(
        beneficiarios["UNIDAD_PRINCIPAL"], UMBRAL_UNIDAD
    )
    beneficiarios["ESPECIALIDAD_AGRUPADA"] = agrupar_categoria_poco_frecuente(
        beneficiarios["ESPECIALIDAD_PRINCIPAL"], UMBRAL_ESPECIALIDAD
    )
    beneficiarios["SEXO_MODELO"] = beneficiarios["SEXO_BENEFICIARIO"].astype("string")

    print("\n" + "=" * 70)
    print("6. GUARDADO")
    print("=" * 70)
    beneficiarios.to_parquet(RUTA_SALIDA_BENEFICIARIOS, index=False)
    df.to_parquet(RUTA_SALIDA_TRANSACCIONAL, index=False)
    print(f"Tabla de beneficiarios: {RUTA_SALIDA_BENEFICIARIOS} ({beneficiarios.shape})")
    print(f"Tabla transaccional limpia: {RUTA_SALIDA_TRANSACCIONAL} ({df.shape})")

    return beneficiarios, df


if __name__ == "__main__":
    ejecutar_pipeline()
