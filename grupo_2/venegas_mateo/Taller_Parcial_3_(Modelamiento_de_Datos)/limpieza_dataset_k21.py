"""
Script de Limpieza y Normalización de Dataset CID K21
======================================================
Este script realiza una limpieza completa del dataset db_2026.csv:
1. Carga datos y filtra registros sin CID
2. Elimina columnas innecesarias
3. Convierte tipos de datos
4. Normaliza códigos CID K21
5. Elimina registros inválidos
6. Exporta dataset limpio
"""

import pandas as pd
import numpy as np
from pathlib import Path
import warnings
warnings.filterwarnings('ignore')

# ============================================================================
# CONFIGURACIÓN INICIAL
# ============================================================================
RUTA_DATOS_CRUDOS = r'C:\Users\hyalv\OneDrive\Documentos\dm_2016325\grupo_2\venegas_mateo\Taller_Parcial_3_()\db_2026.csv'
RUTA_SALIDA = r'C:\Users\hyalv\OneDrive\Documentos\dm_2016325\grupo_2\venegas_mateo\Taller_Parcial_3_()\db_2026_limpio_K21.csv'

# ============================================================================
# PASO 1: CARGAR DATOS
# ============================================================================
print("=" * 80)
print("PASO 1: CARGAR DATOS DEL DATASET CRUDO")
print("=" * 80)

try:
    df = pd.read_csv(RUTA_DATOS_CRUDOS)
    print(f"[OK] Archivo cargado exitosamente")
    print(f"\n[INFO] Informacion del dataset original:")
    print(f"   * Filas: {df.shape[0]:,}")
    print(f"   * Columnas: {df.shape[1]}")
    print(f"   * Columnas: {df.columns.tolist()}")
except FileNotFoundError:
    print(f"[ERROR] No se encontro el archivo en {RUTA_DATOS_CRUDOS}")
    exit(1)
except Exception as e:
    print(f"[ERROR] Error al cargar archivo: {e}")
    exit(1)

# ============================================================================
# PASO 2: FILTRAR REGISTROS SIN CID
# ============================================================================
print("\n" + "=" * 80)
print("PASO 2: ELIMINAR REGISTROS SIN CID")
print("=" * 80)

filas_antes = len(df)

# Filtrar CID no nulos
df = df[df['CID'].notnull()]
print(f"[+] Filtrados CID nulos: {filas_antes - len(df)} registros eliminados")

# Eliminar registros con CID en blanco
filas_antes = len(df)
df = df[df['CID'].str.strip() != '']
print(f"[+] Eliminados CID en blanco: {filas_antes - len(df)} registros eliminados")

print(f"\n[INFO] Registros después de filtrar CID: {len(df):,}")

# ============================================================================
# PASO 3: ELIMINAR COLUMNAS INNECESARIAS
# ============================================================================
print("\n" + "=" * 80)
print("PASO 3: ELIMINAR COLUMNAS INNECESARIAS")
print("=" * 80)

print(f"\n[INFO] Columnas antes: {df.shape[1]}")

# Definir columnas a eliminar (solo si existen)
columnas_eliminar = ['PORTE_ANESTESICO', 'DESC_ESPECIALIDADE']
columnas_existentes = [col for col in columnas_eliminar if col in df.columns]

if columnas_existentes:
    df = df.drop(columns=columnas_existentes)
    for col in columnas_existentes:
        print(f"[+] Eliminada columna: '{col}'")
else:
    print("[INFO] Las columnas a eliminar no existen en el dataset")

print(f"[INFO] Columnas después: {df.shape[1]}")
print(f"[INFO] Columnas restantes: {df.columns.tolist()}")

# ============================================================================
# PASO 4: CONVERTIR DT_UTILIZACAO A DATETIME
# ============================================================================
print("\n" + "=" * 80)
print("PASO 4: CONVERTIR DT_UTILIZACAO A DATETIME")
print("=" * 80)

if 'DT_UTILIZACAO' in df.columns:
    print(f"[INFO] Tipo de dato antes: {df['DT_UTILIZACAO'].dtype}")
    
    try:
        df = df.astype({'DT_UTILIZACAO': 'datetime64[ns]'})
        print(f"[+] Conversión exitosa")
        print(f"[INFO] Tipo de dato después: {df['DT_UTILIZACAO'].dtype}")
        print(f"[INFO] Rango de fechas: {df['DT_UTILIZACAO'].min().date()} a {df['DT_UTILIZACAO'].max().date()}")
    except Exception as e:
        print(f"[WARN] Error en conversión de fecha: {e}")
else:
    print("[INFO] La columna DT_UTILIZACAO no existe en el dataset")

# ============================================================================
# PASO 5: RENOMBRAR ÍNDICE
# ============================================================================
print("\n" + "=" * 80)
print("PASO 5: RENOMBRAR ÍNDICE A 'N'")
print("=" * 80)

df.index.name = 'N'
print(f"[+] Indice renombrado a 'N'")

# ============================================================================
# PASO 6: FILTRAR REGISTROS CID K21
# ============================================================================
print("\n" + "=" * 80)
print("PASO 6: FILTRAR REGISTROS CON CID K21")
print("=" * 80)

filas_antes = len(df)
print(f"\n[INFO] Registros antes del filtro: {filas_antes:,}")
print(f"[INFO] Valores unicos de CID (primeros 15):")
print(f"   {df['CID'].unique()[:15].tolist()}")

# Filtrar filas que contengan K21
df = df[df['CID'].str.contains("K21", regex=False, na=False, case=False)]

print(f"\n[+] Filtro K21 aplicado")
print(f"[INFO] Registros después del filtro: {len(df):,}")
print(f"[INFO] Registros eliminados: {filas_antes - len(df):,}")

if len(df) > 0:
    print(f"[INFO] Valores unicos de CID después del filtro:")
    print(f"   {df['CID'].unique().tolist()}")
else:
    print("[WARN] Advertencia: No hay registros K21 en el dataset")

# ============================================================================
# PASO 7: NORMALIZAR VALORES CID
# ============================================================================
print("\n" + "=" * 80)
print("PASO 7: NORMALIZAR VALORES CID")
print("=" * 80)

print(f"[INFO] Valores de CID antes de normalizar:")
print(f"   {df['CID'].unique().tolist()}")

# Limpiar espacios y reemplazar variantes
df['CID'] = df['CID'].str.strip().replace(r'^(K21|K21\.0)$', 'K210', regex=True)
print(f"[+] Aplicado: K21 y K21.0 -> K210")

print(f"\n[INFO] Valores después del primer reemplazo:")
print(f"   {df['CID'].unique().tolist()}")

# ============================================================================
# PASO 8: ELIMINAR CID INVÁLIDOS
# ============================================================================
print("\n" + "=" * 80)
print("PASO 8: ELIMINAR CID INVÁLIDOS Y AJUSTES FINALES")
print("=" * 80)

filas_antes = len(df)

# Eliminar registros con dígito 9
df = df[~df['CID'].str.contains('9', na=False)]
print(f"[+] Eliminados registros con digito '9': {filas_antes - len(df):,} registros")

# Reemplazar K21.0 -> K210
filas_antes = len(df)
df['CID'] = df['CID'].replace('K21.0', 'K210')
print(f"[+] Reemplazado K21.0 -> K210")

# Reemplazar K21. -> K210
df['CID'] = df['CID'].str.replace("K21.", "K210", case=False, regex=False)
print(f"[+] Reemplazado K21. -> K210")

print(f"\n[INFO] Valores unicos de CID finales:")
print(f"   {df['CID'].unique().tolist()}")

# ============================================================================
# RESUMEN FINAL
# ============================================================================
print("\n" + "=" * 80)
print("[INFO] RESUMEN FINAL DEL DATASET LIMPIO")
print("=" * 80)

print(f"\n[OK] ESTADO FINAL:")
print(f"   * Filas: {len(df):,}")
print(f"   * Columnas: {df.shape[1]}")
print(f"\n[INFO] Columnas finales:")
for i, col in enumerate(df.columns, 1):
    print(f"   {i}. {col} ({df[col].dtype})")

print(f"\n[INFO] Estadisticas:")
print(f"   * Registros totales: {len(df):,}")
if 'DT_UTILIZACAO' in df.columns:
    print(f"   * Rango de fechas: {df['DT_UTILIZACAO'].min().date()} a {df['DT_UTILIZACAO'].max().date()}")
print(f"   * CID unicos: {df['CID'].nunique()}")
print(f"   * Valores faltantes totales: {df.isnull().sum().sum()}")

if len(df) > 0:
    print(f"\n[INFO] Distribucion CID:")
    print(df['CID'].value_counts().to_string())

# ============================================================================
# EXPORTAR DATASET LIMPIO
# ============================================================================
print("\n" + "=" * 80)
print("[INFO] EXPORTAR DATASET LIMPIO")
print("=" * 80)

try:
    df.to_csv(RUTA_SALIDA, index=True)
    print(f"[OK] Dataset exportado exitosamente")
    print(f"[INFO] Ubicacion: {RUTA_SALIDA}")
    print(f"[INFO] Tamaño: {len(df):,} filas x {df.shape[1]} columnas")
    
    # Verificar que el archivo fue creado
    if Path(RUTA_SALIDA).exists():
        tamaño_mb = Path(RUTA_SALIDA).stat().st_size / (1024 * 1024)
        print(f"[INFO] Tamaño del archivo: {tamaño_mb:.2f} MB")
except Exception as e:
    print(f"[ERROR] Error al exportar: {e}")

print("\n" + "=" * 80)
print("[OK] LIMPIEZA DEL DATASET COMPLETADA EXITOSAMENTE")
print("=" * 80)