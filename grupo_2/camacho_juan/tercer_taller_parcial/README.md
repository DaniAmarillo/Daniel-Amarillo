# Taller 3 – Minería de Datos

**Autor:** Juan Andrés Camacho Zárate  
**Grupo:** 2  
**Enfermedad seleccionada:** Cholelithiasis (Colelitiasis)  
**Código CID:** K80 y todas sus subcategorías

## Objetivo

Construir una variable binaria de colelitiasis a nivel de beneficiario, comparar modelos de clasificación, realizar análisis de sensibilidad ante el subregistro de CID y estimar el costo acumulado de los beneficiarios con K80.

## Estructura

```text
tercer_taller_parcial/
├── data/
│   ├── raw/
│   │   └── db_2026.csv
│   └── processed/
│       └── taller3_k80.sqlite
├── figuras/
├── modelos/
├── resultados/
├── scripts/
│   ├── 00_diagnostico_K80_por_bloques.R
│   ├── 01_crear_particiones_k80.R
│   ├── 02_construir_variables_K80_final.R
│   ├── 03_modelamiento_K80.R
│   └── 04_modelamiento_costos_K80.R
├── Taller_3.R
├── taller_3.Rmd
├── taller_3.html
├── README.md
└── .gitignore
```

## Base de datos

La base `db_2026.csv` no se incluye en el repositorio debido a su tamaño y a la naturaleza de la información. Debe ubicarse manualmente en:

```text
data/raw/db_2026.csv
```

## Construcción de la variable objetivo

Para cada `CHAVE_FUNCIONAL`:

- `target_k80 = 1` si existe al menos una transacción cuyo CID normalizado comienza por `K80`.
- `target_k80 = 0` en caso contrario.

La clase negativa representa ausencia de un registro K80 observado y no ausencia clínica confirmada.

## Ejecución

1. Abrir la carpeta como proyecto de RStudio.
2. Colocar `db_2026.csv` en `data/raw/`.
3. Ejecutar:

```r
source("Taller_3.R")
```

4. Compilar:

```r
rmarkdown::render("taller_3.Rmd")
```

## Paquetes requeridos

```r
install.packages(c(
  "DBI",
  "RSQLite",
  "readr",
  "data.table",
  "stringi",
  "Matrix",
  "glmnet",
  "ranger",
  "pROC",
  "PRROC",
  "ggplot2",
  "scales",
  "knitr",
  "kableExtra",
  "rmdformats",
  "here"
))
```

## Metodología

### Clasificación

Se compararon:

- Regresión logística regularizada.
- Random Forest probabilístico.

Se utilizaron particiones estratificadas de entrenamiento, validación y prueba. El umbral se seleccionó en validación maximizando F1. La métrica principal fue PR-AUC, acompañada por ROC-AUC, sensibilidad, especificidad, precisión y F1.

Se realizaron dos análisis:

1. **Principal:** todos los beneficiarios.
2. **Sensibilidad:** solo beneficiarios con algún CID registrado.

### Costos

La unidad de análisis fue el beneficiario con K80. La respuesta fue el costo total positivo acumulado. Se compararon:

- GLM Gamma con enlace log.
- Regresión lognormal con corrección de Duan.
- Línea base basada en la mediana.

La selección se realizó en validación mediante MAE.

## Resultados principales

- 653.631 beneficiarios.
- 306 beneficiarios con K80.
- Prevalencia global aproximada: 0,0468 %.
- Solo 18.951 beneficiarios tienen algún CID registrado.
- Random Forest seleccionado en el análisis principal.
- Regresión logística seleccionada en el análisis de sensibilidad.
- Regresión lognormal seleccionada para los costos.

## Consideraciones

- El fuerte desbalance hace que accuracy y ROC-AUC deban interpretarse junto con PR-AUC, sensibilidad y precisión.
- Las variables directamente asociadas con diagnóstico o tratamiento K80 fueron excluidas del modelo principal para reducir fuga de información.
- Los modelos no sustituyen diagnóstico clínico.
- La estimación de costos es acumulada y depende del periodo observado.
