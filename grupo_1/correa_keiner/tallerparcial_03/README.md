# Taller 3 - Minería de Datos (2016325)

**Enfermedad seleccionada:** ITU (Infección del Tracto Urinario)
**Códigos CID:** N39.0, N30, N10, N11, N12

## Estructura

```
taller_3/
├── data/
│   ├── variables_beneficiario.rds
│   ├── resultados_modelos.rds
│   ├── modelo_costo.rds
│   └── informe_objetos.rds
├── Taller_3.R                    # pipeline completo: carga, calidad de
│                                    datos, variable objetivo, variables,
│                                    modelamiento, interpretación y costos
├── taller_informe.Rmd            # informe final
├── taller_informe.html           # informe renderizado
└── README.md
```

## Cómo correrlo

1. Colocar `db_2026.csv` en `data/`.
2. Correr `taller_3.R` completo-
3. Compilar `taller_3.Rmd` para generar el informe.

## Notas

- La variable objetivo (`itu`) se construye solo con filas que tienen
  `CID` registrado; las variables predictoras se construyen sobre la
  base completa para no subestimar la utilización real del paciente.
- Desbalance de clases: prevalencia ITU ≈ 0.06%. Se maneja con pesos de
  clase / muestreo estratificado, evaluado con PR-AUC y F1 (no accuracy).

## Paquetes requeridos

`tidyverse`, `data.table`, `janitor`, `lubridate`, `stringi`, `caret`,
`randomForest`, `gbm`, `xgboost`, `pROC`, `PRROC`, `broom`
