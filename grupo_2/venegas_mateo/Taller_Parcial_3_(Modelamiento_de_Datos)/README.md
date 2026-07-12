# Taller 3 – Minería de Datos (2016325)

**Estudiante:** Mateo Venegas  
**Enfermedad seleccionada:** Enfermedad por Reflujo Gastroesofágico con Esofagitis — **K21.0** (CIE-10)  
**Fecha:** Julio 2026

---

## Descripción del análisis

Este taller desarrolla un análisis completo de minería de datos aplicado a una base de datos real de utilización de servicios de salud (Brasil, salud suplementaria). Se seleccionó la condición **K21.0 (ERGE con esofagitis)** como variable objetivo binaria, identificando 38 beneficiarios positivos dentro de la base original de ~653.000 beneficiarios.

El análisis incluye:

1. **Comprensión y calidad de datos** — valores faltantes, inconsistencias, valores extremos
2. **Construcción de la variable objetivo** — binaria a nivel de beneficiario (K21 vs No K21)
3. **Análisis descriptivo** — distribución por sexo, edad, estado, tipo de beneficiario, tipo de utilización y costo
4. **Ingeniería de variables** — agregación a nivel de beneficiario (n_utilizaciones, costos, internaciones, etc.)
5. **Modelamiento predictivo** — Regresión Logística LASSO, Random Forest y XGBoost
6. **Evaluación de modelos** — ROC-AUC (métrica principal), PR-AUC, sensibilidad, especificidad, F1
7. **Interpretación** — importancia de variables (RF Gini), coeficientes LASSO, análisis de errores
8. **Estimación de costos** — costo promedio observado, modelo de regresión log-lineal, factores asociados
9. **Conclusiones y limitaciones**

---

## Archivos del proyecto

| Archivo | Descripción |
|---------|-------------|
| `Taller_Parcial_3.Rmd` | Notebook principal con todo el análisis, código y narrativa |
| `Taller_Parcial_3.nb.html` | Salida HTML del notebook (resultado final) |
| `db_2026_limpio_K21.csv` | Dataset limpio filtrado a registros K21 (generado por el script Python) |
| `db_2026.csv` | Dataset original completo (necesario para construir controles negativos) |
| `limpieza_dataset_k21.py` | Script Python de limpieza y filtrado de datos |
| `df_modelo_raw.rds` | Caché intermedia de datos procesados (acelera re-ejecuciones) |

---

## Instrucciones de reproducción

### Requisitos

- **R ≥ 4.5**
- **Paquetes R:**

```r
install.packages(c(
  "data.table", "tidyverse", "skimr", "tidymodels",
  "ranger", "xgboost", "glmnet", "pROC", "themis",
  "kableExtra", "ROCR", "patchwork"
))
```

### Ejecución

1. Asegurarse de que los archivos `db_2026.csv` y `db_2026_limpio_K21.csv` estén en el mismo directorio que el `.Rmd`.

2. Abrir `Taller_Parcial_3.Rmd` en RStudio y ejecutar:

```r
rmarkdown::render("Taller_Parcial_3.Rmd")
```

O usar el botón **Knit** en RStudio.

### Notas de tiempo de ejecución

| Paso | Tiempo estimado |
|------|----------------|
| Primera ejecución (lee 9.3M filas) | 5–8 minutos |
| Re-ejecuciones (usa caché `.rds`) | 1–2 minutos |
| Entrenamiento de modelos (CV) | 3–5 minutos |

Para forzar un reprocesamiento completo, eliminar `df_modelo_raw.rds`.

---

## Decisiones metodológicas clave

| Decisión | Justificación |
|----------|---------------|
| Enfermedad: K21.0 | Presente en la base; patrón de utilización bien definido; códigos CID verificados en CIE-10 |
| Muestra de negativos: 3.000 | Balance entre representatividad y capacidad computacional |
| Métrica principal: ROC-AUC | Más apropiada que accuracy dado el desbalance extremo (~1.25% positivos) |
| Pesos por clase en RF/XGBoost | Compensar el desbalance sin oversampling excesivo con n=38 positivos |
| Eliminación de fechas de nacimiento inválidas | Fechas futuras (>2026) se tratan como NA; no implican eliminación del registro |
| Log-transformación en modelo de costo | La distribución de costos es fuertemente sesgada a la derecha |

---

## Limitaciones principales

- **n = 38 positivos** es muy pequeño para un clasificador robusto; los resultados tienen alta varianza.
- `DESC_ESPECIALIDADE` fue eliminada en la limpieza, perdiendo información potencialmente valiosa.
- Algunos beneficiarios tienen fechas de nacimiento inválidas (futuras), impidiendo calcular la edad.
- El modelo de costo se ajusta con 1.815 registros de solo 38 pacientes.

---

*Asignatura: Minería de Datos (2016325) | Universidad Nacional de Colombia | 2026*
