# Taller 3 — Fenotipado Computacional de Insuficiencia Cardíaca (CIE-10 I50)

**Winston Obeymar Lucano Villota**
Minería de Datos — Universidad Nacional de Colombia

---

## Resumen

Modelo de fenotipado computacional concurrente para identificar beneficiarios con
Insuficiencia Cardíaca (CIE-10 I50) en una base de utilización de servicios de salud
del sistema suplementario brasileño (ANS), con estimación del costo esperado asociado
a la enfermedad.

| Aspecto | Valor |
|---|---|
| Beneficiarios analizados | 653.631 |
| Casos positivos (I50) | 104 (0,0159 %) |
| Modelos comparados | Regresión logística, Random Forest, XGBoost |
| ROC-AUC (test) | 0,985 |
| Sensibilidad operativa | 96,8 % |
| Sobrecosto I50 | 38,3× |

---

## Estructura

```
.
├── R/
│   ├── 01_preparacion_datos.R   # Ingesta, calidad, target, features, EDA
│   └── 02_modelado_costos.R     # Modelos, interpretación, costos, calibración
├── data/
│   └── db_2026.csv              # Base de datos (no versionada)
├── figuras/                     # Gráficos generados
├── modelos/                     # Modelos entrenados (.rds)
├── resultados/                  # Tablas generadas (.csv)
├── taller_3.Rmd                 # Informe principal
├── taller_3.pdf                 # Informe compilado
└── README.md
```

---

## Requisitos

- **R** ≥ 4.2
- Paquetes:

```r
install.packages(c("tidyverse", "lubridate", "janitor", "scales",
                   "tidymodels", "xgboost", "ranger", "glmnet",
                   "probably", "dcurves", "broom", "kableExtra"))
```

---

## Cómo reproducir el análisis

1. Coloca la base de datos en `data/db_2026.csv`.

2. Abre R en la raíz del proyecto y ejecuta los scripts **en orden**:

```r
source("R/01_preparacion_datos.R")   # ~5 min
source("R/02_modelado_costos.R")     # ~30-45 min (validación cruzada)
```

Cada script guarda sus salidas en `resultados/`, `figuras/` y `modelos/`. El segundo
script depende de las salidas del primero, por lo que el orden importa.


---

## Notas metodológicas

- **Reproducibilidad:** semilla fija (2026) en todas las etapas estocásticas.
  XGBoost se ejecuta con `nthread = 1` para garantizar determinismo.

- **Sin fuga de datos:** el preprocesamiento se realiza con `recipes`. Los parámetros
  aprendidos (umbral de winsorización, niveles de factores, medianas de imputación) se
  estiman exclusivamente sobre el conjunto de entrenamiento.

- **Naturaleza del modelo:** es un modelo descriptivo de **fenotipado concurrente**,
  no de predicción temprana. Evalúa el patrón anual de consumo médico ya observado
  para determinar si corresponde al de un paciente con I50.

- **Métrica principal:** la sensibilidad en el punto de operación, apropiada para un
  problema de tamizaje con prevalencia de 0,0159 %. El informe discute por qué el
  ROC-AUC (0,985) y el PR-AUC (0,040) resultan aparentemente contradictorios y por qué
  ambos son correctos.

---

## Fuentes de referencia

- OMS — ICD-10 Browser: https://icd.who.int/browse10/2019/en
- ANS — Padrão TISS: https://www.gov.br/ans/pt-br/assuntos/prestadores/padrao-para-troca-de-informacao-de-saude-suplementar-2013-tiss
