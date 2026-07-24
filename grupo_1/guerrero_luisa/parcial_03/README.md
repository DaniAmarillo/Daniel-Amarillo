# Taller 3 - Minería de Datos

Proyecto reproducible para identificar beneficiarios asociados a **dorsalgia**. Se usa la familia CID **M54**: un beneficiario es positivo (`dorsalgia = 1`) si tiene al menos una transacción cuyo CID comienza por `M54`; de lo contrario es negativo. El modelamiento se realiza a nivel de beneficiario y el CID no se utiliza como predictor.

## Datos y estructura

La base real debe ubicarse manualmente en `data/db_2026.csv`. No se incluye en el repositorio por tamaño y privacidad; `.gitignore` impide publicar archivos CSV, Parquet o ZIP de `data/`.

- `01_preparacion_datos.py`: auditoría de calidad, faltantes, CID, costos e inconsistencias.
- `02_variable_objetivo.py`: etiqueta binaria M54 a nivel de beneficiario.
- `03_analisis_descriptivo.py`: indicadores, distribuciones y figuras descriptivas.
- `04_construccion_variables.py`: base agregada de modelamiento sin fuga de información.
- `05_entrenamiento_modelos.py`: auditoría de redundancia/variabilidad y entrenamiento de regresión logística, árbol de decisión, random forest y gradient boosting; XGBoost se añade solo si ya está instalado.
- `06_evaluacion_comparacion.py`: métricas, curvas y selección por PR-AUC/sensibilidad.
- `07_interpretacion_modelo.py`: importancias, coeficientes y análisis de errores.
- `08_estimacion_costos.py`: costos por utilización M54 y modelo exploratorio de costo.
- `outputs/tablas/`, `outputs/figuras/`, `outputs/modelos/`: resultados generados.
- `informe_taller3.md`: informe académico final en Markdown.
- `informe_taller3.pdf`: versión final del informe para entrega.

Todos los scripts usan rutas derivadas de su propia ubicación mediante `pathlib` y crean automáticamente las carpetas de salida.

## Instalación y ejecución

Desde la raíz del proyecto:

```bash
python -m pip install -r requirements.txt
python 01_preparacion_datos.py
python 02_variable_objetivo.py
python 03_analisis_descriptivo.py
python 04_construccion_variables.py
python 05_entrenamiento_modelos.py
python 06_evaluacion_comparacion.py
python 07_interpretacion_modelo.py
python 08_estimacion_costos.py
```

La base pesa aproximadamente 1,66 GB en el entorno de desarrollo. Las etapas transaccionales leen por bloques; su duración y consumo de memoria dependen del número de beneficiarios y utilizaciones únicas. Las tablas y gráficas se guardan en `outputs/tablas` y `outputs/figuras`. Los modelos serializados quedan en `outputs/modelos` y están ignorados por Git.

## Modelos de clasificación

Los modelos obligatorios, alineados con los contenidos de clase, son:

- regresión logística balanceada como línea base interpretable;
- árbol de decisión balanceado y con profundidad limitada;
- random forest balanceado como ensemble de bagging;
- gradient boosting con pesos muestrales balanceados como ensemble de boosting.

XGBoost es opcional: se usa únicamente cuando el paquete `xgboost` ya está disponible y no se exige en `requirements.txt`. KNN y SVM no forman parte del pipeline principal por el tamaño de la base, el fuerte desbalance y su costo computacional. La comparación reporta todas las métricas, pero prioriza PR-AUC, sensibilidad y F1.
