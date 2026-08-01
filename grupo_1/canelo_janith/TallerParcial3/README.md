# Uso del proyecto

El flujo de trabajo se divide en dos etapas:

1. Entrenamiento del modelo.
2. Generación del reporte.

---

# 1. Entrenamiento del modelo

Ejecutar desde una terminal ubicada en el directorio del proyecto:

```bash
python entrenar.py
```

Antes de ejecutar, verificar la ruta del conjunto de datos en la variable:

```python
PATH = r"C:\Users\johan\Downloads\db_2026.csv"
```

y actualizarla si el archivo CSV se encuentra en otra ubicación.

## Ajuste de hiperparámetros (Random Forest)

El ajuste mediante `RandomizedSearchCV` **no se ejecuta por defecto**.

La búsqueda original (200 ajustes, aproximadamente 4 horas de ejecución) ya fue realizada y los mejores hiperparámetros obtenidos se encuentran definidos en:

```python
MEJORES_PARAMS_RF
```

(Sección 0 de `entrenar.py`).

Por esta razón, la ejecución normal del script únicamente entrena el modelo final utilizando dichos parámetros, reduciendo el tiempo de ejecución a unos pocos segundos.

El código completo utilizado para el ajuste permanece en el archivo (`Sección 9`), dentro del bloque:

```python
if RUN_TUNING:
```

Si se modifican las variables, el proceso de ingeniería de características o el conjunto de datos, puede ejecutarse nuevamente el ajuste cambiando:

```python
RUN_TUNING = True
```

Una vez finalizado el proceso, los nuevos hiperparámetros obtenidos deberán reemplazar el contenido de `MEJORES_PARAMS_RF`.

## Salidas generadas

Al finalizar la ejecución se crea automáticamente el directorio:

```
artifacts/
```

con la siguiente estructura:

```
artifacts/
├── modelo_final.pkl
├── *.csv
├── *.json
└── figuras/
    └── *.png
```

Este directorio contiene:

* Modelo entrenado.
* Tablas en formato CSV.
* Métricas en formato JSON.
* Figuras utilizadas posteriormente en el reporte.

---

# 2. Generación del reporte

Ubicar el archivo:

```
taller3_cancer_mama.qmd
```

en el mismo directorio donde se encuentra la carpeta `artifacts/`.

Posteriormente ejecutar:

```bash
quarto render taller3_cancer_mama.qmd
```

El documento Quarto no realiza entrenamiento de modelos; únicamente carga la información previamente generada en `artifacts/`, por lo que el proceso de renderizado toma únicamente unos segundos.
