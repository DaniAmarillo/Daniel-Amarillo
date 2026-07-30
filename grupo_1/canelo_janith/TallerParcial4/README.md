# Taller 4 — Buscador de artículos científicos

**Minería de Datos (2016325) · Universidad Nacional de Colombia**

Ampliación de la aplicación Streamlit del Taller 2 (dashboard KDD sobre *ACM
Transactions on Graphics*) con un sistema de búsqueda y recuperación de
información sobre los 204 artículos almacenados en la base SQLite del Taller 1.

Aplicación desplegada: `<PEGAR AQUÍ LA URL DE STREAMLIT CLOUD>`

\---

## Ejecución

```bash
git clone <URL-DEL-REPOSITORIO>
cd <carpeta>
pip install -r requirements.txt

python build\_index.py          # construye el índice UNA vez (\~1 s)
python -m streamlit run app.py
```

El buscador necesita el archivo `index/ir\_index.joblib`. Si falta, la app
arranca igual (con el dashboard del Taller 2 completo) y muestra un aviso
pidiendo ejecutar `build\_index.py`. El índice está versionado en el repositorio,
así que en un despliegue limpio no hace falta reconstruirlo.

Para reproducir el documento:

```bash
pip install jupyter matplotlib
jupyter notebook taller\_4.ipynb     # Ejecutar todas las celdas
```

Para regenerar el archivo de evaluación:

```bash
python evaluacion.py               # escribe evaluacion\_consultas.{md,csv}
```

El scraping del Taller 2 sigue funcionando solo en local y requiere las
dependencias adicionales de `requirements-scraper.txt` más Google Chrome.

\---

## Estructura de archivos

|Archivo|Contenido|
|-|-|
|`taller\_4.ipynb`|**Documento reproducible.** Corpus, procesamiento, representación vectorial, reducción dimensional, ranking, evaluación y conclusiones, con el código ejecutado y sus salidas.|
|`app.py`|Aplicación Streamlit. Secciones 1–2 y 4–6 son el Taller 2 intacto; la **sección 3 es el buscador nuevo**.|
|`ir.py`|Módulo de recuperación de información: procesamiento del texto, BM25, TF-IDF, LSA (Truncated SVD), RRF, construcción/carga del índice y función de búsqueda. Lo importan el notebook, `build\_index.py` y `app.py`, de modo que lo documentado es lo que corre.|
|`build\_index.py`|Precómputo del índice → `index/ir\_index.joblib`. Se ejecuta fuera de la app.|
|`evaluacion.py`|Las 6 consultas de prueba, los juicios de relevancia con su criterio escrito, las métricas (P@5, MRR, nDCG@5, Recall@10) y el barrido del número de componentes.|
|`db.py`|Acceso y normalización de la base SQLite (Taller 1/2, sin cambios).|
|`scraper.py`|Actualización por scraping con Selenium (Taller 2, sin cambios).|
|`tog\_q1\_2025.sqlite`|Base de datos: 204 artículos de ACM ToG.|
|`index/ir\_index.joblib`|Índice precomputado: vectorizadores, matriz TF-IDF dispersa, matriz de pesos BM25, modelo SVD y matriz latente (1,3 MB).|
|`evaluacion\_consultas.md`|Consultas, criterios de relevancia, métricas y los 5 primeros resultados de cada estrategia.|
|`evaluacion\_consultas.csv`|Métricas por consulta y estrategia.|
|`evaluacion\_detalle.csv`|Un registro por resultado recuperado, con la marca de relevancia.|
|`requirements.txt`|Dependencias de la app y del buscador.|

\---

## El buscador en dos párrafos

**Corpus.** Cada artículo se representa por su título (repetido 3 veces, para
que pese más que el resumen) seguido del resumen. La base **no tiene palabras
clave** —el scraper del Taller 1 no capturó los CCS Concepts de ACM—, así que el
texto disponible es título + resumen; no se inventó contenido para el campo
ausente. El texto se pasa a minúsculas, se le quitan diacríticos y puntuación
(conservando el guion interno de términos como *real-time*), se tokeniza por
espacios y se descartan *stopwords* inglesas más \~50 términos de relleno
académico. No se aplica *stemming*; la justificación y su costo medido están en
el notebook.

**Estrategias.** Cuatro, seleccionables en la app:

|Estrategia|Tipo|Cómo decide la relevancia|Escala del puntaje|
|-|-|-|-|
|**BM25** (`k1=1.5`, `b=0.75`)|léxica|coincidencia de términos, ponderada por rareza (idf), con saturación de frecuencia y corrección por longitud del documento|no acotada|
|**LSA** = TF-IDF + Truncated SVD|semántica, **reducida**|coseno en un espacio latente de **40 dimensiones**; la consulta se proyecta con el mismo TF-IDF y el mismo SVD|\[-1, 1]|
|**TF-IDF completo**|léxica vectorial|coseno en las 2 943 dimensiones originales; sirve de control para aislar el efecto de la reducción|\[0, 1]|
|**Híbrido (RRF)**|híbrida|fusiona los rankings de BM25 y LSA con `1/(60 + posición)`; solo usa posiciones, así que no exige escalas comparables|\~\[0, 0.033]|

Los puntajes de estrategias distintas **no son comparables** entre sí; el modo
«Comparar BM25 vs LSA» de la app lo advierte y marca los artículos que aparecen
en las dos listas.

## Reducción de dimensionalidad

* **Método:** Truncated SVD (LSA), escogido porque opera directamente sobre la
matriz dispersa —PCA exigiría centrar los datos y densificarla.
* **Dimensión original:** 2 943 (vocabulario TF-IDF de unigramas + bigramas).
* **Dimensión final:** 40 (98,6 % menos; 32,5 % de la varianza).
* **Criterio:** la curva de varianza no tiene codo (corpus temáticamente
homogéneo, rango ≤ 204), así que la elección se hizo por calidad de
recuperación: se reconstruyó el índice para *k* ∈ {20…150} y se evaluaron las
6 consultas. Entre 25 y 55 hay una meseta; se tomó 40 como punto medio para no
sobreajustar a un óptimo puntual.
* **Uso real:** la matriz latente de 204 × 40 produce el **ranking por defecto**
del buscador. La reducción no es un análisis aislado del documento.
* **Memoria:** 32 KiB frente a 176 KiB de la TF-IDF dispersa (y \~4,6 MiB si se
densificara).

## Resultados de la evaluación

Promedio sobre 6 consultas, juicios binarios asignados manualmente sobre el pool
de los 10 primeros resultados de las cuatro estrategias:

|Estrategia|P@5|MRR|nDCG@5|Recall@10|
|-|-|-|-|-|
|BM25|0,433|0,611|0,444|0,614|
|**LSA (reducida)**|**0,500**|**0,769**|**0,535**|0,668|
|TF-IDF completo|0,467|0,625|0,474|0,593|
|**Híbrido (RRF)**|**0,500**|0,667|0,507|**0,735**|

Reducir a 40 dimensiones **mejora** el ranking frente al TF-IDF completo con el
mismo texto y la misma métrica: el SVD actúa como filtro de ruido. La ventaja
viene de las consultas expresadas con sinónimos, donde la coincidencia léxica no
tiene nada que emparejar; en la consulta general, en cambio, BM25 gana, porque
sus términos son literalmente una frase del corpus. Las dos estrategias fallan
por completo en la consulta C5 («clothes … body»): desajuste morfológico sin
*stemming* y polisemia de *«body»*, que en este corpus significa *cuerpo
rígido*. El análisis completo, consulta por consulta, está en el notebook (§7) y
en `evaluacion\_consultas.md`.

## Consideraciones computacionales

* Las matrices de texto se mantienen **dispersas** (CSR); nunca se densifica la
matriz completa.
* Vocabulario recortado con `min\_df=2` y `max\_df=0.5` (de \~5 100 a 2 943
términos), por criterio metodológico además de por memoria.G
* Todas las representaciones se **precalculan** en `build\_index.py` y se cargan
en la app con `@st.cache\_resource`, cuya clave incluye la fecha de
modificación del índice. La app **no** vectoriza el corpus ni entrena el SVD
en ninguna búsqueda: cada consulta es una suma de columnas dispersas o un
producto de 204 × 40.
* Estructuras del buscador en memoria: **< 2 MiB**. Sin modelos neuronales, sin
GPU y sin APIs de pago.
* Si la base tiene más artículos que el índice (por ejemplo tras un scraping),
la app avisa y pide reconstruirlo con `build\_index.py`.

## Funcionalidades del Taller 2 conservadas

Filtros del sidebar (fecha, tema, autor, DOI, título/palabra clave),
indicadores, visualizaciones Plotly, tabla de artículos, consulta SQL de solo
lectura y actualización por scraping. El buscador se agregó como sección 3 y las
secciones siguientes se renumeraron.

