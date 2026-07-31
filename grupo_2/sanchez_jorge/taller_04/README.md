# Taller 4 — Minería de Datos

**Sistema de búsqueda y recuperación de artículos científicos**
Jorge Andrés Sánchez Duarte · Universidad Nacional de Colombia

Ampliación de la aplicación del Taller 2 (dashboard de *The Journal of Finance* en Streamlit)
con un módulo de búsqueda que permite escribir una consulta en lenguaje natural y obtener
artículos ordenados por relevancia.

**Aplicación desplegada:** https://mineria-de-datos-baosxpevw5rc3inmltgf6t.streamlit.app/


## Estructura de archivos

| Archivo | Contenido |
|---|---|
| `taller_4.ipynb` | Documento reproducible: corpus, procesamiento, representación vectorial, reducción de dimensionalidad, ranking y evaluación. |
| `buscador.py` | Módulo con toda la lógica del buscador. Lo importan tanto el notebook como la app. |
| `app.py` | Aplicación Streamlit del Taller 2 con la pestaña **Buscador** añadida. |
| `sqlite_webscraping.db` | Base de datos con los 117 artículos extraídos vía Crossref. |
| `artefactos/buscador.joblib` | Objetos precalculados: TF-IDF, matriz dispersa, modelo SVD, matriz reducida e índice BM25. |
| `consultas_evaluacion.csv` | Las 5 consultas de evaluación con Precision@5, MRR y rankings obtenidos. |
| `requirements.txt` | Dependencias. |

## Ejecución

```bash
pip install -r requirements.txt

# 1) (opcional) reconstruir los artefactos desde la base SQLite
python buscador.py

# 2) levantar la aplicación
streamlit run app.py
```

Si `artefactos/buscador.joblib` no existe, la app lo construye automáticamente en el primer
arranque y lo guarda. El notebook se ejecuta de arriba a abajo sin pasos manuales.

## Uso del buscador

En la pestaña **Buscador** se escribe una consulta en lenguaje natural (**en inglés**: el corpus
está íntegramente en ese idioma) y se elige la estrategia de recuperación. Se puede ver una sola
estrategia o compararlas lado a lado.

Consultas de ejemplo:

- `monetary policy transmission and bank lending`
- `deposit insurance and bank runs`
- `algorithmic trading and market liquidity`
- `corporate governance and shareholder voting`

## Estrategias implementadas

| | Estrategia | Tipo | Espacio | Precision |
|---|---|---|---|---|
| **A** | TF-IDF + similitud coseno | léxica | disperso, 1.128 términos | 0.72 |
| **B** | LSA (Truncated SVD) + similitud coseno | semántica latente | **denso, 50 componentes** | **0.92** |
| **C** | BM25 Okapi | léxica, probabilística | índice de tokens | 0.76 |

La estrategia **B** construye su ranking sobre la representación reducida: la matriz TF-IDF de
1.128 dimensiones se proyecta a 50 componentes latentes (60% de varianza retenida), y las consultas
se proyectan al mismo espacio con `svd.transform` antes de calcular la similitud.

El promedio de Precision se calcula sobre las 5 consultas de evaluación documentadas en el
notebook. El detalle está en `consultas_evaluacion.csv`.

## Nota sobre las palabras clave

La API de Crossref no expone palabras clave para *The Journal of Finance*, y el sitio de Wiley
bloquea el scraping automatizado mediante Cloudflare (el mismo inconveniente reportado en el
Taller 2). El corpus se construye entonces con **título** (ponderado ×3), **resumen** y **temática**.
No se reemplazó ningún texto faltante por contenido inventado.






