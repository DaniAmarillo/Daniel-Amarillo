# JSS Dashboard — Journal of Statistical Software

Dashboard interactivo para explorar el historial de publicaciones del *Journal of Statistical Software* (JSS), construido en R con Shiny. Incluye un pipeline de recolección de datos y visualizaciones interactivas filtradas por fecha, temática y DOI.

---

## Estructura del proyecto

```
├── app.R                        # Dashboard Shiny
├── main.R                       # Pipeline principal de scraping
├── scripts/
│   └── test_update.R            # Script de actualización incremental
├── functions/
│   ├── JSS.R                    # Scraping del sitio de JSS
│   ├── SemanticScholar.R        # Consultas a Semantic Scholar API
│   ├── OpenAlex.R               # Consultas a OpenAlex API
│   ├── Google_Scholar_scrapping.R  # Scraping de citas en Google Scholar
│   └── classify_abstract.R     # Clasificación temática con LLM
└── keys/
    └── BigQuery.json            # Credenciales de Google Cloud (no incluido en git)
```

---

## Pipeline de recolección de datos (`main.R`)

El pipeline construye desde cero las tablas que alimentan el dashboard. Se ejecuta en etapas secuenciales:

### 1. Volúmenes — JSS (scraping)

**Fuente:** sitio web del Journal of Statistical Software ([jstatsoft.org](https://www.jstatsoft.org))  
**Tecnología:** `rvest`  
**Función:** `JSS.Vol()`

Se extraen todos los volúmenes publicados. El último volumen se excluye por estar potencialmente incompleto.

### 2. Artículos — JSS (scraping)

**Fuente:** páginas de cada volumen en JSS  
**Tecnología:** `rvest`, `pbapply`  
**Funciones:** `JSS.Art()`, `JSS.Art.Info()`

Por cada volumen se listan sus artículos y se recupera metadata individual: título, abstract, fecha de publicación y DOI. Se incluye un `Sys.sleep()` entre requests para respetar el servidor.

### 3. Métricas y autores — Semantic Scholar

**Fuente:** [Semantic Scholar API](https://api.semanticscholar.org)  
**Tecnología:** `httr` / `jsonlite`  
**Funciones:** `Semantic.Papers.Batch()`, `Semantic.AuthorInfo.Batch()`

Se consultan en lote los DOIs de los artículos para obtener número de citas, número de referencias, número de autores e identificadores de autores. Para cada autor único se recupera información adicional: nombre, ORCID, perfil de Google Scholar, total de papers y total de citas.

### 4. Completado con OpenAlex

**Fuente:** [OpenAlex API](https://openalex.org)  
**Tecnología:** `httr` / `jsonlite`  
**Función:** `OA.Metrics()`

Los artículos para los cuales Semantic Scholar no devolvió métricas se complementan con OpenAlex, que ofrece cobertura adicional especialmente para publicaciones más antiguas.

### 5. Referencias — OpenAlex

**Fuente:** [OpenAlex API](https://openalex.org)  
**Tecnología:** `httr` / `jsonlite`  
**Funciones:** `OA.ArticleReferences.Batch()`, `OA.ReferenceInfo.Batch()`

Por cada artículo se recupera su lista de referencias. Para cada referencia única se obtiene su metadata: título, número de citas globales y URLs disponibles (DOI, arXiv, PubMed, Semantic Scholar).

### 6. Citas en Google Scholar (complementario)

**Fuente:** [Google Scholar](https://scholar.google.com)  
**Tecnología:** `rvest`  
**Función:** `GS.Citations()`

Consulta alternativa de citas por DOI con reintentos automáticos y espera creciente ante errores de conexión. Se usa como fuente complementaria cuando otras APIs no tienen cobertura.

### 7. Clasificación temática

**Fuente:** abstracts de los artículos  
**Tecnología:** LLM (vía `classify_abstract()`)  
**Categorías:** `Machine Learning`, `Generative AI`, `Statistics`, `Other`

Cada abstract se clasifica automáticamente en una de cuatro categorías temáticas. Los artículos sin abstract se asignan a `Other`.

---

## Almacenamiento — Google BigQuery

Todos los datos se almacenan en un proyecto de Google BigQuery (`jss-dashboard-498723`, dataset `JSS`) con las siguientes tablas:

| Tabla | Descripción |
|---|---|
| `volumes` | Volúmenes publicados con año y URL |
| `articles` | Artículos con metadata, métricas y clasificación temática |
| `authors` | Autores únicos con métricas globales |
| `article_authors` | Relación artículo–autor |
| `refs` | Referencias únicas con metadata y URLs |
| `article_references` | Relación artículo–referencia |

La conexión se realiza con los paquetes `DBI` y `bigrquery`. Los datos se cargan una sola vez al iniciar el dashboard (fuera del server) para minimizar consultas a BigQuery.

---

## Dashboard (`app.R`)

Construido con **Shiny** + **bslib** (`page_sidebar`). Incluye un subheader fijo con métricas globales reactivas y un sidebar con filtros globales.

### Filtros globales (sidebar)

Todos los filtros se aplican en memoria sobre los datos pre-cargados, sin nuevas consultas a BigQuery:

- **Rango de fechas** — filtra por fecha de publicación del artículo. Aplica a todas las pestañas.
- **Temáticas** — selección múltiple por categoría temática. Aplica a Artículos, Referencias y Autores.
- **DOIs** — búsqueda y selección de artículos específicos. Los DOIs disponibles se actualizan dinámicamente según los filtros de fecha y temática activos.

### Subheader

Barra fija con métricas calculadas sobre los artículos filtrados: rango de fechas activo, temáticas seleccionadas, total de artículos, promedio de autores, promedio de citas y promedio de referencias por artículo. Incluye un botón de actualización de datos.

### Pestaña: Volúmenes

Vista general de la actividad editorial por volumen.

- **Composición temática** — gráfico de dona con la distribución global de artículos por categoría.
- **Artículos por volumen** — área apilada con desglose temático por volumen. El tooltip muestra número de volumen, año, total de artículos y porcentaje por categoría.
- **Citas totales** — serie de tiempo por volumen.
- **Promedio de referencias** — serie de tiempo por volumen.
- **Tabla** — métricas agregadas por volumen: artículos, citas promedio, citas totales y promedio de referencias.

### Pestaña: Artículos

Análisis distribucional de métricas a nivel de artículo.

- **Métricas por temática** — boxplots de citas e importance ratio agrupados por categoría temática.
- **Métricas por año** — boxplots de citas e importance ratio agrupados por año de publicación.
- **Tabla** — listado completo de artículos con DOI enlazable, abstract con tooltip, fecha, autores, citas, referencias, importance ratio y temática.

### Pestaña: Referencias

Análisis de las obras más citadas por los artículos del journal.

- **Gráfico de dispersión** — citas globales vs. citas dentro del journal por referencia.
- **Métricas promedio** — tarjetas con promedio de citas globales y promedio de citas en JSS.
- **Tabla** — referencias filtradas por los DOIs activos, agrupadas y ordenadas por número de citas en el journal. Incluye enlace al DOI o URL disponible.

### Pestaña: Autores

Perfil de los autores que publicaron en los artículos filtrados.

- **Boxplot de papers totales** — distribución del total de publicaciones de los autores.
- **Boxplot de citas totales** — distribución del total de citas de los autores.
- **Dispersión papers totales vs. papers en JSS** — permite identificar autores con alta producción global pero poca presencia en el journal y viceversa.
- **Tabla** — autores con número de papers en JSS, papers totales, citas totales y enlaces a ORCID y Google Scholar.

### Actualización de datos

El botón **Actualizar** en el subheader ejecuta un script de actualización incremental (`scripts/test_update.R` / `main.R`) de forma asíncrona usando `future` + `promises`, sin bloquear la sesión del dashboard. El botón refleja el estado del proceso: gris (default), amarillo (procesando) y verde (actualizado). Al finalizar muestra un resumen con artículos, referencias y autores nuevos incorporados y la duración del proceso.

---

## Tecnologías utilizadas

| Categoría | Paquetes / Servicios |
|---|---|
| Dashboard | `shiny`, `bslib`, `shinyjs` |
| Tablas interactivas | `DT` |
| Gráficos | `highcharter` |
| Manipulación de datos | `dplyr`, `stringr` |
| Scraping | `rvest` |
| APIs externas | Semantic Scholar, OpenAlex, Google Scholar |
| Base de datos | Google BigQuery (`DBI`, `bigrquery`) |
| Asincronía | `future`, `promises` |

---

## Autor

**Mendivenson Barragán**  
[mbarraganz@unal.edu.co](mailto:mbarraganz@unal.edu.co) · [GitHub](https://github.com/Mendivenson/data-mining) · [LinkedIn](https://www.linkedin.com/in/mendivenson/)
