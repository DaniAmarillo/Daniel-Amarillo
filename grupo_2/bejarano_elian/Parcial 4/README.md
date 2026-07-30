# Entropy Dashboard - Talleres 2 y 4

Aplicacion interactiva en **R Shiny** para explorar, actualizar y recuperar articulos cientificos de la revista **Entropy (MDPI)**. El proyecto parte del dashboard desarrollado en el Taller 2 y lo amplia en el Taller 4 con un sistema de busqueda en lenguaje natural, ranking de relevancia, comparacion de estrategias y reduccion dimensional.

Aplicacion desplegada: [https://elianstevenbejarano.shinyapps.io/files/](https://elianstevenbejarano.shinyapps.io/files/)

## Proposito

El dashboard integra el flujo KDD trabajado en clase:

1. **Adquisicion:** web scraping de articulos y metricas desde MDPI.
2. **Almacenamiento:** persistencia de los articulos en SQLite.
3. **Preparacion:** limpieza de fechas, autores, categorias, resumenes y metricas.
4. **Consulta:** filtros reactivos y tablas dinamicas.
5. **Mineria:** indicadores, graficos, rankings, outliers y busqueda textual.
6. **Actualizacion:** incorporacion o verificacion de articulos nuevos.

## Funcionalidades principales

- Conexion directa a `entropy_2025.sqlite`.
- Filtros por fecha, tematica, titulo, autor, DOI, citas, descargas y completitud de metricas.
- Indicadores descriptivos: total de articulos, promedio de autores, citas, referencias y descargas.
- Visualizaciones interactivas con `highcharter`.
- Tablas dinamicas con `DT`.
- Modulo de scraping configurable por ano, volumen, issue, limite y modo de consulta.
- Modulo de busqueda cientifica para consultas en lenguaje natural.
- Comparacion entre **TF-IDF + coseno** y **LSA/SVD + coseno**.
- Evaluacion de resultados mediante **Precision@5**.

## Estructura recomendada del repositorio

```text
.
|-- app.R
|-- entropy_2025.sqlite
|-- search_helpers.R
|-- build_search_index.R
|-- search_index.rds
|-- evaluate_search.R
|-- score_precision.R
|-- consultas_evaluacion.csv
|-- precision_at_5.csv
|-- taller_4.Rmd
|-- taller_4.html
|-- dependencies.R
`-- README.md
```

## Descripcion de archivos

| Archivo | Descripcion |
|---|---|
| `app.R` | Aplicacion Shiny principal. Conserva el dashboard del Taller 2 e incorpora el buscador del Taller 4. |
| `entropy_2025.sqlite` | Base SQLite con la tabla `papers`. |
| `search_helpers.R` | Funciones de normalizacion, tokenizacion, TF-IDF, LSA/SVD, similitud coseno y ranking. |
| `build_search_index.R` | Script para construir `search_index.rds` de forma reproducible. |
| `search_index.rds` | Indice cacheado usado por Shiny para no recalcular matrices en cada consulta. |
| `evaluate_search.R` | Ejecuta las cinco consultas de evaluacion y genera resultados top 5. |
| `score_precision.R` | Marca relevancia 1/0 y calcula Precision@5. |
| `consultas_evaluacion.csv` | Resultados evaluados por consulta, estrategia y posicion. |
| `precision_at_5.csv` | Resumen de Precision@5 por consulta y estrategia. |
| `taller_4.Rmd` | Documento reproducible del Taller 4. |
| `taller_4.html` | Version renderizada del documento reproducible. |
| `dependencies.R` | Script para instalar paquetes requeridos en R. |

## Base de datos

La tabla principal es `papers`.

| Campo | Descripcion |
|---|---|
| `paper_id` | Identificador interno. |
| `journal_name` | Nombre de la revista. |
| `article_number` | Numero del articulo en MDPI. |
| `title` | Titulo del articulo. |
| `publication_date` | Fecha de publicacion. |
| `year` | Ano de publicacion. |
| `doi` | Enlace DOI. |
| `url` | URL del articulo. |
| `abstract` | Resumen y, cuando esta disponible, palabras clave. |
| `authors_raw` | Autores separados por punto y coma. |
| `n_authors` | Numero de autores. |
| `citations` | Citas reportadas. |
| `downloads` | Descargas reportadas. |
| `views` | Visualizaciones reportadas. |
| `n_references` | Numero de referencias. |
| `topic_label` | Categoria tematica asignada. |

## Requisitos

Version recomendada:

- R 4.4.0 o superior.
- RStudio para ejecutar, revisar y renderizar el documento reproducible.
- Conexion a internet si se usa el modulo de scraping o se despliega en shinyapps.io.

Paquetes de R:

```r
install.packages(c(
  "shiny",
  "DBI",
  "RSQLite",
  "dplyr",
  "stringr",
  "highcharter",
  "DT",
  "httr2",
  "rvest",
  "jsonlite",
  "tidyr",
  "htmltools",
  "rmarkdown"
))
```

## Ejecucion local

1. Clonar o descargar el repositorio.

2. Abrir la carpeta que contiene `app.R`.

3. Construir o actualizar el indice de busqueda:

```r
source("build_search_index.R")
```

4. Ejecutar la aplicacion:

```r
shiny::runApp()
```

5. Abrir la URL local generada por Shiny, por ejemplo:

```text
http://127.0.0.1:xxxx
```

## Reproduccion del Taller 4

Para reconstruir todo el modulo de recuperacion:

```r
source("build_search_index.R")
source("evaluate_search.R")
source("score_precision.R")
rmarkdown::render("taller_4.Rmd", encoding = "UTF-8")
```

El documento `taller_4.Rmd` describe:

- construccion del corpus;
- procesamiento de texto;
- representacion TF-IDF;
- reduccion dimensional con SVD/LSA;
- mecanismo de ranking;
- comparacion entre estrategias;
- evaluacion con cinco consultas;
- Precision@5.

## Buscador cientifico

El modulo de busqueda trabaja sobre titulo, resumen, palabras clave embebidas en el resumen y categoria. Permite seleccionar:

- **TF-IDF + coseno:** recuperacion lexica basada en coincidencia ponderada de terminos.
- **LSA/SVD + coseno:** recuperacion en espacio reducido de 80 dimensiones.
- **Comparar ambas:** muestra los rankings de las dos estrategias para la misma consulta.

La aplicacion carga `search_index.rds` desde cache. Si la base SQLite cambia, el indice se reconstruye automaticamente al iniciar una busqueda.

## Despliegue en shinyapps.io

La aplicacion ya se encuentra publicada en:

[https://elianstevenbejarano.shinyapps.io/files/](https://elianstevenbejarano.shinyapps.io/files/)

Para desplegar nuevamente:

```r
install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(
  name = "TU_USUARIO",
  token = "TU_TOKEN",
  secret = "TU_SECRET"
)

rsconnect::deployApp()
```

No se deben subir tokens, secretos ni archivos con credenciales al repositorio.

## Archivos que no se deben subir

```text
.RData
.Rhistory
rsconnect/
*.log
run_app_*.R
app.before_taller4.R
```

## Autor

Proyecto desarrollado para la asignatura **Mineria de Datos (2016325)**.

Autor: **Elian Steven Bejarano**
