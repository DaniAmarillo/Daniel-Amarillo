# Taller 4 — Minería de Datos

## Búsqueda y recuperación de artículos científicos con TF-IDF y LSA

**Autor:** Cristian Harvey Ardila Bolívar  
**Correo:** chardilab@unal.edu.co  
**Asignatura:** Minería de Datos — 2016325  
**Colección:** *Frontiers in Bioinformatics*

## Enlaces

- **Aplicación Shiny:** <https://clh6kc-cristian0harvey-ardila0bolivar.shinyapps.io/taller2-mineria-datos/>
- **Revista Frontiers in Bioinformatics:** <https://www.frontiersin.org/journals/bioinformatics>

Este proyecto en una continuación de la aplicación Shiny desarrollada en el Taller 2. Además de consultar, filtrar, visualizar y actualizar los artículos almacenados en SQLite, la aplicación incorpora un buscador por relevancia que compara dos estrategias de recuperación de información:

- TF-IDF con similitud coseno.
- LSA mediante SVD truncada y similitud coseno.

La colección se construye con el título, el resumen y las palabras clave de cada artículo. Las consultas escritas por el usuario reciben el mismo procesamiento aplicado al corpus y los resultados se presentan ordenados de mayor a menor similitud.

## Funcionalidades principales

La aplicación conserva las funciones desarrolladas en el Taller 2:

- consulta de artículos desde SQLite;
- filtros por fecha, tema, autor, DOI y contenido textual;
- indicadores descriptivos;
- gráficos interactivos;
- tabla interactiva de artículos;
- actualización de la colección mediante scraping.

El Taller 4 añade:

- campo de consulta en lenguaje natural;
- selección entre TF-IDF, LSA o comparación de ambas estrategias;
- rankings de 5, 10 o 20 artículos;
- posición, título, autores, fecha, tema, DOI o enlace, similitud y fragmento;
- representaciones precalculadas para responder sin reconstruir el corpus en cada búsqueda;
- actualización de TF-IDF y LSA cuando el scraping incorpora artículos nuevos;
- evaluación de cinco consultas mediante Precision@5.

Los puntajes corresponden a medidas de similitud. No representan probabilidades y no deben compararse directamente entre TF-IDF y LSA ya que no son cantidades comparables.

## Estrategias de recuperación

### TF-IDF y similitud coseno

Cada artículo se representa mediante los términos de su título, resumen y palabras clave. El procesamiento incluye normalización de texto, tokenización, retiro de palabras vacías y filtrado del vocabulario.

TF-IDF asigna más peso a los términos que ayudan a distinguir un artículo dentro de la colección. La consulta se transforma con el mismo vocabulario y se compara con todos los documentos mediante similitud coseno.

Esta estrategia suele responder bien cuando la consulta contiene vocabulario técnico y explícito, aunque puede ser muy literal cuando los artículos relacionados utilizan expresiones diferentes.

### LSA mediante SVD truncada

LSA reduce la representación TF-IDF mediante una descomposición SVD. En la ejecución documentada, la dimensión pasó de **4.282 términos** a **150 componentes**.

La consulta primero se representa con TF-IDF y después se proyecta al mismo espacio reducido de los artículos. La similitud coseno se calcula en ese espacio.

Esta estrategia puede recuperar asociaciones que no dependen únicamente de coincidencias literales, pero también puede acercar artículos relacionados de manera demasiado general.

## Colección utilizada

La aplicación se conecta a:

```text
revista_q1_2025.sqlite
```

La tabla principal es:

```text
papers
```

En la ejecución documentada se utilizaron **360 artículos**. Este número puede aumentar después de una actualización por scraping.

Los campos textuales principales son:

- `title`;
- `abstract`;
- `keywords`.

También se conservan metadatos como autores, fecha de publicación, tema, DOI, URL, citas, visualizaciones y referencias.

## Estructura del proyecto

```text
Taller_4_Cristian_Harvey_Ardila_Bolivar/
├── app.R
├── taller_4.Rmd
├── README.md
├── revista_q1_2025.sqlite
├── renv.lock
│
├── R/
│   ├── scraping_frontiers.R
│   ├── preparar_indices_busqueda.R
│   └── medir_rendimiento_busqueda.R
│
├── modelos/
│   ├── modelo_busqueda_taller4.rds
│   ├── funciones_busqueda_taller4.rds
│   └── resultados_rendimiento_busqueda.rds
│
└── evaluacion/
    ├── consultas_evaluacion.csv
    ├── juicios_relevancia.csv
    └── metricas_rankings.csv
```

### Descripción de los archivos

- `app.R`: aplicación Shiny con las funciones heredadas y el buscador por relevancia.
- `taller_4.Rmd`: documento reproducible con procesamiento, representaciones, reducción dimensional, rankings, evaluación, tiempos, memoria y discusión.
- `revista_q1_2025.sqlite`: base SQLite de artículos.
- `R/scraping_frontiers.R`: extracción y clasificación de artículos recientes.
- `R/preparar_indices_busqueda.R`: construcción de TF-IDF, LSA y funciones de búsqueda.
- `R/medir_rendimiento_busqueda.R`: medición de dimensiones, memoria y tiempos.
- `modelos/modelo_busqueda_taller4.rds`: matrices, vocabulario, metadatos y parámetros.
- `modelos/funciones_busqueda_taller4.rds`: funciones utilizadas por la aplicación.
- `modelos/resultados_rendimiento_busqueda.rds`: resultados computacionales presentados en el documento.
- `evaluacion/consultas_evaluacion.csv`: cinco consultas seleccionadas.
- `evaluacion/juicios_relevancia.csv`: revisión de los 50 resultados.
- `evaluacion/metricas_rankings.csv`: Precision@5 por consulta y estrategia.
- `renv.lock`: versiones de las dependencias de R.

`Main Taller 4.R` no es necesario para ejecutar ni reproducir la entrega.

## Dependencias

Los paquetes principales son:

- `shiny`
- `shinydashboard`
- `DBI`
- `RSQLite`
- `dplyr`
- `stringr`
- `tibble`
- `readr`
- `purrr`
- `tidyr`
- `lubridate`
- `tidytext`
- `tm`
- `proxy`
- `DT`
- `highcharter`
- `rvest`
- `xml2`
- `httr`
- `janitor`
- `ggplot2`
- `scales`
- `knitr`
- `rmarkdown`

Para el despliegue se utiliza además `rsconnect`.

Las versiones del proyecto pueden restaurarse con `renv.lock` mediante:

```r
install.packages("renv")
renv::restore()
```

Como alternativa, los paquetes pueden instalarse desde CRAN:

```r
install.packages(c(
  "shiny", "shinydashboard", "DBI", "RSQLite",
  "dplyr", "stringr", "tibble", "readr",
  "purrr", "tidyr", "lubridate", "tidytext",
  "tm", "proxy", "DT", "highcharter",
  "rvest", "xml2", "httr", "janitor",
  "ggplot2", "scales", "knitr", "rmarkdown"
))
```

## Ejecución local

Abra RStudio en la carpeta principal del Taller 4. Las rutas utilizadas por el proyecto son relativas, por lo que `app.R`, SQLite, `R/`, `modelos/` y `evaluacion/` deben conservar la estructura indicada.

Para iniciar la aplicación:

```r
shiny::runApp()
```

La aplicación carga los objetos guardados en `modelos/` al iniciar. Una búsqueda normal no reconstruye TF-IDF ni LSA.

## Reproducción del documento

Desde la carpeta principal:

```r
rmarkdown::render("taller_4.Rmd")
```

El documento utiliza la misma base, las mismas funciones, la misma dimensión LSA y las mismas reglas de orden que la aplicación.

## Reconstrucción de los objetos de búsqueda

Los archivos de `modelos/` ya se incluyen para que la aplicación pueda abrir sin preparar nuevamente el corpus.

La reconstrucción solo es necesaria cuando:

- faltan los RDS;
- cambia el contenido textual de la tabla `papers`;
- se incorporan artículos nuevos mediante scraping;
- se modifica el procesamiento o la dimensión LSA.

Para reconstruir:

```r
source("R/preparar_indices_busqueda.R")

preparar_indices_busqueda(
  ruta_sqlite = "revista_q1_2025.sqlite",
  tabla_principal = "papers",
  archivo_modelo = file.path(
    "modelos",
    "modelo_busqueda_taller4.rds"
  ),
  archivo_funciones = file.path(
    "modelos",
    "funciones_busqueda_taller4.rds"
  ),
  min_documentos_termino = 2L,
  max_proporcion_documentos = 0.90,
  k_lsa = 150L
)
```

El guardado se realiza primero en archivos temporales. Los RDS vigentes solo se reemplazan después de comprobar que pueden cargarse y que contienen las dos funciones de búsqueda.

## Actualización mediante scraping

La aplicación conserva el botón para buscar artículos recientes.

- Cuando no se encuentran artículos nuevos, SQLite y los índices permanecen sin cambios.
- Cuando se insertan artículos nuevos, TF-IDF y LSA se reconstruyen una sola vez.
- Una consulta normal nunca ejecuta scraping ni vuelve a calcular las representaciones.

La comparación de artículos nuevos utiliza DOI y, cuando este falta, URL.

## Evaluación de los rankings

Se utilizaron cinco consultas:

| ID | Tipo | Consulta |
|---|---|---|
| E1 | Términos literales | `single-cell RNA sequencing` |
| E2 | Términos relacionados | `artificial intelligence tumor biomarkers` |
| E3 | Consulta general | `bioinformatics data analysis` |
| E4 | Consulta específica | `microbiome 16S sequencing` |
| E5 | Consulta difícil | `data methods results` |

Cada consulta produjo cinco resultados con TF-IDF y cinco con LSA. Los 50 resultados fueron revisados con una etiqueta binaria:

- `1`: relevante;
- `0`: no relevante o relación insuficiente.

### Precision@5

| Consulta | TF-IDF | LSA |
|---|---:|---:|
| E1 | 0,80 | 1,00 |
| E2 | 0,40 | 0,00 |
| E3 | 1,00 | 1,00 |
| E4 | 0,60 | 0,60 |
| E5 | 1,00 | 1,00 |
| **Promedio** | **0,76** | **0,72** |

La diferencia promedio fue pequeña. TF-IDF funcionó mejor en la consulta compuesta sobre inteligencia artificial, tumores y biomarcadores; LSA funcionó mejor en la consulta de secuenciación de RNA de célula única. En las tres consultas restantes hubo empate.

Los archivos completos se encuentran en `evaluacion/`.

## Consideraciones computacionales

En la ejecución documentada:

- TF-IDF representó 360 artículos con 4.282 términos;
- LSA representó los mismos artículos con 150 componentes;
- la reducción de columnas fue aproximadamente 96,5 %;
- el consumo total de los objetos cargados se mantuvo muy por debajo de 16 GB;
- las consultas utilizaron los objetos ya cargados;
- los RDS no cambiaron durante una búsqueda normal.

Los valores detallados de memoria y tiempo se encuentran en el Rmd y en:

```text
modelos/resultados_rendimiento_busqueda.rds
```

## Despliegue en shinyapps.io

La versión actualizada se encuentra disponible en:

<https://clh6kc-cristian0harvey-ardila0bolivar.shinyapps.io/taller2-mineria-datos/>

Datos del despliegue:

- cuenta: `clh6kc-cristian0harvey-ardila0bolivar`;
- nombre de la aplicación: `taller2-mineria-datos`;
- identificador de la aplicación: `17452823`.

## Seguridad y portabilidad

- El proyecto utiliza rutas relativas.
- No requiere servicios de pago.
- No deben incluirse contraseñas, tokens ni secretos de `shinyapps.io`.
- Las credenciales de despliegue deben permanecer fuera del repositorio.
- No se deben subir archivos `.Rhistory`, respaldos temporales, bibliotecas HTML ni carpetas locales de paquetes.
