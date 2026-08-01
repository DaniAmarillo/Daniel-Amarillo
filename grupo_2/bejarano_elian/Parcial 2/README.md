# Entropy Dashboard

Dashboard interactivo desarrollado en **R Shiny** para explorar, filtrar y actualizar articulos cientificos de la revista **Entropy (MDPI)**, como parte del Taller 2 de Mineria de Datos.

Aplicacion desplegada: [https://elianstevenbejarano.shinyapps.io/files/](https://elianstevenbejarano.shinyapps.io/files/)

## Descripcion general

Este proyecto presenta una aplicacion analitica conectada a una base de datos SQLite construida a partir de web scraping de la revista **Entropy**. El dashboard permite consultar articulos, revisar metricas bibliometricas, analizar categorias tematicas y ejecutar procesos de actualizacion para nuevos articulos publicados en MDPI.

La aplicacion integra:

- Conexion directa a una base de datos local en SQLite.
- Filtros reactivos por fecha, tematica, titulo, autor, DOI y metricas minimas.
- Indicadores principales sobre articulos, autores, citas, referencias y descargas.
- Graficos interactivos con `highcharter`.
- Tablas interactivas con `DT`.
- Vista expandida del abstract de cada articulo.
- Modulo de actualizacion por web scraping para consultar publicaciones recientes.
- Verificacion de calidad de datos y registros con valores faltantes.

## Enlace de la aplicacion desplegada

La aplicacion se encuentra publicada en shinyapps.io:

[https://elianstevenbejarano.shinyapps.io/files/](https://elianstevenbejarano.shinyapps.io/files/)

Este enlace permite abrir el dashboard directamente desde el navegador sin instalar R ni ejecutar archivos locales.

## Estructura del repositorio

```text
.
|-- app.R
|-- entropy_2025.sqlite
`-- README.md
```

### `app.R`

Archivo principal de la aplicacion Shiny. Contiene:

- Carga de librerias.
- Funciones de conexion a SQLite.
- Funciones auxiliares de web scraping.
- Definicion de la interfaz de usuario.
- Logica del servidor.
- Filtros, indicadores, graficos, tablas y modulo de actualizacion.

### `entropy_2025.sqlite`

Base de datos SQLite utilizada por la aplicacion. Contiene la tabla `papers`, donde se almacenan los articulos recolectados de Entropy.

Campos principales:

| Campo | Descripcion |
|---|---|
| `paper_id` | Identificador interno del articulo |
| `journal_name` | Nombre de la revista |
| `article_number` | Numero del articulo en MDPI |
| `title` | Titulo del articulo |
| `publication_date` | Fecha de publicacion |
| `year` | Ano de publicacion |
| `doi` | Enlace DOI |
| `url` | URL del articulo en MDPI |
| `abstract` | Resumen del articulo |
| `authors_raw` | Autores del articulo |
| `n_authors` | Numero de autores |
| `citations` | Numero de citas reportadas |
| `downloads` | Numero de descargas |
| `views` | Numero de visualizaciones |
| `n_references` | Numero de referencias |
| `topic_label` | Clasificacion tematica del articulo |

## Requisitos

Para ejecutar el proyecto localmente se requiere:

- R 4.4.0 o superior.
- RStudio, Positron o una consola de R.
- Conexion a internet si se desea usar el modulo de web scraping.

Paquetes de R requeridos:

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
  "tidyr"
))
```

## Ejecucion local

1. Clonar el repositorio:

```bash
git clone <URL_DEL_REPOSITORIO>
```

2. Abrir la carpeta del proyecto.

3. Verificar que `app.R` y `entropy_2025.sqlite` esten en el mismo directorio.

4. Ejecutar la aplicacion desde R:

```r
shiny::runApp()
```

Tambien puede ejecutarse indicando la ruta del proyecto:

```r
shiny::runApp("ruta/a/la/carpeta")
```

5. Abrir la URL local generada por Shiny, normalmente similar a:

```text
http://127.0.0.1:xxxx
```

## Funcionalidades del dashboard

### 1. Filtros interactivos

La barra lateral permite filtrar los articulos por:

- Rango de fechas.
- Tematica.
- Titulo.
- Autor.
- DOI.
- Citas minimas.
- Descargas minimas.
- Registros con metricas completas.

Los filtros modifican automaticamente los indicadores, graficos y tablas del dashboard.

### 2. Indicadores principales

El dashboard resume la seleccion actual mediante indicadores como:

- Total de articulos.
- Promedio de autores por articulo.
- Promedio de citas.
- Promedio de referencias.
- Promedio de descargas.

### 3. Visualizaciones interactivas

La aplicacion incluye graficos para analizar:

- Evolucion temporal de publicaciones.
- Distribucion de articulos por tematica.
- Distribucion de citas.
- Autores mas frecuentes.
- Relacion entre visualizaciones y descargas.
- Promedios bibliometricos por tematica.

### 4. Tablas interactivas

Las tablas permiten buscar, ordenar y explorar los articulos. Al seleccionar un registro se despliega un panel con el abstract y metadatos relevantes.

### 5. Actualizacion por web scraping

El modulo de actualizacion consulta paginas de MDPI para identificar articulos recientes y actualizar metricas bibliometricas. Este proceso depende de la disponibilidad de MDPI y de posibles restricciones temporales del sitio.

## Despliegue en shinyapps.io

Para publicar la aplicacion en shinyapps.io:

1. Crear una cuenta en [https://www.shinyapps.io/](https://www.shinyapps.io/).

2. Instalar y configurar `rsconnect`:

```r
install.packages("rsconnect")
library(rsconnect)
```

3. Configurar la cuenta con los datos suministrados por shinyapps.io:

```r
rsconnect::setAccountInfo(
  name = "TU_USUARIO",
  token = "TU_TOKEN",
  secret = "TU_SECRET"
)
```

4. Desde la carpeta donde estan `app.R` y `entropy_2025.sqlite`, ejecutar:

```r
rsconnect::deployApp()
```

5. Copiar el enlace generado por shinyapps.io y verificar que la aplicacion funcione correctamente desde el navegador.

6. Incluir el enlace de la aplicacion desplegada en este README y en la entrega del taller.

Enlace actual:

[https://elianstevenbejarano.shinyapps.io/files/](https://elianstevenbejarano.shinyapps.io/files/)

## Recomendaciones para subir a GitHub

Antes de subir el proyecto al repositorio, se recomienda incluir solo los archivos necesarios:

```text
app.R
entropy_2025.sqlite
README.md
```

No es necesario subir archivos temporales como:

```text
.RData
.Rhistory
rsconnect/
*.log
run_app_*.R
```

Ejemplo de comandos para subir a GitHub:

```bash
git init
git add app.R entropy_2025.sqlite README.md
git commit -m "Add Entropy Shiny dashboard"
git branch -M main
git remote add origin <URL_DEL_REPOSITORIO>
git push -u origin main
```

## Notas metodologicas

La construccion del dashboard sigue un flujo de trabajo tipo KDD:

1. **Adquisicion:** recopilacion de informacion desde MDPI mediante web scraping.
2. **Almacenamiento:** organizacion de los articulos en SQLite.
3. **Preparacion:** limpieza de fechas, autores, metricas y categorias.
4. **Consulta:** filtros reactivos y exploracion tabular.
5. **Mineria:** analisis de tendencias, impacto y distribuciones.
6. **Actualizacion:** consulta de nuevos articulos o actualizacion de metricas.

## Autor

Proyecto desarrollado para el curso de **Mineria de Datos**.

Autor: **Elian Steven Bejarano**

Repositorio destinado a la entrega academica del Taller 2.
