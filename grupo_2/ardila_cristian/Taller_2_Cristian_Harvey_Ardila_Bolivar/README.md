# Taller 2 - Minería de Datos

Aplicación realizada en Shiny para consultar, visualizar y actualizar la base SQLite que fue realizada en el Taller 1.

La aplicación puede explorar artículos académicos almacenados en la base de datos `revista_q1_2025.sqlite`, aplicar filtros, visualizar indicadores descriptivos, mostrar gráficos interactivos y ejecutar una actualización haciendo un scraping de artículos recientes.

## Base de datos

La aplicación se conecta a la base SQLite del creada en el Taller 1:

-   `revista_q1_2025.sqlite`

La tabla principal usada por la aplicación corresponde a:

-   `papers`

Esta tabla contiene información como título, autores, fecha de publicación, DOI, URL, resumen, citas, descargas, número de referencias y clasificación temática.

## Funcionalidad de la aplicación

-   Consulta artículos desde SQLite.
-   Filtra por rango de fechas.
-   Filtra por tema o categoría.
-   Realiza búsqueda por autor.
-   Realiza búsqueda por DOI.
-   Realiza búsqueda por título, resumen o palabras clave.
-   Muestra indicadores descriptivos.
-   Realiza gráficos interactivos.
-   Genera una tabla interactiva de artículos filtrados.
-   Muestra de botón para buscar artículos nuevos mediante scraping.
-   Efectúa la revisión de los últimos cinco artículos cuando no se detectan registros nuevos.

## Bibliotecas usadas

La aplicación usa las siguientes bibliotecas de R:

-   `shiny`
-   `shinydashboard`
-   `DBI`
-   `RSQLite`
-   `dplyr`
-   `stringr`
-   `tibble`
-   `readr`
-   `DT`
-   `highcharter`
-   `lubridate`
-   `rvest`
-   `xml2`
-   `httr`
-   `purrr`
-   `tidyr`
-   `janitor`
-   `pacman`

## Archivos generados

-   `app.R`: Archivo principal de la aplicación Shiny.
-   `revista_q1_2025.sqlite`: Base de datos SQLite construida en el Taller 1 y actualizada en el presente taller.
-   `R/scraping_frontiers.R`: funciones de scraping recuperadas y adaptadas desde el Taller 1.
-   `README.md`: instrucciones generales del proyecto.

## Ejecución local

Para ejecutar la aplicación localmente, abrir RStudio en la carpeta del proyecto y correr:

``` r
shiny::runApp()
```

o ejecutar completamente el script `app.R` en RStudio.

La base `revista_q1_2025.sqlite` debe estar ubicada en la misma carpeta que `app.R`.

## Actualización mediante scraping

La aplicación incluye un botón llamado **Buscar artículos nuevos**. Al presionarlo, la app ejecuta el scraping de artículos recientes, compara los resultados obtenidos contra el archivo SQLite usando la variable DOI o URL, guarda los artículos nuevos y recarga la tabla principal.

En caso de no encontrar artículos nuevos, la aplicación verifica los últimos cinco artículos almacenados y muestra el resultado de dicha revisión.

## Enlace de despliegue

<https://clh6kc-cristian0harvey-ardila0bolivar.shinyapps.io/taller2-mineria-datos/>

**NOTA:** La base de datos se encuentra sin actualizar, aunque localmente ya se verificó que la actualización funciona adecuadamente. Por favor ejecutar la actualización de la base de datos mediante scraping usando el botón diseñado para ese propósito al momento de revisar el trabajo.
