# Taller Parcial 2 - Dashboard JAIR (Shiny)

**Autor:** Mateo Venegas Clavijo - CC. 1075878496  
**Curso:** Minería de Datos - Universidad Nacional de Colombia, 2025

## Descripción

Tablero interactivo desarrollado en **R Shiny** que permite explorar, analizar y **actualizar en tiempo real** una base de datos de artículos científicos extraídos del [Journal of Artificial Intelligence Research (JAIR)](https://www.jair.org/index.php/jair).

La base de datos fue construida originalmente en el Taller Parcial 1 mediante Web Scraping de los volúmenes 82, 83 y 84 (año 2025). Este tablero permite incorporar nuevos volúmenes directamente desde la interfaz.

## Estructura de Archivos

```
Taller_Parcial_2_(Shiny_Streamlit)/
├── app.R                  # Aplicación Shiny principal (UI + Server)
├── scraper_engine.R       # Motor de Web Scraping (módulo auxiliar)
├── README.md              # Este archivo
└── p2_md.pdf              # Instrucciones del taller
```

## Requisitos

### Paquetes de R

```r
install.packages(c(
  "shiny", "bslib", "DBI", "RSQLite",
  "ggplot2", "dplyr", "tidyr", "DT",
  "rvest", "httr", "jsonlite", "stringr", "plotly"
))
```

### Base de Datos

La aplicación requiere acceso a la base de datos SQLite ubicada en:

```
../Taller_Parcial_1_(WS_SQL)/revista_q1_2025.sqlite
```

Esta ruta es relativa al directorio de la app. Asegúrese de que la estructura de carpetas del proyecto se mantenga intacta.

## App Desplegada

La aplicación está publicada en shinyapps.io:

**https://mateovecla12.shinyapps.io/JAIR-Dashboard/**

## Ejecución Local

Desde RStudio, abra el archivo `app.R` y presione **Run App**, o ejecute:

```r
shiny::runApp("Taller_Parcial_2_(Shiny_Streamlit)")
```

## Funcionalidades del Tablero

| Pestaña | Descripción |
|---------|-------------|
| **Resumen General** | Value boxes con métricas clave, distribución por tema, histograma de citas, top 10 artículos más citados. Filtros por tema y rango de citas. |
| **Autores** | Ranking de autores más prolíficos, tabla de coautorías, tabla completa con filtros. |
| **Explorador** | Búsqueda de artículos por título, tema y citas mínimas. |
| **Actualizar Datos** | Selección de volúmenes JAIR para incorporar vía Web Scraping. Log en tiempo real del proceso. |
| **Acerca de** | Información del proyecto, tecnologías y fuente de datos. |

## Fuente de Datos

- **Revista:** Journal of Artificial Intelligence Research (JAIR) - Q1
- **API de enriquecimiento:** [OpenAlex](https://openalex.org/) (citas y referencias)
- **Volúmenes iniciales:** 82, 83, 84 (2025)

## Notas Técnicas

- El motor de scraping detecta automáticamente artículos ya existentes en la DB (por URL) para evitar duplicados.
- La clasificación temática se realiza por palabras clave en el título del artículo.
- Los tiempos de espera (`Sys.sleep`) entre requests respetan las políticas de uso de JAIR y OpenAlex.
