# Taller 03 - Minería de Datos

## Nota para el profesor

Buenos días profesor,

Le pido disculpas por entregarle el trabajo una semana tarde. La semana pasada inicié mis prácticas profesionales y se me acumuló con el cierre de varias materias. Habría podido entregarle una versión preliminar antes, pero decidí tomarme el tiempo necesario para entregarle un trabajo de calidad. Quedo totalmente atento y dispuesto a aceptar la penalización correspondiente sobre la nota.

---

## Guía de Ejecución y Requisitos del Proyecto

### 1. Ubicación de la Base de Datos

Colocar la base de datos de origen en la siguiente ruta del sistema:

`C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03`

---

### 2. Configuración y Ejecución de Scripts

Establecer la ruta anterior como el directorio de trabajo (mediante `setwd()` o abriendo el proyecto de RStudio desde esa carpeta).

Ejecutar los scripts en orden estricto del `01` al `07`. Cada script depende de los archivos `.rds` generados por el script anterior dentro de la carpeta `output/`.

---

### 3. Paquetes de R Requeridos

Asegúrese de contar con los siguientes paquetes de R instalados antes de ejecutar el proyecto:

- `dplyr`
- `readr`
- `stringr`
- `lubridate`
- `tidyr`
- `ggplot2`
- `scales`
- `knitr`
- `purrr`
- `caret`
- `randomForest`
- `gbm`
- `pROC`
- `MLmetrics`

---

### 4. Compilación del Informe Final (`taller_3.Rmd`)

Una vez que existan todos los archivos `.rds` y `.png` en la carpeta `output/`, proceda a compilar el documento principal `taller_3.Rmd`.
