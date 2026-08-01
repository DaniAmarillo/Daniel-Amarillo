# Taller 2 — Minería de Datos · PLOS ONE 

**Asignatura:** Minería de Datos 


Aplicación Shiny en R para explorar, visualizar y actualizar la base de datos de artículos científicos de PLOS ONE construida en el Taller 1. Funciona como un dashboard analítico orientado al proceso KDD, integrando adquisición, almacenamiento, consulta y visualización de datos.

##  Aplicación en linea
Se puede consultar en el siguiente link:

[https://33ayg6-keiner0felipe-correa0leguizamon.shinyapps.io/shiny_app_-md/](https://33ayg6-keiner0felipe-correa0leguizamon.shinyapps.io/shiny_app_-md/)

---

##  Estructura del proyecto

```
├── global.R                  # Conexión a SQLite, scraping y funciones compartidas
├── ui.R                      # Interfaz de usuario y estilos
├── server.R                  # Lógica reactiva y visualizaciones
└── revista_plosone.sqlite    # Base de datos generada en el Taller 1
```

---

##  Funcionalidades

### Filtros interactivos (sidebar)
El panel lateral permite filtrar los artículos por:

- Rango de fechas
- Temática (Machine Learning, IA Generativa, Estadística, Otros)
- Autor
- DOI
- Palabra clave en título o abstract

El botón **🗑 limpiar** restablece todos los filtros a sus valores por defecto de forma instantánea.

---

### 📊 Pestaña: Indicadores

Muestra un subtítulo dinámico con la categoría activa según el filtro de temática seleccionado, para que siempre sea claro sobre qué subconjunto de datos se calculan las métricas.

Incluye 6 tarjetas KPI calculadas en tiempo real:

| Indicador | Descripción |
|---|---|
| Total artículos | Cantidad de artículos según filtros activos |
| Promedio autores | Media de autores por artículo |
| Promedio de citas | Media de citas por artículo |
| Promedio de referencias | Media de referencias por artículo |
| Más citado | Artículo con mayor número de citas |
| Más descargado | Artículo con mayor número de vistas |

#### ️ Redireccionamiento desde KPI al artículo
Las tarjetas Más citado y Más descargado tienen una funcionalidad extra, Al hacer clic:

1. La aplicación cambia automáticamente a la pestaña **📋 Artículos**
2. Se rellena el campo DOI del sidebar con el identificador del artículo
3. La tabla se filtra mostrando únicamente ese artículo
4. Aparece una notificación en pantalla recordando que se puede usar **🗑 limpiar** para volver a la vista completa

Esto permite ir del indicador directamente al artículo concreto sin búsqueda manual.

---

### 📈 Visualizaciones interactivas (Highcharter)

| Gráfico | Descripción |
|---|---|
| Publicaciones por mes | Línea de tendencia (spline) con la evolución mensual de publicaciones |
| Artículos por temática | Barras horizontales por categoría clasificada |
| Top 10 autores | Autores con mayor participación en artículos del conjunto filtrado |
| Distribución de citas | Columnas agrupadas por rangos de citas (0-5, 6-10, 11-25, 26-50, 51-100, >100) |

Todos los gráficos responden a los filtros activos del sidebar.

---

### 📋 Pestaña: Artículos

Tabla dinámica con los artículos filtrados que incluye:
- Título, Autores, Fecha, Tema, DOI, Citas, Descargas

Permite búsqueda interna, paginación y ordenamiento por columna.

---

### 🆕 Pestaña: Nuevos artículos — Actualización automática por scraping

La aplicación incluye un botón 🔍 Buscar artículos nuevos que ejecuta el siguiente proceso:

1. Consulta la BD y extrae todos los DOIs ya almacenados
2. Scrapea las primeras 120 páginas de la pagina de PLOS ONE (Computer & Information Sciences) con saltos de 20 en 20 para cubrir el rango reciente sin sobrecargar el servidor
3. Filtra duplicados comparando los DOIs encontrados contra los ya existentes
4. Si hay artículos nuevos: los scrapea individualmente combinando tres fuentes:
   - API de PLOS → título, fecha, autores, abstract, vistas
   - HTML de la página → referencias
   - API de OpenAlex → número de citas
5. Los inserta en la tabla `papers` de SQLite con IDs continuos
6. Si no hay artículos nuevos: muestra los últimos 5 DOIs almacenados para verificación, tal como lo requiere el enunciado

La aplicación informa en todo momento cuántos artículos nuevos fueron encontrados y almacenados.

---

##  Librerías requeridas

```r
install.packages(c(
  "shiny", "DBI", "RSQLite", "dplyr", "tidyr", "stringr",
  "lubridate", "highcharter", "DT", "httr", "rvest", "purrr", "tibble"
))
```

---

##  Correr localmente

```r
# Desde la carpeta del proyecto
shiny::runApp()
```

Gracias :D
