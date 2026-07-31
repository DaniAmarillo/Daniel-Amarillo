# Taller 4 — Minería de Datos (2016325)
### Sistema de recuperación de información sobre *Nature Ecology & Evolution*

**Autor:** Sebastián Tabares-Segovia · setabaress@unal.edu.co

Amplía el dashboard del Taller 2 con un buscador de artículos científicos. El usuario
escribe una consulta en lenguaje natural y obtiene una lista de artículos ordenados por
relevancia, comparando dos estrategias de recuperación:

- **Léxica:** TF-IDF disperso + similitud coseno.
- **Semántica reducida:** LSA (Truncated SVD sobre el TF-IDF, k=200) + similitud coseno.

La consulta se proyecta al mismo espacio vectorial de los artículos antes de calcular la
similitud. El índice se **precalcula** y se carga una sola vez; la app no reprocesa el
corpus en cada búsqueda.

---

## Enlace de la aplicación desplegada

> **App en línea:** https://tabares-segovia.shinyapps.io/nature-eco-evo-dashboard/


---

## Estructura de archivos

| Archivo | Descripción |
|------------------------|--------------------------------------------------------|
| `app.R` | Aplicación Shiny (dashboard del Taller 2 + pestaña **Buscador**). |
| `build_index.R` | Precálculo del índice → genera `search_index.rds`. |
| `search_engine.R` | Funciones de recuperación y ranking (usadas por la app y el documento). |
| `search_index.rds` | Índice precalculado (TF-IDF + LSA + metadatos). *Se genera al ejecutar `build_index.R`.* |
| `taller_4.Rmd` | Documento reproducible (R Markdown → PDF) del proceso completo y la evaluación. |
| `evaluacion_consultas.csv` | Precision@5 por consulta y estrategia. |
| `evaluacion_detalle.csv` | Top-5 de cada consulta y estrategia con puntajes. |
| `nature_eco_evo_2025.sqlite` | Base de datos de artículos. |
| `install.R` | Instalación de dependencias. |
| `www/` | Logos (`logo_unal.png`, `logo_nature.png`). |

> La evaluación (5 consultas + Precision@5 + análisis de resultados) también se calcula
> dentro de `taller_4.Rmd` al renderizar; los CSV se incluyen como archivo de resultados.

---

## Cómo ejecutar

```bash
# 1. Instalar dependencias (una vez)
Rscript install.R

# 2. Construir el índice de búsqueda (una vez; ~1-2 s)
Rscript build_index.R          # crea search_index.rds

# 3. Ejecutar la aplicación
R -e "shiny::runApp('app.R', launch.browser = TRUE)"

# 4. (Opcional) Reproducir el documento
R -e "rmarkdown::render('taller_4.Rmd')"   # regenera tambien search_index.rds
```

> **Importante:** ejecute `build_index.R` **antes** de abrir la app. Si `search_index.rds`
> no existe, la pestaña Buscador mostrará un aviso indicándolo.

---

## Despliegue en shinyapps.io

```r
# una sola vez: instalar y configurar la cuenta
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<tu-usuario>",
                          token = "<token>",
                          secret = "<secret>")   # copiar desde shinyapps.io > Account > Tokens

# desplegar (incluye el .rds y la SQLite; genere el indice antes)
rsconnect::deployApp(
  appName  = "taller4-buscador",
  appFiles = c("app.R", "search_engine.R", "search_index.rds",
               "nature_eco_evo_2025.sqlite", "www")
)
```

Tras publicar, copie la URL resultante en la sección **«Enlace de la aplicación
desplegada»** de arriba.

---

## Uso del buscador

1. Abrir la pestaña **Buscador**.
2. Escribir una consulta (p. ej. *"generative AI for disease diagnosis"*).
3. Elegir estrategia: léxica, semántica o **comparar ambas**.
4. Elegir número de resultados (5 / 10 / 20) y pulsar **Buscar**.

Cada resultado muestra: posición en el ranking, título (con enlace), autores, fecha, tema,
DOI, puntaje de similitud coseno y un fragmento del texto.

---

## Notas metodológicas

- **Puntaje:** similitud coseno en [0, 1]. No es una probabilidad y **no** es comparable
  entre estrategias distintas (viven en espacios vectoriales diferentes).
- **Empates:** se rompen por número de citas y luego por fecha (criterios secundarios).
- **Corpus:** 373 artículos; solo ~20 % tienen resumen, por lo que la mayoría se representa
  por su título. No se inventa texto para los resúmenes ausentes.
- **Reducción:** Truncated SVD (LSA) truncada a k=200; como el corpus es pequeño, la SVD se
  calcula de forma exacta. Para un corpus grande se usaría un solver disperso (irlba/RSpectra).
- **Requisitos:** ejecuta con holgura en 16 GB de RAM (índice < 5 MB). Sin API de pago ni
  modelos neuronales grandes.
