# Taller 4 — Buscador de artículos científicos (PLOS ONE)

Minería de Datos (2016325) — Universidad Nacional de Colombia

Aplicación Shiny que amplía el dashboard del Taller 2 con un módulo de
búsqueda y recuperación de artículos científicos en lenguaje natural utilizando
estrategias de recuperación comparables.

## Enlace de la aplicación desplegada

https://33ayg6-keiner0felipe-correa0leguizamon.shinyapps.io/parcial_4_-app/

## Estructura de archivos

```
├── ui.R + server.R)   # Aplicación Shiny
├── global.R                    # Librerías, conexión SQLite, scraping, filtros
├── retrieval_functions.R       # Funciones de búsqueda (carga índice, NO lo reconstruye)
├── build_search_index.R        # Construye el índice de recuperación (se corre 1 sola vez)
├── es_en_dict.R                # Diccionario básico de traducción español → inglés
├── revista_plosone.sqlite      # Base de datos de artículos (Taller 1)
├── index/                      # Objetos precalculados (.rds), generados por build_search_index.R
│   ├── meta.rds
│   ├── tfidf_mat.rds
│   ├── idf_vector.rds
│   ├── lsa_model.rds
│   ├── bm25_model.rds
│   └── stats.rds
├── taller_4.Rmd                # Documento reproducible (metodología + evaluación)
├── evaluacion_resultados.csv   
├── instalar_dependencias.R     # Instala los paquetes de R usados por el proyecto
└── README.md
```

## Instrucciones de ejecución

1. Instala las dependencias:
   ```r
   source("instalar_dependencias.R")
   ```
2. **Construye el índice de recuperación** (solo la primera vez, o si cambia
   `revista_plosone.sqlite`):
   ```r
   source("build_search_index.R")
   ```
   Esto genera los 6 archivos `.rds` dentro de `index/`. La app no
   arrancará correctamente sin ellos.
3. Corre la aplicación desde la raíz del proyecto:
   ```r
   shiny::runApp()
   ```
4. Para reproducir la metodología y la evaluación documentada, abre y
   compila (`Knit`) `taller_4.Rmd` desde la misma carpeta raíz.

## Notas

- El corpus de artículos está en inglés. El campo de búsqueda de la app
  acepta consultas en español: se traducen con un diccionario básico
  (`es_en_dict.R`) antes de vectorizarse — la traducción es intencionalmente
  simple (basada en diccionario, no en un modelo de traducción) y puede no
  cubrir todo el vocabulario.
