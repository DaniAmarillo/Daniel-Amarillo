# Springer Visual Miner — Taller 4

Sistema de recuperación de información sobre el corpus de la revista
*Artificial Intelligence Review* (Springer), integrado como módulo nuevo en la
aplicación Shiny del Taller 2.

**Aplicación desplegada:** ` https://qwbdau-wlucano.shinyapps.io/Springer-Visual/`

---

## Qué hace

Recibe una consulta en lenguaje natural (inglés o español) y devuelve artículos
ordenados por relevancia, con cuatro estrategias intercambiables:

| Estrategia | Tipo | Espacio |
|---|---|---|
| BM25 | Léxica | 5.711 dimensiones dispersas |
| TF-IDF + coseno | Léxica | 5.711 dimensiones dispersas |
| LSA (Truncated SVD) | Semántica | 200 componentes latentes |
| Híbrida (RRF) | Fusión de rangos | — |

Además ofrece dos modos de comparación lado a lado: *BM25 vs LSA* y
*Ablación TF-IDF vs LSA*, este último con el pesado y la similitud fijos para
aislar el efecto de la reducción dimensional.

---

## Estructura

### Aplicación

```
global.R                    Librerías, tema, normalización relacional,
                            scraping, tokenizador y las cuatro estrategias
ui.R                        Interfaz (7 páginas, incluida «Buscador»)
server.R                    Lógica reactiva
Springer_Visual_Miner.sqlite  Base de datos del corpus
search_index.rds            Índice precalculado que consume la app
```

### Pipeline offline (no se despliega)

```
pipeline.R                  Todo el proceso fuera de línea en un solo archivo:
                            dependencias, índice, vocabulario, evaluación y
                            pruebas de regresión
```

### Documento y evaluación

```
resultados.rds              Todas las tablas de resultados en un artefacto
juicios.csv                 Juicios de relevancia, editados manualmente
```

`pipeline.R` escribe además archivos CSV intermedios mientras corre. Son
inspeccionables pero no hacen falta para reproducir: `resultados.rds` los
contiene todos.

---

## Requisitos

R ≥ 4.1. La lista completa está en `dependencias.txt`. Para revisarlos e
instalarlos:

```r
source("pipeline.R")
instalar_dependencias()
```

`RSpectra` solo se necesita para construir el índice, no para ejecutar la
aplicación.

---

## Ejecución

### Solo usar la aplicación

El repositorio incluye `Springer_Visual_Miner.sqlite` y `search_index.rds` ya
construidos:

```r
shiny::runApp()
```

### Reproducir todo desde cero

```r
source("pipeline.R")

# 1. Corpus (~25 min: una petición HTTP por artículo, con pausas)
ejecutar_scraping(modo = "completo", desde = "2025-01-01", max_nuevos = 2000)
completar_keywords()
normalizar_tablas()

# 2. Todo lo demás: índice, vocabulario, pruebas, evaluación y empaquetado
ejecutar_todo()

# 3. Documento
rmarkdown::render("taller_4.Rmd")
```

`ejecutar_todo()` corre las etapas en orden. Si faltan juicios de relevancia se
detiene tras escribir `juicios_plantilla.csv` y lo indica; se llena la columna
`relevante`, se guarda como `juicios.csv` y se vuelve a ejecutar.

Las etapas también pueden correrse por separado: `construir_indice()`,
`analizar_vocabulario()`, `evaluar()`, `probar_corrector()`,
`empaquetar_resultados()`.

`ejecutar_scraping()` escribe en bloques de 25 artículos: si se interrumpe, una
sincronización incremental completa lo que falte sin repetir lo hecho.
`completar_keywords()` es resumible por la misma razón.

---

## Notas de arquitectura

**El tokenizador vive en `global.R` y `pipeline.R` hace `source("global.R")`.**
Así el procesamiento de indexación es idéntico al de consulta por construcción,
no por disciplina.

**La aplicación no reindexa.** Los pesos BM25 están precalculados celda a celda
y la matriz de proyección del SVD está guardada; una búsqueda es un producto
matriz-vector. Los tiempos medidos son de 15–40 ms por consulta.

**El índice se carga en `global.R`, no en `server()`,** para compartirlo entre
sesiones en lugar de duplicarlo por usuario.

**El scraping no usa navegador headless.** Las páginas de artículo de
SpringerLink son renderizadas en servidor, así que basta `httr2` + `rvest`
leyendo meta-etiquetas Highwire y Dublin Core, que son más estables que las
clases CSS.

---

## Limitaciones conocidas

- Modelo de bolsa de palabras: no hay tratamiento de la negación.
- El puente español→inglés es un léxico de dominio de ~80 términos que opera
  token por token, no un traductor.
- Los juicios de relevancia son de un único evaluador, sin medida de acuerdo
  entre anotadores.
- El pooling subestima el recall real, por lo que solo se reportan métricas de
  precisión en el tope del ranking.
- El campo `keywords` mezcla palabras clave del autor con la taxonomía editorial
  de Springer; `max_df` neutraliza su efecto sobre el ranking.

El detalle y la justificación de cada decisión están en `taller_4.Rmd`.

## Archivos entregados

| Archivo | Necesario para |
|---|---|
| `global.R`, `ui.R`, `server.R` | Ejecutar la aplicación |
| `Springer_Visual_Miner.sqlite` | Ejecutar la aplicación |
| `search_index.rds` | Ejecutar la aplicación |
| `pipeline.R` | Reproducir todo el proceso offline |
| `juicios.csv` | Reproducir la evaluación |
| `resultados.rds` | Tejer el documento |

Para **solo usar la aplicación** bastan los cinco primeros.

---
