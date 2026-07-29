# Taller 4 — Sistema de recuperación de información sobre JMLR

**Minería de Datos (2016325) · Universidad Nacional de Colombia**
Juan José Gómez García

Amplía el dashboard Shiny del Taller 2 (base JMLR construida en el Taller 1) con
un **buscador de artículos científicos**: el usuario escribe una consulta en
lenguaje natural y obtiene artículos ordenados por relevancia.

**App desplegada:** https://juanjo24.shinyapps.io/taller2-jmlr/

---

## Estrategias implementadas

| | Estrategia | Tipo | Representación | Similitud | ¿Reducción? |
|---|---|---|---|---|---|
| **E1** | BM25 | léxica | DTM dispersa (4 479 términos) | puntaje BM25 (`k1=1.2`, `b=0.75`) | No |
| **E2** | LSA | semántica latente | TF-IDF + Truncated SVD | coseno en el espacio latente | **Sí, k = 120** |
| **E3** | Híbrida | fusión | rangos de E1 y E2 | Reciprocal Rank Fusion (`K=60`) | Sí (hereda de E2) |

E1 y E2 comparten el mismo pipeline de texto y el mismo vocabulario: la **única**
diferencia entre ambas es la representación, de modo que cualquier diferencia en
el ranking es atribuible a la reducción dimensional.

La consulta se proyecta al espacio reducido con el *fold-in* de Deerwester:
`q_k = qᵀ V Σ⁻¹`, usando el mismo IDF y la misma normalización aprendidos del
corpus.

---

## Estructura de archivos

```
.
├── taller_4.Rmd              # documento reproducible (fuente)
├── taller_4.html             # documento compilado
├── app.R                     # aplicación Shiny (Taller 2 + buscador)
├── build_index.R             # precálculo y serialización del índice
├── enriquecer_openalex.R     # reintento de métricas de impacto (ver más abajo)
├── install_deps.R            # dependencias
├── README.md
├── R/
│   ├── text_processing.R     # normalización, tokenización, DTM
│   ├── retrieval.R           # BM25, LSA, RRF, ranking, fragmentos
│   └── evaluation.R          # Precision@k, MRR, nDCG@k, Recall@k
├── data/
│   └── jmlr_q1_2025.sqlite   # base del Taller 1 (308 artículos, Vol. 26)
├── index/
│   └── indice_jmlr.rds       # índice precalculado (4.8 MB, xz)
└── eval/
    ├── consultas.csv                # las 5 consultas y su tipo
    ├── juicios_relevancia.csv       # juicios manuales (pooling)
    ├── resultados_evaluacion.csv    # métricas por consulta y estrategia
    └── sensibilidad_k.csv           # calidad en función de k
```

`app.R` hace `source()` de los mismos módulos de `R/` que usa `taller_4.Rmd`:
**no hay código duplicado** entre el informe y la aplicación, y las estrategias
documentadas son literalmente las que ofrece el buscador.

---

## Ejecución

### 1. Dependencias

```r
source("install_deps.R")
```

### 2. Construir el índice (una sola vez, ~10 s)

```bash
Rscript build_index.R
```

Genera `index/indice_jmlr.rds` con la DTM, los pesos BM25, el TF-IDF y el modelo
LSA ya ajustado.

### 3. Lanzar la aplicación

```bash
Rscript -e 'shiny::runApp(".")'
```

### 4. Compilar el informe

```bash
Rscript -e 'rmarkdown::render("taller_4.Rmd")'
```

---

## Uso del buscador

Pestaña **Buscador**:

- **Consulta** en lenguaje natural (el corpus es 100 % inglés; las consultas en
  español no recuperan nada útil — ver limitaciones en el informe).
- **Modo**: una estrategia, o comparar BM25 y LSA lado a lado con la misma consulta.
- **Resultados a mostrar**: 5, 10 o 20.

Cada resultado muestra posición, título, autores, fecha, tema, DOI enlazado,
puntaje y el fragmento del resumen con mayor densidad de términos de la consulta.

Consulta de ejemplo para probar: `conformal prediction uncertainty quantification`

---

## Notas de despliegue

**El índice se carga una sola vez.** `readRDS()` está **fuera** de `server()`, de
modo que el objeto se comparte entre todas las sesiones del proceso. Una consulta
solo ejecuta: tokenizar una cadena corta, un producto matriz–vector, ordenar 308
puntajes y extraer fragmentos (~25 ms). **Nunca** se reconstruye la matriz ni se
reajusta el SVD.

**Consecuencia:** si se añaden artículos con el botón de scraping, no aparecerán
en el buscador hasta volver a ejecutar `build_index.R`. La aplicación lo advierte
en el mensaje de actualización.

**Al desplegar en shinyapps.io** hay que subir `data/`, `index/` y `R/` junto con
`app.R`. Las tres dependencias nuevas respecto al Taller 2 son `Matrix`, `irlba`
y `SnowballC`.

---

## Limitación conocida: métricas de impacto (citas, descargas, referencias)

El dashboard muestra **N/D** en «Promedio de citas» y «Promedio de referencias»,
y la columna «Descargas» de la tabla aparece vacía. **No es un fallo del
procesamiento: los datos no existen en la fuente.** Se documenta aquí porque es
una limitación heredada del Taller 1 que condiciona decisiones de diseño del
Taller 4.

### Por qué no están disponibles

**1. JMLR no publica métricas de impacto.** El índice de volumen
(`jmlr.org/papers/v26/`) expone título, autores, volumen/páginas/año y los
enlaces `abs`, `pdf`, `bib` y `code`. No hay citas, descargas, *views* ni
*accesses* en ninguna parte del sitio. No es que el scraper no los capturara:
no existe el campo que raspar.

**2. Las referencias solo están dentro del PDF.** El HTML del artículo no expone
la bibliografía de forma estructurada. Extraerla exigiría parsear 308 PDFs con
formatos de citación heterogéneos, un problema de reconocimiento de referencias
bibliográficas que excede el alcance de ambos talleres y cuya tasa de error sería
difícil de acotar.

**3. OpenAlex no tenía indexado el Vol. 26 bajo el *source* del journal.** Era la
vía estructurada para obtener `cited_by_count` y `referenced_works`. En el Taller
1 (mayo de 2026) se consultó con tres filtros distintos:

| Filtro | Resultado |
|---|---|
| `primary_location.source.id:S192987850` (ID histórico) | 0 |
| `primary_location.source.id:S118988714` (ID Wikidata) | 0 |
| `primary_location.source.issn:1532-4435` | 0 |

Para descartar que el problema fuera del filtro y no de la cobertura, se hizo una
búsqueda libre por nombre de revista y `publication_year:2025`: aparecen ~19 885
resultados, pero **ninguno alojado bajo un `source` correspondiente a JMLR** —
figuran en arXiv, repositorios institucionales y otros repositorios secundarios.
Es decir, los artículos sí están en OpenAlex, pero el vínculo con el journal
canónico no estaba establecido.

### Dificultades adicionales encontradas

**El campo no era el problema.** `cited_by_count` es el atributo correcto y ya se
leía en el Taller 1. Pero es un atributo *de cada registro devuelto*: si el array
`results` viene vacío, no hay ningún registro del cual leerlo. La dificultad no
estaba en identificar el campo sino en lograr que la API devolviera los trabajos.

**Los DOIs de la base son sintéticos.** JMLR no asigna DOI a sus artículos; el
scraper del Taller 1 los construye con
`paste0("10.5555/jmlr.", vol_tag, ".", paper_id_str)`. El prefijo `10.5555` es el
usado convencionalmente para registros sin DOI real, de modo que
`https://doi.org/10.5555/jmlr.v26.24-1009` probablemente no resuelve. Esto cierra
la vía de consultar OpenAlex por DOI, que habría sido la más precisa.

**OpenAlex ahora requiere clave.** Desde febrero de 2025 el límite sin clave es de
~100 peticiones diarias (insuficiente para 308 consultas por título). La clave es
gratuita y eleva el límite a 100 000/día.

**Las citas recuperadas no serían del artículo en JMLR.** Si los artículos solo
son alcanzables a través de sus versiones en repositorios secundarios, el
`cited_by_count` obtenido corresponde a ese registro. OpenAlex normalmente fusiona
las versiones de un mismo trabajo en un único `work`, pero mientras el volumen no
esté indexado bajo el journal, cabe la posibilidad de que la cifra refleje citas
al *preprint* y no a la versión publicada. Cualquier uso de estos números debería
declarar ese matiz.

### Reintento: `enriquecer_openalex.R`

Como la cobertura de OpenAlex se actualiza de forma continua, el script permite
repetir la consulta sin rehacer el Taller 1:

```bash
Rscript enriquecer_openalex.R              # ensayo: informa, no escribe nada
Rscript enriquecer_openalex.R --escribir   # aplica los cambios a la base
```

Opera en dos etapas:

- **Estrategia A** — filtro por `source` (2 peticiones). Es la del Taller 1.
- **Estrategia B** — búsqueda por `title.search` artículo por artículo, para lo
  que A no cubra (~308 peticiones, ~2 min). Cada candidato pasa por verificación
  de título (coincidencia exacta normalizada o Jaccard ≥ 0.80): sin esa guarda,
  `title.search` devolvería siempre «lo más parecido» aunque fuera otro artículo.

Si necesitas clave, se lee de una variable de entorno para **no dejarla escrita
en GitHub**, como exige el enunciado:

```r
Sys.setenv(OPENALEX_KEY = "tu_clave")   # obtenida gratis en openalex.org
```

El script **no modifica la base si la cobertura es 0**, y por defecto no
sobrescribe valores ya poblados. El DOI real recuperado se guarda en una columna
nueva `doi_openalex`, sin tocar la columna `doi` original.

### Efecto sobre el Taller 4: ninguno

El enunciado establece que *«no se considerará suficiente ordenar los artículos
únicamente por fecha, citas o descargas»* y que el ranking principal debe
construirse desde una representación numérica del texto. El buscador cumple eso
por diseño: BM25 y LSA puntúan **exclusivamente** a partir del contenido textual,
y las citas no intervienen en ningún punto — ni como criterio principal ni como
desempate, que se resuelve por año y `paper_num`.

Esta decisión se mantendría aunque las columnas se poblaran. Si la cobertura
resultara **parcial**, ponderar por una métrica disponible solo para una fracción
del corpus penalizaría sistemáticamente a los artículos no emparejados por una
razón ajena a su relevancia, introduciendo un sesgo peor que el de no usarlas.

---

## Reproducibilidad

Dos detalles que afectan a que el índice sea **idéntico en cualquier máquina**:

1. La normalización de caracteres usa un mapeo explícito con `chartr`, no
   `iconv(..., "ASCII//TRANSLIT")`, cuyo resultado **depende del locale** (en
   locale C descarta caracteres que en UTF-8 sí translitera).
2. El vocabulario se ordena con `sort(method = "radix")`, que usa orden de bytes
   y no la colación del locale.

Verificado idéntico bajo `C`, `C.UTF-8` y `en_US.UTF-8`. Los archivos fuente
están guardados en UTF-8.

---

## Corrección respecto al Taller 2

`insertar_papers()` insertaba sin `paper_id` y recuperaba la clave con
`last_insert_rowid()`. Como `paper_id` es `TEXT PRIMARY KEY` (y no un alias de
`rowid`), los artículos nuevos quedaban con la clave en `NULL`, lo que rompía la
relación con `paper_authors`. Ahora se inserta explícitamente. Todo lo demás del
dashboard se conserva sin cambios: filtros, 6 indicadores, 4 visualizaciones,
tabla con enlaces DOI y actualización por scraping.

---

## Si algo falla

**`no fue posible abrir el archivo 'R/text_processing.R'`**

Los scripts resuelven sus caminos respecto a su propia ubicación, así que este
error significa que **falta la carpeta `R/` junto al script**. Casi siempre es
porque se descargaron archivos sueltos (`app.R`, `taller_4.Rmd`) en vez del
proyecto completo. Comprueba con:

```r
list.files()   # deben aparecer: R/, data/, index/, app.R, build_index.R
```

Si solo ves los `.R` sueltos, descomprime `taller_4_jmlr.zip` completo y trabaja
dentro de esa carpeta.

**`No se encontró index/indice_jmlr.rds`**

Falta construir el índice. Ejecuta `Rscript build_index.R` (o
`source("build_index.R")`) **antes** de lanzar la app.

**Las tarjetas de citas y referencias muestran N/D**

Es esperado: los datos no existen en la fuente. Ver la sección «Limitación
conocida: métricas de impacto» más arriba, y `enriquecer_openalex.R` si quieres
reintentar la consulta a OpenAlex.

**La app arranca pero el buscador no devuelve nada**

Revisa el recuadro de la barra lateral: indica cuántos artículos y términos tiene
el índice cargado y cuándo se construyó. Si el número de artículos no coincide
con la base, vuelve a ejecutar `build_index.R`.
