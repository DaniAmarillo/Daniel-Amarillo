# ==================================================================================================
#               EXTRACCIÓN DE INFORMACIÓN - JOURNAL OF STATISTICAL SOFTWARE (JSS)
# ==================================================================================================
# El Journal of Statistical Software es una revista de acceso abierto de gran importancia
# en la comunidad estadística y de ciencia de datos. Publica artículos sobre software,
# algoritmos y métodos computacionales ampliamente utilizados en estadística aplicada.
#
# La cantidad de citaciones por artículo y la referencias usadas en el artículo no pueden accederse
# directamente desde la página del JSS por lo que se harán consultas a otras herramientas externas. 
# 
# Autor           : Michel Mendivenson Barragán Zabala
# Materia         : Minería de Datos
# ==================================================================================================

paquetes <- c("rvest",      # web scrapping
              "httr",       # web scrapping with headers
              "xml2",       # parsear HTML
              "dplyr",      # manipulación dataframes
              'stringr',    # manipulación strings
              'httr2',      # Semantic Scholar API handling
              'rollama',    # acceder a ollama desde R
              'beepr')      # notificaciones de sonido

nuevos <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if (length(nuevos) > 0) install.packages(nuevos)

invisible(lapply(paquetes, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# ------------------------- PASO 1: RECOLECCIÓN INFORMACIÓN VOLÚMENES ------------------------------
# Cada uno de los volumenes son guardados en una nueva instancia por lo que si se conoce la url 
# de cada uno de los volumenes se puede hacer scrapping de los artículos para cada uno de los
# volúmenes
# 
# OUTPUT: Tabla vol con la siguiente info
#   - id = Número del volumen
#   - order = Orden de publicación en el año
#   - year = Año de publicación
#   - url = URL en que se aloja ese volumen

cat('
------------ JSS Articles Scraping ------------
This process may take a while. Sound cues will
play after each step and upon completion.

Note: Article classification requires the
ollama3.2:3b model running locally to work.\n\n')

cat("* Retrieving volumes info: PROCESSING ⌛\r")

archive <- read_html('https://www.jstatsoft.org/issue/archive')

vol <- data.frame(
  id = archive |> 
           html_elements(".media-heading .title") |> 
           html_text2(), 
  url    = archive |> 
           html_elements(".media-heading .title") |>
           html_attr('href')
)

vol$year <- str_remove(archive |> 
                       html_elements(".media-heading") |> 
                       html_text2(),
                       paste0(vol$id, collapse = '|', end = ' , ')) |>
            as.numeric()

vol$id <- vol$id |> str_remove('Volume ') |> as.numeric()

vol <- vol |> 
  dplyr::arrange(year, id)

vol$order <- sequence(table(vol$year))

vol <- vol[, c("id","order","year","url")]

beepr::beep()
cat("* Retrieving volumes info: CHECK ✅     \n\n")

# ------------------------- PASO 2: RECOLECCIÓN SISTEMÁTICA DE ARTÍCULOS ---------------------------
# Ahora se recorreran las URLs de todos los volumenes para extraer las urls de los articulos (Así como
# sus títulos) y de forma sistemática recorrer las urls de esos artículos para extraer la información
# necesaria de los artículos. 

# RECOLECCIÓN URL DE ARTÍCULOS: 
# Se recorren los archives de los diferentes volumenes para relacionarlos con los artículos de cada
# uno de los volumenes para luego poder filtrar los artículos por año de ser necesarios.
# 
# OUTPUT: Tabla articles con la siguiente información
#   - title: Título del artículo
#   - issue: Orden de publicación de los artículos dentro de un volumen
#   - url: URL en la que está alojado el artículo

total.vol <- nrow(vol)
articles <- data.frame()
count <- 1
for (id in vol$id){
  volume = vol$url[vol$id == id]
  cat(sprintf('* Retrieving articles URLs: (%d/%d) Vol. ⌛\r', count, total.vol))
  articles <- rbind(
    articles,
    cbind(vol.id = id,
          title = read_html(volume) |>
            html_elements('.media-heading a') |>
            html_text2() |>  str_trim(),

          issue = read_html(volume) |>
            html_elements('.col-sm-3') |>
            html_text2() |>
            str_trim() |>
            (\(x) sub(".*,\\s*", "", x))() |>
            str_remove('Issue '),

          url = read_html(volume) |>
            html_elements('.media-heading a') |>
            html_attr('href'))
  )
  Sys.sleep(1)
  count <- count + 1
}
beepr::beep()
cat('* Retrieving articles URLs: CHECK ✅              \n\n')


# RECOLECCIÓN INFORMACIÓN DE LOS ARTÍCULOS: 
# El JSS no disponibiliza directamente ninguna información relacionada con la cantidad de veces que
# se ha citado el artículo ni las referencias por lo que esa información se recolectará
# posteriormente. 
# 
# OUTPUT: Con el fin de montar una estructura de datos luego y facilitar la relación entre autores y 
# artículos la información de los autores para cada artículo se guarda en una tabla diferente 
#   * art.info: Información principal de los artículos
#     - url: url del artículo
#     - abstract: abstratc del artículo
#     - date: Fecha de publicación
#     - DOI: DOI del artículo
#   * art.aut.assoc: Relación autoresy artículos
#     - author: Nombre del autor
#     - ORCID: Identificación ORCID del autor
#     - DOI: Artículo relacionado al autor
# 
# Nota (1): Hasta el primer volumen del 2020, la dirección ORCID de los autores no se expone
# directamente en la página del artículo.
# Nota (2): Tanto art.info y art.aut.assoc pasarán por cambios para ser más coherentes con la
# estructura usual de bases de datos en SQL.

art.info <- data.frame()
art.aut.assoc <- data.frame()
total.art <- nrow(articles)
count <- 1

for (url in articles$url){
  cat(sprintf('* Retrieving articles info: (%d/%d) ⌛\r', count, total.art))
  
  article <- read_html(url)
  
  art.info <- rbind(
    art.info,
    cbind(url = url, 
          abstract = article|> 
            html_element('.article-abstract') |> 
            html_text2(), 
          date = article|> 
            html_elements(".col-sm-8") |> 
            html_text2() |> 
            (\(x) (x[c(2)]))(),
          DOI = article|> 
            html_elements(".col-sm-8") |> 
            html_text2() |> 
            (\(x) (x[c(3)]))())
  )
  
  art.aut.assoc <- rbind(
    art.aut.assoc,
    cbind(
      author = article |> 
        html_elements('.authors span strong') |> 
        html_text2(),
      ORCID = article |> 
        html_elements('.authors span') |>
        html_element('a.orcid') |> 
        html_attr('href'),
      DOI = article|> 
        html_elements(".col-sm-8") |> 
        html_text2() |> 
        (\(x) (x[c(3)]))())
  )
  
  count <- count + 1
}

beepr::beep()
cat('* Retrieving articles info: CHECK ✅              \n')

# ---------------------------- PASO 3: VERSIÓN FINAL DE LAS TABLAS ---------------------------------
# Se reúne la información recolectada en 4 tablas: 
#   - vol: Información de los volumenes de la revista 
#   - articles: Información de los artículos
#   - authors: Nombre y dirección ORCID de los autores
#   - authorship: ORCID author y DOI de artículos. Para relacionar artículos con autores

articles <- articles |> left_join(art.info, by = c('url' = 'url'))

authors <- art.aut.assoc |> 
  arrange(ORCID) |> 
  distinct(ORCID, author, .keep_all = T) |>
  (\(x) (x[c('ORCID', 'author')]))()

authorship  <- art.aut.assoc[art.aut.assoc$ORCID != '',] |>
  (\(x) (x[c('ORCID', 'DOI')]))()

# --------------------------- PASO 4: GENERANDO INFORMACIÓN FALTANTE -------------------------------
# Como se mencionó anteriormente, en la págionma del JSS no se menciona ninguna métrica de importancia
# ni la cantidad de citas de un artículo por lo que inicialmente se traerá la métrica de importancia 
# (Influential citation count), la cantidad de citas y la cantidad de referencias desde la API
# pública de Semantic Scholar mientras se encuentra una mejor métrica. 
# Del mismo modo, la clasificación de artículos en Machine Learning, IA generativa, Estadística y Otros
# se realiza usando uno de los modelos LLM open source de Ollama. 
# 
# En ese orden de ideas se generarán nuevas columnas en la tabla articles: 
#   - citation: Cantidad de citas del artículo
#   - references: Cantidad de referencias
#   - influential citations: Cantidad de citas de importancia 
#   - topíc: Clasificación temática

# OBTENCIÓN DE LA CANTIDAD DE CITAS. 
# La información de Semantic Scholar (Sobre todo la cantidad de citas) suele estar desincronizada
# con la información de Google Scholar, pero debido a la relación de la métrica de importancia con
# la cantidad total de citaciones del artículo por el momento se usará completamente la información
# de Semantic Scholar

total.art <- nrow(articles)
count <- 1
fields <- paste(c(
  "citationCount",
  "referenceCount",
  "influentialCitationCount"
), collapse = ",")

new_info <- c()

for (doi in articles$DOI) {
  
  cat(sprintf('* Retrieving articles metrics: (%d/%d) ⌛\r', count, total.art))
  
  success <- FALSE
  
  # Este while existe con el único objetivo de evitar el rate limit de la API pública (1000 request
  # compartidas por segundo)
  while (!success) {
    resp <- request(paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi)) |>
      req_url_query(fields = fields) |>
      req_error(is_error = \(resp) FALSE) |>   # 👈
      req_perform(verbosity = 0)
    
    if (resp_status(resp) == 429) {
      Sys.sleep(10)
    } else {
      success <- TRUE  # 200 o 404 salen del ciclo
    }
  }
  
  body <- if (resp_status(resp) == 404) list() else resp_body_json(resp)  # 👈
  
  new_info <- rbind(
    new_info,
    cbind(
      "cites"                 = body$citationCount %||% NA,
      "references"            = body$referenceCount %||% NA,
      "influential citations" = body$influentialCitationCount %||% NA
    )
  )
  
  Sys.sleep(1)
  count <- count + 1
}

articles[, c("citations", "references", "influential citations")] <- new_info

beepr::beep()
cat('* Retrieving articles metrics: CHECK ✅              \n')

# CLASIFICACIÓN DE TEMÁTICA: 
# Se usará un modelo LLM local para hacer más sencilla la tarea de clasificación en el ejercicio. 

while (!ping_ollama(silent = TRUE)) {
  message(
    "\nOllama is not running.",
    "\n- If not installed, download it from: https://ollama.com/download",
    "\n- If already installed, run in the terminal: ollama serve\n"
  )
  invisible(readline("Press Enter to try again..."))
}

if (!check_model_installed("llama3.2:3b", check_only = T)){
  pull_model("llama3.2:3b", verbose = T)
}

total.art <- nrow(articles)
count <- 1
clas <- c()

for (info in articles$abstract){
  cat(sprintf('* Classifying articles: (%d/%d) ⌛\r', count, total.art))
  response <- query(
    tibble::tribble(
      ~role,    ~content,
      "system", "Classify the following abstract into exactly one of these categories:
- Machine Learning: papers related to ML methodology, including clustering, classification, prediction, neural networks, deep learning, supervised/unsupervised learning, software implementations of ML algorithms, etc.
- Generative AI: papers related to generative models, including large language models, text/image generation, diffusion models, GANs, transformers, LLMs, etc.
- Statistics: papers related to statistical methodology, including inference, hypothesis testing, probability, regression, experimental design, statistical models, clinical trial design, and software implementations of statistical methods.
- Other: papers that apply existing methods to a specific domain problem without any methodological focus.
      
Reply in this exact format:
  Reasoning: <Reasoning behind why the category election>
  Classification: <category>",
      "user", info), 
    model = "llama3.2:3b", 
    stream  = FALSE, 
    verbose = FALSE, 
    keep_alive = 0,
    model_params = list(temperature = 0, seed = 0513)
  )
  clas <- c(clas, 
             response[[1]]$message$content |> 
               str_extract('(?<=Classification: ).*'))
  count <- count + 1
}

articles$topic <- clas
cat('* Classifying articles: CHECK ✅              \n')
beepr::beep()

# TRAYENDO REFERENCIAS:
# La API de Semantic Scholar pagina las referencias en grupos de 100 por lo que para artículos
# con muchas referencias es necesario hacer múltiples consultas para obtener el listado completo.
#
# OUTPUT: Tabla references con la siguiente información:
#   - DOI.origin: DOI del artículo original
#   - DOI.reference: DOI del artículo referenciado (NA si no está disponible)
#   - paperId: ID interno de Semantic Scholar del artículo referenciado

total.art <- nrow(articles)
count <- 1
all_references <- data.frame()

for (doi in articles$DOI) {
  
  cat(sprintf('* Retrieving references: (%d/%d) ⌛\r', count, total.art))
  
  offset <- 0
  limit  <- 100
  keep_paging <- TRUE
  
  while (keep_paging) {
    
    success <- FALSE
    
    while (!success) {
      resp <- request(paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi, "/references")) |>
        req_url_query(fields = "externalIds,paperId", limit = limit, offset = offset) |>
        req_error(is_error = \(resp) FALSE) |>
        req_perform(verbosity = 0)
      
      if (resp_status(resp) == 429) {
        Sys.sleep(10)
      } else {
        success <- TRUE
      }
    }
    
    if (resp_status(resp) != 200) {
      keep_paging <- FALSE
      next
    }
    
    body <- resp_body_json(resp)
    data <- body$data
    
    if (length(data) == 0) {
      keep_paging <- FALSE
      next
    }
    
    page_refs <- do.call(rbind, lapply(data, function(ref) {
      cited <- ref$citedPaper
      data.frame(
        DOI.origin    = doi,
        DOI.reference = cited$externalIds$DOI %||% NA,
        paperId       = cited$paperId %||% NA,
        stringsAsFactors = FALSE
      )
    }))
    
    all_references <- rbind(all_references, page_refs)
    
    # Si la página devolvió menos de `limit` registros ya no hay más páginas
    if (length(data) < limit) {
      keep_paging <- FALSE
    } else {
      offset <- offset + limit
      Sys.sleep(1)
    }
  }
  
  Sys.sleep(1)
  count <- count + 1
}

references <- all_references


cat('* Retrieving references: CHECK ✅              \n')

beepr::beep('treasure')
cat('
------------ Scrapping complete ------------\n\n')

rm(list = setdiff(ls(), c('vol', 'articles', 'authors', 'authorship', 'references')))

# EOF
