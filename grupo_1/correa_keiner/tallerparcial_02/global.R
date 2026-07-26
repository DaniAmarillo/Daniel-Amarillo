library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(highcharter)
library(DT)
library(httr)
library(rvest)
library(purrr)
library(tibble)
library(rsconnect)


#publicación de la app

rsconnect::setAccountInfo(name='33ayg6-keiner0felipe-correa0leguizamon', 
                          token='5CB0BD33D1D4F4E8D58659A99ACD59CA', 
                          secret='C4mN7RYuwmU6PmFLh8tosF+wbCk3NTlBH7EgOa8S')





# Conexión a SQLite 
DB <- "revista_plosone.sqlite"

get_con <- function() dbConnect(SQLite(), DB)

# Funciones de clasificación de los articulos
clasificar_articulo <- function(title, abstract) {
  texto <- tolower(paste(title, abstract))
  if (grepl("machine learning|deep learning|neural network|classification|random forest|xgboost|supervised|unsupervised|reinforcement learning", texto)) {
    return("Machine Learning")
  } else if (grepl("generative|large language model|llm|gpt|diffusion|transformer|chatgpt|bert|stable diffusion|text generation|image generation", texto)) {
    return("IA Generativa")
  } else if (grepl("regression|bayesian|statistical|hypothesis|p-value|correlation|variance|anova|probability|distribution|sampling", texto)) {
    return("Estadística")
  } else {
    return("Otros")
  }
}

# Scraping de un artículo individual
scrapear_articulo <- function(url) {
  tryCatch({
    doi <- sub(".*id=", "", url)

    api_url <- paste0(
      'https://api.plos.org/search?q=id:"', doi,
      '"&fl=id,title_display,publication_date,author_display,abstract,counter_total_all&wt=json'
    )
    r <- GET(api_url, user_agent("Mozilla/5.0"))
    if (status_code(r) != 200) stop("API PLOS error")

    json    <- content(r, "parsed")
    doc     <- json$response$docs[[1]]
    title   <- doc$title_display
    date    <- as.Date(substr(doc$publication_date, 1, 10))
    authors <- paste(unlist(doc$author_display), collapse = ", ")
    abstract <- paste(unlist(doc$abstract), collapse = " ") %>% trimws()
    views   <- doc$counter_total_all

    res  <- GET(url, user_agent("Mozilla/5.0"))
    html <- read_html(content(res, "text", encoding = "UTF-8"))

    references <- html %>%
      html_nodes("ol.references li") %>%
      html_text(trim = TRUE) %>%
      gsub("\\s+", " ", .)

    references_count <- length(references)

    cited_by <- NA
    oa_url   <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
    oa_r     <- GET(oa_url)
    if (status_code(oa_r) == 200) {
      cited_by <- content(oa_r, "parsed")$cited_by_count
    }

    tibble(
      title, date, doi, url, authors,
      abstract, cited_by, views,
      references_count,
      references = list(references)
    )
  }, error = function(e) {
    message("Error en: ", url, " — ", e$message)
    return(NULL)
  })
}

# Obtener links de páginas nuevas (2026)
obtener_links_nuevos <- function(paginas = 1:5) {
  lista_links <- c()
  for (p in paginas) {
    url <- paste0(
      "https://journals.plos.org/plosone/browse/computer_and_information_sciences?resultView=list&page=", p
    )
    res  <- GET(url, user_agent("Mozilla/5.0"))
    html <- read_html(content(res, "text"))
    links <- html %>%
      html_nodes("a[href*='/plosone/article?id=']") %>%
      html_attr("href")
    lista_links <- c(lista_links, links)
    Sys.sleep(runif(1, 1, 2))
  }
  lista_links <- unique(lista_links)
  paste0("https://journals.plos.org", lista_links)
}

# Función principal de actualización 
buscar_articulos_nuevos <- function(progress_callback = NULL) {
  con        <- get_con()
  dois_exist <- dbGetQuery(con, "SELECT doi FROM papers")$doi

  if (!is.null(progress_callback)) progress_callback("Obteniendo links recientes...", 0.1)
  links_nuevos <- obtener_links_nuevos(paginas = seq(1, 120, by =20))

  dois_nuevos <- sub(".*id=", "", links_nuevos)
  links_filtrados <- links_nuevos[!dois_nuevos %in% dois_exist]

  if (length(links_filtrados) == 0) {
    # Reconsultar últimos 5 artículos almacenados
    if (!is.null(progress_callback)) progress_callback("Sin artículos nuevos. Verificando últimos 5...", 0.5)
    ultimos_5 <- dbGetQuery(con, "SELECT doi, url FROM papers ORDER BY paper_id DESC LIMIT 5")
    dbDisconnect(con)
    return(list(nuevos = 0, verificados = ultimos_5$doi, df_nuevos = NULL))
  }

  if (!is.null(progress_callback)) progress_callback(paste("Scrapeando", length(links_filtrados), "artículos nuevos..."), 0.3)

  df_nuevos_raw <- map2_dfr(
    links_filtrados, seq_along(links_filtrados),
    function(url, i) {
      if (!is.null(progress_callback))
        progress_callback(paste("Artículo", i, "de", length(links_filtrados)), 0.3 + 0.5 * (i / length(links_filtrados)))
      Sys.sleep(runif(1, 0.5, 1.5))
      scrapear_articulo(url)
    }
  )

  if (is.null(df_nuevos_raw) || nrow(df_nuevos_raw) == 0) {
    dbDisconnect(con)
    return(list(nuevos = 0, verificados = character(0), df_nuevos = NULL))
  }

  # Preparar e insertar papers nuevos
  max_id <- dbGetQuery(con, "SELECT MAX(paper_id) AS m FROM papers")$m
  df_insert <- df_nuevos_raw %>%
    mutate(
      paper_id         = (max_id + 1):(max_id + n()),
      journal_name     = "PLOS ONE",
      publication_date = as.character(date),
      year             = as.integer(year(date)),
      n_authors        = sapply(strsplit(authors, ",\\s*"), length),
      citations        = as.integer(cited_by),
      n_references     = as.integer(references_count),
      topic_label      = mapply(clasificar_articulo, title, abstract)
    ) %>%
    select(paper_id, journal_name, title, publication_date, year,
           doi, url, abstract, authors, n_authors, citations, views, n_references, topic_label)

  dbWriteTable(con, "papers", df_insert, append = TRUE)
  dbDisconnect(con)

  if (!is.null(progress_callback)) progress_callback("¡Listo!", 1)

  list(nuevos = nrow(df_insert), verificados = character(0), df_nuevos = df_insert)
}

# Helpers de consulta 
get_papers_filtrados <- function(fecha_inicio = NULL, fecha_fin = NULL,
                                  topic = NULL, autor = NULL,
                                  doi_busq = NULL, keyword = NULL) {
  con <- get_con()
  q   <- "SELECT * FROM papers WHERE 1=1"

  if (!is.null(fecha_inicio) && fecha_inicio != "")
    q <- paste0(q, " AND publication_date >= '", fecha_inicio, "'")
  if (!is.null(fecha_fin) && fecha_fin != "")
    q <- paste0(q, " AND publication_date <= '", fecha_fin, "'")
  if (!is.null(topic) && topic != "Todos")
    q <- paste0(q, " AND topic_label = '", topic, "'")
  if (!is.null(autor) && autor != "")
    q <- paste0(q, " AND authors LIKE '%", autor, "%'")
  if (!is.null(doi_busq) && doi_busq != "")
    q <- paste0(q, " AND doi LIKE '%", doi_busq, "%'")
  if (!is.null(keyword) && keyword != "")
    q <- paste0(q, " AND (title LIKE '%", keyword, "%' OR abstract LIKE '%", keyword, "%')")

  df <- dbGetQuery(con, q)
  dbDisconnect(con)
  df
}
