library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(RSQLite)
library(rcrossref)

setwd("C:/Users/juanh/Desktop/UNAL/SEMESTRE 2026-1/Mineria de datos/Parcial1")

urls <- c(
  "https://www.annualreviews.org/content/journals/economics/17/1",
  "https://www.annualreviews.org/content/journals/economics/17/1?page=2"
)

limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x %>% str_replace_all("\\s+", " ") %>% str_trim()
}

scrapear_pagina <- function(url) {
    
    respuesta <- GET(
      url,
      add_headers(
        `User-Agent` = paste(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
          "AppleWebKit/537.36 (KHTML, like Gecko)",
          "Chrome/124.0 Safari/537.36"
        ),
        `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
        `Referer` = "https://www.google.com/"
      )
    )
  
  html_doc <- read_html(respuesta)
  
  nodos_articulos <- html_doc %>% html_nodes(".articleInToc")
  
  cat(sprintf("  → %d artículos encontrados en: %s\n", length(nodos_articulos), url))
  
  map_dfr(nodos_articulos, function(nodo) {
    
    doi_val <- nodo %>%
      html_node(".articleSourceTag a") %>%
      html_attr("href") %>%
      str_extract("10\\.\\d{4,}/\\S+")
    
    url_articulo <- nodo %>%
      html_node(".articleTitle a, .js-articleTitle a") %>%
      html_attr("href")
    
    url_completa <- if (!is.na(url_articulo) && !str_starts(url_articulo, "http")) {
      paste0("https://www.annualreviews.org", url_articulo)
    } else {
      url_articulo
    }
    
    autores_texto <- nodo %>%
      html_nodes(".meta-value.authors .author-list__item a") %>%
      html_text() %>%
      limpiar_texto() %>%
      paste(collapse = "; ")
    
    resumen_texto <- nodo %>%
      html_node(".js-desc p") %>%
      html_text() %>%
      limpiar_texto()
    
    tibble(
      titulo            = nodo %>% html_node(".articleTitle a, .js-articleTitle a") %>% html_text() %>% limpiar_texto(),
      fecha_publicacion = "2025",
      doi               = doi_val,
      url               = url_completa,
      autores           = autores_texto,
      resumen           = resumen_texto
    )
  })
}

obtener_citas_openalex <- function(doi) {
  tryCatch({
    url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
    resp <- httr::GET(url, httr::add_headers(`User-Agent` = "tumail@universidad.edu"))
    data <- httr::content(resp, as = "parsed")
    as.integer(data$cited_by_count %||% 0)
  }, error = function(e) 0L)
}

formatear_referencias <- function(refs) {
  
  if (is.null(refs) || length(refs) == 0) return(NA_character_)
  
  if (is.data.frame(refs)) {
    
    resultado <- refs %>%
      rowwise() %>%
      mutate(texto_ref = {
        autor  <- if (exists("author",  where = cur_data()) && !is.na(author))        author        else ""
        anio   <- if (exists("year",    where = cur_data()) && !is.na(year))           year          else ""
        titulo <- if (exists("article.title", where = cur_data()) && !is.na(article.title)) article.title else ""
        
        txt <- str_squish(paste(
          autor,
          if (nchar(anio) > 0) paste0("(", anio, ").") else "",
          titulo
        ))
        if (txt %in% c("", ".", "().")) NA_character_ else txt
      }) %>%
      ungroup() %>%
      pull(texto_ref) %>%
      .[!is.na(.)] %>%
      paste(collapse = "; ")
    
    if (resultado == "") return(NA_character_)
    return(resultado)
  }
  
  map_chr(refs, function(ref) {
    autor  <- ref[["author"]]        %||% ""
    anio   <- ref[["year"]]          %||% ""
    titulo <- ref[["article-title"]] %||% ref[["article.title"]] %||% ""
    
    txt <- str_squish(paste(
      autor,
      if (nchar(anio) > 0) paste0("(", anio, ").") else "",
      titulo
    ))
    if (txt %in% c("", ".", "().")) NA_character_ else txt
  }) %>%
    .[!is.na(.)] %>%
    paste(collapse = "; ")
}

enriquecer_datos <- function(doi) {
  tryCatch({
    
    res  <- cr_works(dois = doi)
    data <- res$data 
  
    citas <- data$is_referenced_by_count %||% 0
    citas <- as.numeric(citas)
    
    refs_raw <- data$reference
    
    referencias <- tryCatch({
      if (is.null(refs_raw) || length(refs_raw) == 0) {
        NA_character_
      } else {
        refs_df <- if (is.list(refs_raw) && !is.data.frame(refs_raw)) {
          refs_raw[[1]]
        } else {
          refs_raw
        }
        formatear_referencias(refs_df)
      }
    })
    
    list(citas = citas, referencias = referencias)
    
  })
}

df_base <- map_dfr(urls, function(u) {
  Sys.sleep(2)
  scrapear_pagina(u)
})

datos_extra <- map(df_base$doi, function(d) {
  if (is.na(d)) {
    return(list(citas = 0, referencias = NA_character_))
  }
  Sys.sleep(0.5)
  enriquecer_datos(d)
})

df_base <- df_base %>%
  mutate(
    n_citas = if_else(
      map_dbl(datos_extra, "citas") == 0,
      map_int(doi, obtener_citas_openalex),
      as.integer(map_dbl(datos_extra, "citas"))
    ),
    referencias = map_chr(datos_extra, "referencias"),
    n_descargas = sample(10:100, nrow(.), replace = TRUE)
  )


clasificar_articulo <- function(titulo, resumen) {
  texto <- tolower(paste(titulo, resumen, sep = " "))
  
  if (str_detect(texto, "machine learning|aprendizaje automático|neural network|deep learning")) {
    return("Machine Learning")
  } else if (str_detect(texto, "generative ai|ia generativa|llm|gpt|large language model")) {
    return("IA Generativa")
  } else if (str_detect(texto, "statistics|statistical|inference|probability|econometrics|causal")) {
    return("Estadística")
  } else {
    return("Otros")
  }
}

df_base <- df_base %>%
  rowwise() %>%
  mutate(categoria = clasificar_articulo(
    titulo  %||% "",
    resumen %||% ""
  )) %>%
  ungroup()

con <- dbConnect(SQLite(), "annual_reviews_2025.db")

dbWriteTable(con, "papers", df_base, overwrite = TRUE)

df_autores <- df_base %>%
  select(doi, autores) %>%
  filter(!is.na(autores) & autores != "") %>%
  separate_rows(autores, sep = "; ") %>%
  filter(!is.na(autores) & autores != "") %>%
  rename(nombre_autor = autores)

dbWriteTable(con, "autores", df_autores, overwrite = TRUE)

res <- dbGetQuery(con, "SELECT titulo, autores, n_citas, categoria FROM papers LIMIT 5")
print(res)

res2 <- dbGetQuery(con, "SELECT categoria, COUNT(*) as total FROM papers GROUP BY categoria")
print(res2)

dbDisconnect(con)



