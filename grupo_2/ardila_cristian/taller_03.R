#===========================================================
#  EJERCICIO 7.2 - DROGUERÍAS CAFAM
#  Scraping con rvest / httr2 y RSelenium (Firefox)
#===========================================================

#-------------------------------------
# 1. Paquetes
#-------------------------------------
paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "knitr", "RSelenium", "stringi"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

#-------------------------------------
# 2. Configuración general
#-------------------------------------
user_agent_cafam <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Firefox/125.0"
)

base_cafam <- "https://www.drogueriascafam.com.co"

# Reparación prudente de mojibake: solo intenta corregir cuando detecta
# patrones típicos de doble codificación.
reparar_mojibake <- function(x, max_iter = 2) {
  x <- as.character(x)
  
  # Reemplazo de espacios no separables
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  
  # Reparación iterativa, con criterio conservador
  for (i in seq_len(max_iter)) {
    idx <- !is.na(x) & stringr::str_detect(x, "Ã|Â|â€|�")
    if (!any(idx)) break
    
    candidato <- suppressWarnings(iconv(x[idx], from = "latin1", to = "UTF-8"))
    candidato[is.na(candidato)] <- x[idx][is.na(candidato)]
    
    score_actual <- stringr::str_count(x[idx], "Ã|Â|â€|�")
    score_cand   <- stringr::str_count(candidato, "Ã|Â|â€|�")
    
    mejora <- score_cand <= score_actual
    x[idx] <- ifelse(mejora, candidato, x[idx])
  }
  
  # Normalización Unicode y limpieza final
  x <- stringi::stri_trans_nfkc(x)
  x <- stringr::str_replace_all(x, "[[:cntrl:]]+", " ")
  x <- stringr::str_squish(x)
  
  ifelse(is.na(x) | x == "", NA_character_, x)
}

limpiar_texto <- function(x) {
  reparar_mojibake(x, max_iter = 2)
}

# Función para imprimir de forma más legible
preparar_impresion <- function(df, n = 10) {
  df %>%
    mutate(
      across(
        any_of(c("titulo", "marca", "descripcion", "enlace")),
        ~ stringr::str_trunc(.x, 60)
      )
    ) %>%
    head(n)
}

# Crear peticiones repetibles con encabezados consistentes
crear_peticion_cafam <- function(url) {
  request(url) %>%
    req_user_agent(user_agent_cafam) %>%
    req_headers(`accept-language` = "es-CO,es;q=0.9")
}

# Leer HTML con codificación explícita
leer_html_utf8 <- function(respuesta) {
  txt <- tryCatch(httr2::resp_body_string(respuesta), error = function(e) NA_character_)
  if (is.na(txt) || is.null(txt) || !nzchar(txt)) return(NULL)
  xml2::read_html(txt, encoding = "UTF-8")
}

# Extrae texto o atributo usando varios selectores CSS candidatos
# Si attr = NULL extrae texto; si se especifica, extrae ese atributo
extraer_css <- function(nodo, selectores, attr = NULL) {
  for (sel in selectores) {
    x <- tryCatch(html_element(nodo, sel), error = function(e) NULL)
    if (!is.null(x) && length(x) > 0) {
      val <- tryCatch(
        if (is.null(attr)) html_text2(x) else html_attr(x, attr),
        error = function(e) NA_character_
      )
      val <- limpiar_texto(val)
      if (!is.na(val)) return(val)
    }
  }
  NA_character_
}

# Normaliza enlaces relativos a absolutos
hacer_absoluto <- function(enlace) {
  if (is.na(enlace)) return(NA_character_)
  xml2::url_absolute(enlace, base_cafam)
}

# Tibble vacío de fallback para detalles no recuperables
detalle_vacio <- tibble(
  precio      = NA_character_,
  marca       = NA_character_,
  descripcion = NA_character_
)

#-------------------------------------
# 3. URL de búsqueda
#-------------------------------------
construir_url_cafam <- function(termino, pagina = 1) {
  paste0(
    base_cafam,
    "/search?controller=search&s=",
    URLencode(termino),
    "&page=", pagina
  )
}

#-------------------------------------
# 4. Extraer información desde la página de detalle
#-------------------------------------
extraer_detalle_cafam <- function(url_producto, pausa = 1) {
  Sys.sleep(pausa)
  
  respuesta <- tryCatch(
    crear_peticion_cafam(url_producto) %>% req_perform(),
    error = function(e) NULL
  )
  
  if (is.null(respuesta)) return(detalle_vacio)
  
  pagina <- leer_html_utf8(respuesta)
  if (is.null(pagina))   return(detalle_vacio)
  
  precio <- extraer_css(
    pagina,
    c(
      "meta[itemprop='price']",
      "meta[property='product:price:amount']",
      "meta[property='og:price:amount']"
    ),
    attr = "content"
  )
  
  if (is.na(precio)) {
    precio <- extraer_css(
      pagina,
      c(".current-price span", ".product-prices .current-price span", ".price")
    )
  }
  
  marca <- extraer_css(
    pagina,
    c("meta[itemprop='brand']", "meta[property='product:brand']"),
    attr = "content"
  )
  
  if (is.na(marca)) {
    marca <- extraer_css(
      pagina,
      c(".product-manufacturer", ".manufacturer-name", ".brand")
    )
  }
  
  if (is.na(marca)) {
    texto_total <- tryCatch(html_text2(pagina), error = function(e) NA_character_)
    if (!is.na(texto_total)) {
      m1 <- stringr::str_match(texto_total, "Nombre y/o Marca:\\s*([^\\n\\r]+)")
      if (!is.na(m1[1, 2])) marca <- limpiar_texto(m1[1, 2])
    }
  }
  
  descripcion <- extraer_css(
    pagina,
    c("meta[name='description']", "meta[property='og:description']"),
    attr = "content"
  )
  
  if (is.na(descripcion)) {
    descripcion <- extraer_css(
      pagina,
      c("#description", ".product-description", ".product-description-short")
    )
  }
  
  tibble(
    precio      = limpiar_texto(precio),
    marca       = limpiar_texto(marca),
    descripcion = limpiar_texto(descripcion)
  )
}

#-------------------------------------
# 5. Extraer información desde una tarjeta de resultado
#-------------------------------------
extraer_producto_cafam <- function(nodo, pausa = 1) {
  titulo <- extraer_css(
    nodo,
    c("h2 a", ".product-title a", "h3 a", "a.product-thumbnail")
  )
  
  enlace <- extraer_css(
    nodo,
    c("h2 a", ".product-title a", "h3 a", "a.product-thumbnail"),
    attr = "href"
  )
  enlace <- hacer_absoluto(enlace)
  
  precio_visible <- extraer_css(
    nodo,
    c(".price", ".current-price span", ".product-price", ".product-price-and-shipping .price")
  )
  
  detalle <- if (!is.na(enlace)) extraer_detalle_cafam(enlace, pausa = pausa) else detalle_vacio
  
  tibble(
    titulo          = limpiar_texto(titulo),
    enlace          = limpiar_texto(enlace),
    precio          = limpiar_texto(dplyr::coalesce(precio_visible, detalle$precio)),
    marca           = limpiar_texto(detalle$marca),
    descripcion     = limpiar_texto(detalle$descripcion),
    precio_visible  = !is.na(precio_visible)
  )
}

#-------------------------------------
# 6. Función principal: una página
#-------------------------------------
scrapear_pagina_cafam <- function(termino, pagina = 1, pausa = 2) {
  url <- construir_url_cafam(termino, pagina)
  
  Sys.sleep(pausa)
  
  respuesta <- tryCatch(
    crear_peticion_cafam(url) %>% req_perform(),
    error = function(e) NULL
  )
  
  if (is.null(respuesta)) return(tibble())
  
  pagina_html <- leer_html_utf8(respuesta)
  if (is.null(pagina_html)) return(tibble())
  
  nodos <- pagina_html %>%
    html_elements("article.product-miniature, .product-miniature, .js-product-miniature, div.product-miniature")
  
  if (length(nodos) == 0) nodos <- pagina_html %>% html_elements("div.product")
  if (length(nodos) == 0) return(tibble())
  
  map_dfr(nodos, ~extraer_producto_cafam(.x, pausa = 0.8)) %>%
    janitor::clean_names() %>%
    mutate(pagina = pagina, termino = termino) %>%
    mutate(across(where(is.character), limpiar_texto))
}

#-------------------------------------
# 7. Función principal: varias páginas
#-------------------------------------
scrapear_cafam_rvest <- function(termino, paginas = 1:3, pausa = 2) {
  map_dfr(
    paginas,
    ~scrapear_pagina_cafam(termino = termino, pagina = .x, pausa = pausa)
  ) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), limpiar_texto))
}

#-------------------------------------
# 8. Ejecución con rvest
#-------------------------------------
tabla_cafam <- scrapear_cafam_rvest("jabon", paginas = 1:3, pausa = 2)

knitr::kable(
  preparar_impresion(tabla_cafam, n = 10),
  caption = "Primeros resultados extraídos desde Cafam"
)

#-------------------------------------
# 9. Respuestas automáticas a las preguntas
#-------------------------------------
resumen_pagina <- tabla_cafam %>%
  group_by(pagina) %>%
  summarise(
    productos_extraidos        = n(),
    porcentaje_precio_visible  = mean(precio_visible, na.rm = TRUE) * 100,
    .groups = "drop"
  )

knitr::kable(resumen_pagina, caption = "Resumen por página")

resumen_global <- tabla_cafam %>%
  summarise(
    productos_totales          = n(),
    porcentaje_precio_visible  = mean(precio_visible, na.rm = TRUE) * 100
  )

resumen_global

#===========================================================
#  EJERCICIO 7.2 - DROGUERÍAS CAFAM
#  Selenium / RSelenium (Firefox)
#===========================================================

#-------------------------------------
# 10. Verificación previa y arranque de Selenium
#-------------------------------------
usar_selenium <- TRUE

rD    <- NULL
remDr <- NULL

if (usar_selenium) {
  if (Sys.which("java") == "") {
    warning("No se encontró Java en PATH. Se omitirá Selenium.")
  } else {
    rD <- tryCatch(
      rsDriver(
        browser   = "firefox",
        geckover  = "latest",
        chromever = NULL,
        phantomver = NULL,
        check     = FALSE,
        port      = 4545L
      ),
      error = function(e) {
        warning("No fue posible iniciar Firefox con Selenium: ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(rD)) {
      remDr <- rD[["client"]]
      
      ok_open <- tryCatch(
        { remDr$open(); TRUE },
        error = function(e) {
          warning("Selenium inició, pero no se pudo abrir la sesión del navegador: ", conditionMessage(e))
          FALSE
        }
      )
      
      if (!ok_open) remDr <- NULL
    }
  }
}

#-------------------------------------
# 11. Función para una página con Selenium
#-------------------------------------
scrapear_pagina_cafam_selenium <- function(termino, pagina = 1, pausa = 4) {
  if (is.null(remDr)) return(tibble())
  
  url <- construir_url_cafam(termino, pagina)
  
  remDr$navigate(url)
  Sys.sleep(pausa)
  
  pagina_html <- remDr$getPageSource()[[1]] %>%
    read_html(encoding = "UTF-8")
  
  nodos <- pagina_html %>%
    html_elements("article.product-miniature, .product-miniature, .js-product-miniature, div.product-miniature")
  
  if (length(nodos) == 0) nodos <- pagina_html %>% html_elements("div.product")
  if (length(nodos) == 0) return(tibble())
  
  map_dfr(nodos, ~extraer_producto_cafam(.x, pausa = 0.6)) %>%
    janitor::clean_names() %>%
    mutate(pagina = pagina, termino = termino) %>%
    mutate(across(where(is.character), limpiar_texto))
}

#-------------------------------------
# 12. Función para varias páginas con Selenium
#-------------------------------------
scrapear_cafam_selenium <- function(termino, paginas = 1:3, pausa = 4) {
  if (is.null(remDr)) return(tibble())
  
  map_dfr(
    paginas,
    ~scrapear_pagina_cafam_selenium(termino = termino, pagina = .x, pausa = pausa)
  ) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), limpiar_texto))
}

#-------------------------------------
# 13. Ejecución con Selenium
#-------------------------------------
tabla_cafam_selenium <- tibble()

if (!is.null(remDr)) {
  tabla_cafam_selenium <- scrapear_cafam_selenium("jabon", paginas = 1:3, pausa = 4)
  
  if (nrow(tabla_cafam_selenium) > 0) {
    knitr::kable(
      preparar_impresion(tabla_cafam_selenium, n = 10),
      caption = "Primeros resultados extraídos con Selenium"
    )
  } else {
    message("No se generó tabla con Selenium. Revise Firefox, Java o GeckoDriver.")
  }
}

#-------------------------------------
# 14. Resumen comparativo
#-------------------------------------
tabla_cafam %>%
  count(pagina, name = "productos_por_pagina")

tabla_cafam %>%
  summarise(
    n_rvest                    = n(),
    pct_precio_visible_rvest   = mean(precio_visible, na.rm = TRUE) * 100
  )

if (nrow(tabla_cafam_selenium) > 0) {
  tabla_cafam_selenium %>%
    summarise(
      n_selenium                    = n(),
      pct_precio_visible_selenium   = mean(precio_visible, na.rm = TRUE) * 100
    )
} else {
  tibble(
    n_selenium                  = 0,
    pct_precio_visible_selenium = NA_real_
  )
}

#-------------------------------------
# 15. Cierre seguro de Selenium
#-------------------------------------
try(remDr$close(), silent = TRUE)
try(if (!is.null(rD)) rD$server$stop(), silent = TRUE)

