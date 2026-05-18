
# Instalar paquetes 
paquetes <- c("rvest", "httr", "dplyr", "stringr", "tibble", "RSelenium",
              "netstat", "wdman")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


install.packages('RSelenium')
install.packages('wdman')
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tibble)
library(RSelenium)
library(wdman)


# Función para hacer scraping 

scraping_cafam_rvest <- function(termino_busqueda, max_paginas = 3) {
  
  cat(" Iniciando búsqueda con rvest:", termino_busqueda, "\n")
  
  # Codificar el término para URL
  termino_codificado <- URLencode(termino_busqueda, reserved = TRUE)
  
  resultados_totales <- list()
  
  for (pagina in 1:max_paginas) {
    
    cat("  → Página", pagina, "...\n")
    
    # Construir URL de búsqueda (parámetro 'q' y paginación 'page')
    url <- paste0(
      "https://www.drogueriascafam.com.co/search?q=",
      termino_codificado,
      "&page=",
      pagina
    )
    
    # Realizar la petición HTTP con cabeceras para simular un navegador
    respuesta <- tryCatch({
      GET(
        url,
        add_headers(
          `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0 Safari/537.36",
          `Accept-Language` = "es-CO,es;q=0.9",
          `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        ),
        timeout(30)
      )
    }, error = function(e) {
      cat("    Error en página", pagina, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (is.null(respuesta) || status_code(respuesta) != 200) {
      cat("   Página", pagina, "no disponible (código:", status_code(respuesta), ")\n")
      next
    }
    
    # Parsear el HTML
    pagina_html <- read_html(content(respuesta, as = "text", encoding = "UTF-8"))
    
    
    # Contenedor de cada tarjeta de producto
    tarjetas <- pagina_html |>
      html_elements(".product-item, .vtex-product-summary, article.vtex-product-summary-2-x-element,
                     .vtex-search-result-3-x-galleryItem, [class*='galleryItem'],
                     [class*='productSummary'], [class*='product-summary']")
    
    cat("  → Tarjetas encontradas:", length(tarjetas), "\n")
    
    if (length(tarjetas) == 0) {
      cat(" Sin tarjetas en página", pagina,
          "— el sitio puede requerir JavaScript (usar RSelenium)\n")
      next
    }
    
    # Extraer información de cada tarjeta
    for (tarjeta in tarjetas) {
      
      # Descripción / Nombre del producto
      descripcion <- tarjeta |>
        html_element("[class*='productBrand'], [class*='product-name'],
                      [class*='name'], .vtex-product-summary-2-x-productBrand,
                      h3, h2, .product-title") |>
        html_text2()
      
      # Marca
      marca <- tarjeta |>
        html_element("[class*='brandName'], [class*='brand'],
                      .vtex-product-summary-2-x-brandName") |>
        html_text2()
      
      # Precio (selector para precio de venta)
      precio_texto <- tarjeta |>
        html_element("[class*='sellingPrice'], [class*='selling-price'],
                      [class*='price'], .vtex-product-price-1-x-sellingPrice,
                      [class*='Price'], .price, .product-price") |>
        html_text2()
      
      # Limpiar y convertir precio
      precio_numerico <- NA_real_
      precio_disponible <- FALSE
      
      if (!is.na(precio_texto) && nchar(trimws(precio_texto)) > 0) {
        precio_limpio <- precio_texto |>
          str_remove_all("[^0-9,\\.]") |>
          str_replace_all("\\.", "") |>
          str_replace(",", ".")
        precio_numerico  <- suppressWarnings(as.numeric(precio_limpio))
        precio_disponible <- !is.na(precio_numerico)
      }
      
      # Agregar fila si hay al menos descripción
      if (!is.na(descripcion) && nchar(trimws(descripcion)) > 0) {
        resultados_totales <- append(resultados_totales, list(
          tibble(
            descripcion       = trimws(descripcion),
            marca             = ifelse(is.na(marca), "No disponible", trimws(marca)),
            precio            = precio_numerico,
            precio_disponible = precio_disponible,
            pagina            = pagina,
            metodo            = "rvest"
          )
        ))
      }
    }
    
    # Pausa respetuosa entre páginas
    Sys.sleep(runif(1, 1, 2))
  }
  
  if (length(resultados_totales) == 0) {
    cat("\n No se extrajeron productos. El sitio probablemente renderiza con JavaScript.\n")
    cat("   → Usa scraping_cafam_selenium() para obtener resultados reales.\n\n")
    return(tibble(
      descripcion = character(), marca = character(),
      precio = numeric(), precio_disponible = logical(),
      pagina = integer(), metodo = character()
    ))
  }
  
  df <- bind_rows(resultados_totales)
  cat("\n rvest completado:", nrow(df), "productos extraídos\n\n")
  return(df)
}



# Función para hacer scraping de Droguerías Cafam con RSelenium

scraping_cafam_selenium <- function(termino_busqueda,
                                    max_paginas = 3,
                                    headless    = TRUE) {
  
  library(RSelenium)
  
  cat(" Iniciando RSelenium para:", termino_busqueda, "\n")
  
  # Opciones de Chrome
  opciones_extra <- list(
    chromeOptions = list(
      args = c(
        if (headless) "--headless=new" else character(0),
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--disable-gpu",
        "--window-size=1366,768",
        "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0"
      )
    )
  )
  
  # Iniciar servidor y driver
  driver <- tryCatch({
     rsDriver(
      browser = "chrome",
      chromever = NULL,     
      phantomver = NULL,    
      port = 4567L,
      verbose = FALSE
    )
  }, error = function(e) {
    cat(" Error al iniciar RSelenium:", conditionMessage(e), "\n")
    cat("   Asegúrate de tener Chrome instalado y chromedriver disponible.\n")
    return(NULL)
  })
  
  if (is.null(driver)) return(NULL)
  
  navegador <- driver$client
  resultados_totales <- list()
  
  tryCatch({
    
    for (pagina in 1:max_paginas) {
      cat("  → Página", pagina, "...\n")
      
      termino_codificado <- URLencode(termino_busqueda, reserved = TRUE)
      url <- paste0(
        "https://www.drogueriascafam.com.co/search?q=",
        termino_codificado,
        "&page=", pagina
      )
      
      navegador$navigate(url)
      
      # Esperar a que cargue el contenido dinámico (VTEX tarda ~3-5 seg)
      Sys.sleep(5)
      
      # Scroll para activar lazy loading
      navegador$executeScript(
        "window.scrollTo(0, document.body.scrollHeight / 2);"
      )
      Sys.sleep(2)
      navegador$executeScript(
        "window.scrollTo(0, document.body.scrollHeight);"
      )
      Sys.sleep(2)
      
      # Obtener HTML renderizado
      html_fuente <- navegador$getPageSource()[[1]]
      pagina_html <- read_html(html_fuente)
      
      # Tarjetas de producto (VTEX usa clases con hash dinámico)
      tarjetas <- pagina_html |>
        html_elements(paste(
          "[class*='galleryItem']",
          "[class*='productSummary']",
          "[class*='product-summary']",
          "[class*='ProductSummary']",
          "article",
          sep = ", "
        ))
      
      # Filtrar solo tarjetas que contengan info de producto
      tarjetas <- tarjetas[
        sapply(tarjetas, function(t) {
          length(html_elements(t, "[class*='price'], [class*='Price'], [class*='name']")) > 0
        })
      ]
      
      cat("  → Tarjetas encontradas:", length(tarjetas), "\n")
      
      for (tarjeta in tarjetas) {
        
        descripcion <- tarjeta |>
          html_element(paste(
            "[class*='productBrand']", "[class*='brandName']",
            "[class*='product-name']", "[class*='name']",
            "h3", "h2", sep = ", "
          )) |>
          html_text2()
        
        marca <- tarjeta |>
          html_element(paste(
            "[class*='brand']", "[class*='Brand']",
            "[class*='seller']", sep = ", "
          )) |>
          html_text2()
        
        precio_texto <- tarjeta |>
          html_element(paste(
            "[class*='sellingPrice']", "[class*='selling-price']",
            "[class*='Price']", "[class*='price']",
            ".price", sep = ", "
          )) |>
          html_text2()
        
        precio_numerico  <- NA_real_
        precio_disponible <- FALSE
        
        if (!is.na(precio_texto) && nchar(trimws(precio_texto)) > 0) {
          precio_limpio <- precio_texto |>
            str_remove_all("[^0-9,\\.]") |>
            str_replace_all("\\.", "") |>
            str_replace(",", ".")
          precio_numerico   <- suppressWarnings(as.numeric(precio_limpio))
          precio_disponible <- !is.na(precio_numerico)
        }
        
        if (!is.na(descripcion) && nchar(trimws(descripcion)) > 0) {
          resultados_totales <- append(resultados_totales, list(
            tibble(
              descripcion       = trimws(descripcion),
              marca             = ifelse(is.na(marca), "No disponible", trimws(marca)),
              precio            = precio_numerico,
              precio_disponible = precio_disponible,
              pagina            = pagina,
              metodo            = "RSelenium"
            )
          ))
        }
      }
      
      Sys.sleep(runif(1, 1.5, 3))
    }
    
  }, error = function(e) {
    cat(" Error durante scraping:", conditionMessage(e), "\n")
  }, finally = {
    # Cerrar navegador siempre
    navegador$close()
    driver$server$stop()
    cat(" Navegador cerrado.\n")
  })
  
  if (length(resultados_totales) == 0) {
    cat("\n  No se extrajeron productos con RSelenium.\n")
    return(tibble(
      descripcion = character(), marca = character(),
      precio = numeric(), precio_disponible = logical(),
      pagina = integer(), metodo = character()
    ))
  }
  
  df <- bind_rows(resultados_totales)
  cat("\n RSelenium completado:", nrow(df), "productos extraídos\n\n")
  return(df)
}



# analisis ####

analizar_resultados <- function(df) {
  
  if (nrow(df) == 0) {
    cat("  El data frame está vacío. No hay nada que analizar.\n")
    return(invisible(NULL))
  }
  
  cat("  ANÁLISIS DE RESULTADOS DEL SCRAPING\n")

  
  cat(" PREGUNTA 1: ¿Cuántos productos se extrajeron por página?\n")
  por_pagina <- df |>
    group_by(pagina) |>
    summarise(
      productos = n(),
      con_precio = sum(precio_disponible),
      .groups = "drop"
    )
  print(por_pagina)
  cat("\nTotal general:", nrow(df), "productos en", n_distinct(df$pagina), "página(s)\n\n")
  
  
  cat(" PREGUNTA 2: ¿Qué porcentaje tiene precio visible?\n")
  pct_precio <- mean(df$precio_disponible) * 100
  cat(sprintf("  Con precio:    %d / %d productos (%.1f%%)\n",
              sum(df$precio_disponible), nrow(df), pct_precio))
  cat(sprintf("  Sin precio:    %d / %d productos (%.1f%%)\n\n",
              sum(!df$precio_disponible), nrow(df), 100 - pct_precio))
  
  cat("🔧 PREGUNTA 3: Selectores que dejaron de funcionar o hubo que ajustar\n")
  cat("
En el caso de Cafam, el sitio usa VTEX, que genera clases CSS con
nombres que tienen un hash al final.

El problema es que esos hash cambian, entonces si uno usa el selector exacto,
deja de funcionar cuando el sitio se actualiza.

Por eso varios selectores fallaron y tocó ajustarlos.

La idea fue usar [class*='...'], que busca clases que contengan ese texto
sin importar el hash. También se uso varios selectores como fallback.
\n")
  
  cat("⚖️  PREGUNTA 4: Diferencias entre rvest y RSelenium\n")
  cat("
La diferencia clave es cómo acceden a la página.

rvest hace peticiones HTTP directas, o sea, solo obtiene HTML estático.
Es muy rápido, pero no ejecuta JavaScript.

RSelenium en cambio usa un navegador real, entonces sí ejecuta
JavaScript y carga la página completa.

En la práctica:

- rvest: rápido y sencillo, pero no sirve bien para páginas dinámicas
- RSelenium: más lento y pesado, pero funciona con JavaScript

En el caso de Cafam, rvest no funciona porque los productos se cargan con
JavaScript (usan VTEX con React), entonces no devuelve resultados.

Con RSelenium sí pude obtener los productos correctamente.

En conclusión, para este caso RSelenium es la mejor opción, aunque sea más lento.
\n")
  
  
  
  # Resumen del dataframe
  cat(" MUESTRA DEL DATA FRAME RESULTANTE:\n")
  print(head(df, 10))
  
  invisible(df)
}



# EJECUCIÓN

# intentar con rvest 
df_selenium <- scraping_cafam_selenium("jabón", max_paginas = 2)


