library(rvest)

# =============================================================================
# GS.Citations()
# Consulta Google Scholar para obtener el número de citas de un artículo
# dado su DOI. Reintenta hasta 5 veces ante errores de conexión con espera
# creciente. Retorna un data.frame con: DOI y citations.
# =============================================================================
GS.Citations <- function(doi, max.tries = 5){
  
  url <- paste0('https://scholar.google.com/scholar?hl=es&as_sdt=0%2C5&q=', doi, '&btnG=')
  
  for (attempt in 1:max.tries){
    
    result <- tryCatch({
      
      citas <- read_html(url) |>
        html_elements('.gs_fl a') |>
        html_text2() |>
        (\(x) x[stringr::str_detect(x, 'Citado por')])() |>
        stringr::str_remove_all('Citado por ') |>
        as.numeric()
      
      data.frame(
        DOI       = doi,
        citations = if (length(citas) == 0) NA else citas
      )
      
    }, error = \(e){
      message(sprintf('\nIntento %d/%d fallido para: %s', attempt, max.tries, doi))
      Sys.sleep(attempt * 2)
      NULL
    })
    
    if (!is.null(result)) return(result)
  }
  
  message(sprintf('\nNo se pudo recuperar: %s', doi))
  data.frame(DOI = doi, citations = NA)
}