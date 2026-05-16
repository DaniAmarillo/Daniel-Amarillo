paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr","openalexR",
  "purrr", "tibble", "janitor", "readr", "knitr", "tidyr","RSQLite"
)

# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)

#URLs DE JOURNALS IMPRESOS
issues_url <- c("https://www.akjournals.com/view/journals/2006/14/1/2006.14.issue-1.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/2/2006.14.issue-2.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/3/2006.14.issue-3.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/4/2006.14.issue-4.xml"
                ,"https://www.akjournals.com/view/journals/2006/15/1/2006.15.issue-1.xml")

crear_nodo <- function(url){
# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal

respuesta <- request(url) |>
  req_user_agent(user_agent_navegador) |>
  req_headers(`accept-language` = "en-US,en;q=0.9") |>
  req_perform()

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
nodo <- respuesta |>
  resp_body_html()
nodo
}
extraer_general <- function(nodo) {
  # Extraemos el títulos de los articulos
  titulo <- nodo |>
    html_elements("[data-testid='block-primitivetitle']") |>
    html_text2()
  
  issue <- titulo[length(titulo)]
  titulo <- titulo[-length(titulo)]
    # Extraemos el enlace relativo doi del articulo
  doi <- nodo |> 
    html_elements( " a,[target='_blank'], .c-Button--link") |>
    html_text()
  #filtramos por links que contengan https doi 
  doi <- doi[grep(pattern="https://doi*",doi)]
  #Creamos una tabla con la información general de cada journal 
  tibble(
    titulo = titulo,
    doi = doi,
    issue = issue
  )
}

papers <- data.frame()
for (i in issues_url) {
  nodo = crear_nodo(i)
  df <- extraer_general(nodo)
  papers <- rbind(papers,df)
}


extraer_paper <- function(url){
 
  nodo <- crear_nodo(url)
 #EXTRACCIÓN DOI (LLAVE EN COMUN CON GENERALITIES)
doi <- nodo |> 
  html_elements( " a,[target='_blank'], .c-Button--link") |>
  html_text()
doi <- doi[grep(pattern="https://doi*",doi)][1]

 #AUTORES 
autores_ex <- nodo |> 
  html_elements("[data-testid='author-name']") |>
  html_text()
autores <- paste(autores_ex,collapse = ",")
 #ABSTRACT
 raw_abstract <- nodo |> html_elements(".abstract.border-bottom.border-bottom-solid.border-bottom-medium") |>
   html_text2()
 a <- unlist(strsplit(raw_abstract, split = "\n"))
 Backround_and_aims <- NA
 Methods <- NA
 Results <- NA
 Conclusion <- NA
 abstract <- NA
 if (length(a)<4) {
   abstract <- a[3] 
 }else {
   Backround_and_aims <- a[4]
   Methods <- a[8]
   Results <- a[12]
   Conclusion <- a[16]
 }

 #FECHA DE PUBLICACIÓN
 fecha <- nodo |> 
   html_elements(".onlinepubdate.c-List__items") |>
   html_text2() |>
   str_squish() |>
   str_remove("^[^0-9]+")
 
 #Métrica
 fwci <- oa_fetch(entity = "works", doi = doi)$fwci
 
 #URL 
 URL <- nodo |> 
   html_elements('[rel="canonical"], href')|>
   html_attr("href")
 
 #REFERENCIAS
 ref_ex <- NA
 ref_ex <- nodo |> 
   html_elements(".citationText.text-body1") |>
   html_text()
 
 
 # CITAS
 dyn_cit = read_html_live(URL)
 try(dyn_cit$click("[data-tab-id='citedBy-30734']"),silent = TRUE)
 n_citaciones <- NA
 intentos <- 0
 while (intentos < 10) {
   Sys.sleep(3)
   p <- dyn_cit |> 
     html_elements("#citedByWidget") |>
     html_children()
   
   texto <- p[2] |> html_text() |> str_squish()
   
   if (length(texto) > 0 && nchar(texto) > 0) {
     n_citaciones <- word(texto, -2)
     break
   }
   intentos <- intentos + 1
 }


  paper_row <- tibble (doi = doi,
                      url = URL,
                      fecha_publicacion = fecha,
                      nro_de_citas = n_citaciones,
                      fwci = fwci)

 abstract_row <- tibble( doi = doi,
         resumen = abstract,
         Backround = Backround_and_aims,
         Metodos = Methods,
         Resultados = Results,
         Conclusion = Conclusion
         )
 autores_row <- tibble( doi = doi,
                        autores = list(autores_ex) )
 ref_row <- tibble(doi = doi,
                   list(ref_ex))
 
 dyn_cit$session$close()
 

 return(list(paper_row,abstract_row,autores_row,ref_row))
}
#INICIALIZACIÓN DE TABLAS DE EXTRACCIÓN 
abstract <- tibble()
autores <- tibble()
referencias <- tibble()
comp_papers <- tibble()

 pb <- txtProgressBar(min = 1, max = length(papers$doi), style = 3)
 it  = 0
 
 #PRECAUCIÓN TIEMPO APROXIMADO DE EJECUCIÓN 40 mins a 1 hora 
  for (i in papers$doi){
    tibbles <- extraer_paper(i)
    comp_papers <- comp_papers |> bind_rows(tibbles[1])  
    abstract <- abstract |> bind_rows(tibbles[2]) 
    autores <- autores |> bind_rows(tibbles[3]) 
    referencias <- referencias |> bind_rows(tibbles[4]) 
    it= it +1 
    setTxtProgressBar(pb, it)
  }


 

# Ordenamos y separamos las tablas de referencias y autores
paper_references <- referencias |> tidyr::unnest(`list(ref_ex)`) |> rename(referencia = `list(ref_ex)`)
# Creación de Ids de autores
paper_references$id_referencia <- as.numeric(as.factor(paper_references$referencia))
references <- paper_references[c("id_referencia","referencia")] |> unique() |> arrange(id_referencia)
paper_references <- paper_references |> select(-referencia) |> unique()


paper_authors <- autores |> tidyr::unnest(autores) 
# Creación de Ids de referencias
paper_authors$id_autor <- as.numeric(as.factor(paper_authors$autores)) 
authors <- paper_authors[c("id_autor","autores")] |> unique() |> arrange(id_autor)
paper_authors <- paper_authors |> select(-autores) |> unique()

#REMPLAZO DE CITAS EN NA POR 0 (Explicación en el informe)
comp_papers <- comp_papers |> mutate(nro_de_citas = coalesce(as.numeric(nro_de_citas), 0))

#Creación variables en la tabla principal 
papers["journal_name"] <- "Journal of Behavioral Addictions" 
papers <- papers |> full_join(comp_papers,by="doi")
papers <- papers |> full_join(paper_authors |>
                                     group_by(doi) |>
                                     summarise(n_autores = n()),by="doi")

papers <- papers |> full_join(paper_references |>
                                     group_by(doi) |>
                                     summarise(n_referencia = n()),by="doi")

#NORMALIZACIÓN DE CARACTERES
abstract <- abstract |>
  mutate(across(-doi, tolower))

#ETIQUETADO DE TEMAS 
abstract <- abstract |> 
    mutate(topic_label = case_when(
    if_any(-doi , ~ str_detect(., "regression|inference|variance|covariance|statistic|sample|survey|random")) ~ "Statistic" ,
    if_any(-doi , ~ str_detect(., "artificial intelligence| llm |gpt|nlp|agents|neuronal networks|chatbot|deep learning|transformers| ai ")) ~ "AI",
    if_any(-doi , ~ str_detect(., "machine learning|clustering|knn|overfitting")) ~ "Machine Learning",
    TRUE ~ "Other"
  ))

#ANEXION DE LA COLUMNA  ETIQUETA Y CREACIÓN DE LA COLUMNA AÑO
 papers <- papers |> full_join(abstract[c("doi","topic_label")],by="doi")
 papers <- papers |> mutate( year = str_sub(fecha_publicacion, -4, -1))
 
 # CREACIÓN LA BASE DE DATOS 
 con <- dbConnect(RSQLite::SQLite(), "JBA_25_26.sqlite")
 dbWriteTable(con, "papers", papers)
 dbWriteTable(con, "paper_authors", paper_authors)
 dbWriteTable(con, "authors", authors)
 dbWriteTable(con, "paper_references", paper_references)
 dbWriteTable(con, "references", references)
 dbWriteTable(con, "abstract", abstract)
 dbDisconnect(con)
 
 
