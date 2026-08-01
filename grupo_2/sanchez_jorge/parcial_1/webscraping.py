
from bs4 import BeautifulSoup
import pandas as pd
import time
import undetected_chromedriver as uc # es un selemiun mejor xd

def clasificar_tematica(resumen):
    """
    Funcion para clasificar el topico del articulo dependiendo de las palabras del resumen
    """
    texto = str(resumen).lower() # convertimos a minuscula
    
    # Criterios de búsqueda por palabras clave
    if any(palabra in texto for palabra in ['machine learning', 'aprendizaje automático', 'neural network', 'deep learning']):
        return "Machine Learning"
    elif any(palabra in texto for palabra in ['generative ai', 'ia generativa', 'gpt', 'llm', 'large language model', 'diffusion']):
        return "IA Generativa"
    elif any(palabra in texto for palabra in ['statistics', 'estadística', 'regression', 'probability', 'p-value', 'inference']):
        return "Estadística"
    else:
        return "Otros"
    
#Definimos un user-agent
headers = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/122.0.0.0 Safari/537.36"
    ),
    "accept-language": "en-US,en;q=0.9"
}

#evitar que las páginas web detecten que estoy usando un bot
options = uc.ChromeOptions()
options.add_argument('--no-first-run')
options.add_argument('--no-service-autorun')
options.add_argument('--password-store=basic')

driver = uc.Chrome(options=options, version_main=147)

urls = [f"https://onlinelibrary.wiley.com/action/doSearch?AfterYear=2025&BeforeYear=2025&SeriesKey=15406261&content=articlesChapters&sortBy=Earliest&startPage={i}&target=default&pageSize=20" for i in range(6)]

def art(url):
    driver.get(url)
    time.sleep(10)
    pagina_finanzas = BeautifulSoup(driver.page_source, "html.parser")
    titulos = [i.get_text(strip=True) for i in pagina_finanzas.select("a.publication_title ")]
    fecha = [i.get_text(strip=True).replace("First published:", "").strip() for i in pagina_finanzas.select(".meta__epubDate")]
    
    dois  = [i.get("value") for i in pagina_finanzas.select("div.bulkDownloadInput input")]
    links = [f"https://onlinelibrary.wiley.com{i['href']}" for i in pagina_finanzas.select("a.publication_title")]
    autores = [", ".join(i.get_text(strip=True).strip(",").split(",")) for i in pagina_finanzas.select(".meta__authors")]

    df = pd.DataFrame({
    "title": titulos,
    "publication_date": fecha,
    "doi": dois,
    "url": links,
    "authors_raw": autores
    })

    return df

df_total = pd.concat([art(urls[0]), art(urls[1]), art(urls[2]), art(urls[3]), art(urls[4]), art(urls[5])], ignore_index=True) 

# limapiamos los data frames de articulos basura (ISSUE INFORMATION, AMERICAN FINANCE ASSOCIATION y ANNOUNCEMENTS)
# Ademas limpiamos articulos que son bibiografias o reportes que no aportan nada.
lista = ["ISSUE INFORMATION", "AMERICAN FINANCE ASSOCIATION", "ANNOUNCEMENTS", "Tyler Muir: Winner of the 2025 Fischer Black Prize", "Steven N. Kaplan", 
         "Report of the EST and of the 2025 Annual Membership Meeting", "BRATTLE GROUP AND DIMENSIONAL FUND ADVISORS PRIZES FOR 2024", "Report of the Editor ofThe Journal of Financefor the Year 2024"]
df_limpio = df_total[~df_total['title'].isin(lista)]

# Convertir la columna fecha a formato datetime de Python/Pandas
df_limpio['publication_date'] = pd.to_datetime(df_limpio['publication_date'], errors='coerce').dt.date

# Ahora vamos por el resumen, referencias, el numero de citas y las visitas
#Para ello, necesitamos ingresar a cada uno de los links y obtener los datos

urls2 = [i for i in df_limpio["url"]]

def art2(url):
    driver.get(url)
    time.sleep(1)
    pagina_finanzas = BeautifulSoup(driver.page_source, "html.parser")
    
    resumenes = [i.get_text(strip=True) for i in pagina_finanzas.select(".article-section__content.en.main")]
    resumen_final = resumenes[0] if resumenes else "" # Tomamos solo el texto, no necesitamos el titulo 
    
    referencias = [", ".join(i.get_text(strip=True).strip(",").split(",")) for i in pagina_finanzas.select("ul.rlist.separator li")]
    # Unimos las referencias con un separador (punto y coma) 
    referencias_final = " ; ".join(referencias)
    
    citas = [i.get_text(strip=True) for i in pagina_finanzas.select(".cited-by-count span")]
    vistas = [i.get_text(strip=True) for i in pagina_finanzas.select(".number-of-downloads span")]
    
    citas2 = citas[1] if len(citas) > 1 else 0
    vistas_final = vistas[0] if vistas else 0
    
    df = pd.DataFrame({
        "abstract": [resumen_final],
        "references": [referencias_final],
        "citations": [int(citas2)],
        "views": [int(vistas_final)]
    })
    return df

#Resetear el índice de df_limpio para que coincida con df_nuevo y evitar porblemas en la creacion del archivo exel
df_limpio = df_limpio.reset_index(drop=True)

df_nuevo = pd.DataFrame()
for i in urls2:
    a = art2(i)
    df_nuevo = pd.concat([df_nuevo, a], ignore_index=True) 

df_definitivo = pd.concat([df_limpio, df_nuevo], axis=1)

# Creamos columnas que faltan
df_definitivo['paper_id'] = range(1, len(df_definitivo) + 1) # columna de identificacion
df_definitivo['journal_name'] = "Journal of Finance" # Agregamos columna con el nombre del journal
df_definitivo['year'] = [2025 for i in range(len(df_definitivo))] # columna año
df_definitivo['n_authors'] = [0 for i in range(len(df_definitivo))]
df_definitivo['n_references'] = [0 for i in range(len(df_definitivo))]
df_definitivo['topic_label'] = ["hola" for i in range(len(df_definitivo))]
#para calular el numero de autores, y referencias
for i in range(len(df_definitivo)):
    df_definitivo.loc[i, 'n_authors'] = len(df_definitivo.loc[i, 'authors_raw'].split(","))
    df_definitivo.loc[i, 'n_references'] = len(str(df_definitivo.loc[i, 'references']).split(";"))
    df_definitivo.loc[i, 'topic_label'] = clasificar_tematica(df_definitivo.loc[i, 'abstract'])

# ordenamos las columnas para que se vea como lo quiere el profe
columnas_papers = [
    'paper_id', 'journal_name', 'title', 'publication_date', 'year', 
    'doi', 'url', 'abstract', 'authors_raw', 'n_authors', 
    'citations', 'views', 'n_references', 'topic_label', 'references'
]
df_definitivo = df_definitivo[columnas_papers]

ruta_destino = r"C:\Users\jorge\OneDrive\Escritorio\Materias\Mineria\Taller1_Mineria\exel_webscraping.xlsx"
# Guardar
df_definitivo.to_excel(ruta_destino, index=False)

