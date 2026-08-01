source('scrapping.R')
source('sql build.R')

# ---------------------------------------- PREGUNTAS TALLER #1 -------------------------------------
# Una vez se corren los dos scripts, la base de datos debería quedar disponible dentro del directorio
# de trabajo con el nombre JSS.sqlite. 

library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), "JSS.sqlite")

# Autor: Michel Mendivenson Barragán Zabala mbarraganz@unal.edu.co
# Asignatura: Minería de datos. 2026-1 
# --------------------------------------------------------------------------------------------------

# 1. ¿CUÁL ES EL NÚMERO PROMEDIO DE AUTORES POR PAPER? 
#    Debido a como se organizo la tabla de autores y autoría, simplemente con agrupar por DOI en la
#    tabla authorship y contar la cantidad de elementos. Además de hacer left joins para identificar
#    de qué año son los conteos.

avg_authors <- dbGetQuery(con, "
  SELECT 
      v.year,
      AVG(n_authors) AS avg_authors_per_paper
  FROM (
      SELECT 
          a.DOI,
          a.vol_id,
          COUNT(au.ORCID) AS n_authors
      FROM      articles   a
      LEFT JOIN authorship au ON a.DOI = au.DOI
      GROUP BY  a.DOI, a.vol_id
  ) sub
  LEFT JOIN vol v ON sub.vol_id = v.id
  WHERE v.year != 2019
  GROUP BY v.year
  ORDER BY v.year
")
avg_authors

# 2. ¿CUÁNTOS ARTÍCULOS ESTÁN RELACIONADOS CON MACHINE LEARNING?

ml_count <- dbGetQuery(con, "
  SELECT
      v.year,
      COUNT(*) AS n_articles
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  WHERE a.topic = 'Machine Learning'
  GROUP BY v.year
  ORDER BY v.year
")
ml_count

# 3. ¿CUÁNTOS ARTÍCULOS ESTÁN RELACIONADOS CON IA GENERATIVA?

gen_count <- dbGetQuery(con, "
  SELECT
      v.year,
      COUNT(*) AS n_articles
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  WHERE a.topic = 'Generative AI'
  GROUP BY v.year
  ORDER BY v.year
")
gen_count

# 4. ¿CUÁNTOS ARTÍCULOS ESTÁN RELACIONADOS CON OTROS TEMAS ESTADÍSTICOS?

sta_count <- dbGetQuery(con, "
  SELECT
      v.year,
      COUNT(*) AS n_articles
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  WHERE a.topic = 'Statistics'
  GROUP BY v.year
  ORDER BY v.year
")
sta_count

# 5. ¿CUÁL ES EL NÚMERO TOTAL DE DESCARGAS DE LOS ARTÍCULOS PUBLICADOS EN 2025?
infl_cites <- dbGetQuery(con, "
  SELECT  year, SUM(influential_citations)
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  GROUP BY year
")
infl_cites

# 6. ¿CUÁL ES EL NÚMERO PROMEDIO DE REFERENCIAS POR ARTÍCULO?
avg_references <- dbGetQuery(con, "
  SELECT
      v.year,
      AVG(a.n_references) AS avg_references_per_paper
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  WHERE v.year != 2019
  GROUP BY v.year
  ORDER BY v.year
")
avg_references

# 7. ¿CUÁL ES LA REFERENCIA QUE MÁS SE REPITE ENTRE TODOS LOS ARTÍCULOS?
#    Se cuenta cuántas veces aparece cada DOI_reference en la tabla ref y se toma el máximo.
top_reference <- dbGetQuery(con, "
  SELECT year, reference, citations
  FROM (
    SELECT 
        year,
        reference,
        citations,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY citations DESC) AS rn
    FROM (
        SELECT 
            year,
            reference, 
            COUNT(*) AS citations
        FROM (
            SELECT 
                v.year,
                a.vol_id, a.DOI, 
                r.DOI_reference AS reference
            FROM articles a
            LEFT JOIN vol v ON a.vol_id = v.id
            RIGHT JOIN ref r ON r.DOI_origin = a.DOI
            WHERE r.DOI_reference IS NOT NULL
              AND r.DOI_reference != '')
        GROUP BY year, reference
    )
  )
  WHERE rn = 1
  ORDER BY year
")
top_reference

# 8. ¿CUÁL ES EL PROMEDIO DE CITAS POR ARTÍCULO?
avg_citations <- dbGetQuery(con, "
  SELECT
      v.year,
      AVG(a.n_citations) AS avg_citations
  FROM      articles a
  LEFT JOIN vol      v ON a.vol_id = v.id
  GROUP BY v.year
  ORDER BY v.year
")
avg_citations

# 9. ¿CUÁL ES EL PAPER CON MÁS CITAS?
top_cited_by_year <- dbGetQuery(con, "
  SELECT v.year, a.title, a.n_citations
  FROM articles a
  LEFT JOIN vol v ON a.vol_id = v.id
  WHERE a.n_citations = (
      SELECT MAX(a2.n_citations)
      FROM articles a2
      LEFT JOIN vol v2 ON a2.vol_id = v2.id
      WHERE v2.year = v.year
  )
  ORDER BY v.year
")
top_cited_by_year

# 10. ¿CUÁL ES EL PAPER RELACIONADO CON MACHINE LEARNING, IA GENERATIVA O ESTADÍSTICA
#     CON MÁS CITAS?
top_cited_topic <- dbGetQuery(con, "
  SELECT v.year, a.topic, a.title,  a.n_citations 
  FROM articles a
  LEFT JOIN vol v ON a.vol_id = v.id
  WHERE a.topic IN ('Machine Learning', 'Generative AI', 'Statistics')
    AND a.n_citations = (
        SELECT MAX(a2.n_citations)
        FROM articles a2
        LEFT JOIN vol v2 ON a2.vol_id = v2.id
        WHERE v2.year = v.year
          AND a2.topic = a.topic
    )
  ORDER BY v.year, a.topic, a.n_citations
")
top_cited_topic

# 11. ¿CUÁL ES EL PAPER RELACIONADO CON MACHINE LEARNING, IA GENERATIVA O ESTADÍSTICA
#     CON MÁS DESCARGAS?
top_downloaded_topic <- dbGetQuery(con, "
  SELECT v.year, a.topic, a.title,  a.influential_citations 
  FROM articles a
  LEFT JOIN vol v ON a.vol_id = v.id
  WHERE a.topic IN ('Machine Learning', 'Generative AI', 'Statistics')
    AND a.influential_citations = (
        SELECT MAX(a2.influential_citations)
        FROM articles a2
        LEFT JOIN vol v2 ON a2.vol_id = v2.id
        WHERE v2.year = v.year
          AND a2.topic = a.topic
    )
  ORDER BY v.year, a.topic, a.influential_citations
")
top_downloaded_topic
