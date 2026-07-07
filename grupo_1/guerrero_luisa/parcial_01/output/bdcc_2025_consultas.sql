
-- 1. ¿Cuál es el número promedio de autores por paper?
SELECT 
    ROUND(AVG(n_authors), 2) AS promedio_autores_por_paper
FROM papers;

-- 2. ¿Cuántos artículos están relacionados con Machine Learning?
SELECT 
    COUNT(*) AS articulos_machine_learning
FROM papers
WHERE topic_label = 'Machine Learning';

-- 3. ¿Cuántos artículos están relacionados con IA Generativa?
SELECT 
    COUNT(*) AS articulos_ia_generativa
FROM papers
WHERE topic_label = 'IA Generativa';

-- 4. ¿Cuántos artículos están relacionados con otros temas estadísticos?
SELECT 
    COUNT(*) AS articulos_estadistica
FROM papers
WHERE topic_label = 'Estadística';

-- 5. ¿Cuál es el número total de descargas de los artículos publicados en 2025?
SELECT 
    SUM(downloads) AS total_descargas_2025
FROM papers
WHERE year = 2025;

-- 6. ¿Cuál es el número promedio de referencias por artículo?
SELECT 
    ROUND(AVG(n_references), 2) AS promedio_referencias_por_articulo
FROM papers;

-- 7. ¿Cuál es la referencia que más se repite entre todos los artículos?
SELECT 
    reference_text_normalized AS referencia,
    COUNT(*) AS veces_repetida
FROM references_table
WHERE reference_text_normalized IS NOT NULL
GROUP BY reference_text_normalized
ORDER BY veces_repetida DESC
LIMIT 1;

-- 8. ¿Cuál es el promedio de citas por artículo?
SELECT 
    ROUND(AVG(citations), 2) AS promedio_citas_por_articulo
FROM papers;

-- 9. ¿Cuál es el paper con más citas?
SELECT 
    title,
    doi,
    citations,
    topic_label
FROM papers
WHERE citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;

-- 10. ¿Cuál es el paper relacionado con Machine Learning, IA Generativa o Estadística con más citas?
SELECT 
    title,
    doi,
    citations,
    topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;

-- 11. ¿Cuál es el paper relacionado con Machine Learning, IA Generativa o Estadística con más descargas?
SELECT 
    title,
    doi,
    downloads,
    topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND downloads IS NOT NULL
ORDER BY downloads DESC
LIMIT 1;

