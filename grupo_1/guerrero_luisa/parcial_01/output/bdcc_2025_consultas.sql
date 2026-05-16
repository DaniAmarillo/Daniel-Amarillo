
-- 1. Número promedio de autores por paper
SELECT 
    ROUND(AVG(n_authors), 2) AS promedio_autores_por_paper
FROM papers;

-- 2. Cantidad de artículos relacionados con Machine Learning
SELECT 
    COUNT(*) AS articulos_machine_learning
FROM papers
WHERE topic_label = 'Machine Learning';

-- 3. Cantidad de artículos relacionados con IA Generativa
SELECT 
    COUNT(*) AS articulos_ia_generativa
FROM papers
WHERE topic_label = 'IA Generativa';

-- 4. Cantidad de artículos relacionados con Estadística
SELECT 
    COUNT(*) AS articulos_estadistica
FROM papers
WHERE topic_label = 'Estadística';

-- 5. Número total de descargas de artículos publicados en 2025
SELECT 
    SUM(downloads) AS total_descargas_2025
FROM papers
WHERE year = 2025;

-- 6. Número promedio de referencias por artículo
SELECT 
    ROUND(AVG(n_references), 2) AS promedio_referencias_por_articulo
FROM papers;

-- 7. Referencia que más se repite entre todos los artículos
SELECT 
    rt.reference_text_normalized AS referencia,
    COUNT(pr.paper_id) AS veces_repetida
FROM references_table rt
JOIN paper_references pr 
    ON rt.reference_id = pr.reference_id
GROUP BY rt.reference_id, rt.reference_text_normalized
ORDER BY veces_repetida DESC
LIMIT 1;

-- 8. Promedio de citas por artículo
SELECT 
    ROUND(AVG(citations), 2) AS promedio_citas_por_articulo
FROM papers;

-- 9. Paper con más citas
SELECT 
    title,
    doi,
    citations,
    topic_label
FROM papers
WHERE citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;

-- 10. Paper relacionado con Machine Learning, IA Generativa o Estadística con más citas
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

-- 11. Paper relacionado con Machine Learning, IA Generativa o Estadística con más descargas
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

