import sqlite3
import pandas as pd

# ruta  donde esta el archivo sql
ruta_db = r"C:\Users\jorge\OneDrive\Escritorio\Materias\Mineria\Taller1_Mineria\sqlite_webscraping.db"
con = sqlite3.connect(ruta_db)

# Promedio de autores 
resultado1 = pd.read_sql_query("""
    SELECT ROUND(AVG(n_authors), 2) AS promedio_autores 
    FROM papers
""", con)
print("\n", resultado1)

# Conteo por categorías 
resultado2 = pd.read_sql_query("""
    SELECT topic_label AS categoria, 
           COUNT(*) AS total_articulos
    FROM papers
    GROUP BY topic_label
    ORDER BY total_articulos DESC;
""", con)
print("\n", resultado2)

# Total views 2025 
resultado3 = pd.read_sql_query("""
    SELECT SUM(views) AS total_views_2025
    FROM papers
""", con)
print("\n", resultado3)

# Promedio de referencias
resultado4 = pd.read_sql_query("""
    SELECT ROUND(AVG(n_references), 2) AS promedio_referencias 
    FROM papers
""", con)
print("\n", resultado4)

# Referencia más citada 
resultado5 = pd.read_sql_query("""
    SELECT r.reference_text_normalized AS referencia, 
           COUNT(pr.reference_id) AS veces_citada
    FROM paper_references pr
    JOIN references_table r ON pr.reference_id = r.reference_id
    GROUP BY r.reference_id
    ORDER BY veces_citada DESC
    LIMIT 1;
""", con)
print("\n", resultado5)

# Promedio de referencias
resultado6 = pd.read_sql_query("""
    SELECT ROUND(AVG(citations), 2) AS promedio_referencias 
    FROM papers
""", con)
print("\n", resultado6)

# Paper con mas citas
resultado7 = pd.read_sql_query("""
    SELECT title AS titulo,
           citations AS referencias
    FROM papers
    ORDER BY citations DESC
    LIMIT 1;
    """, con)
print("\n", resultado7)

# Top Citas en categorías específicas
resultado8 = pd.read_sql_query("""
    SELECT title AS titulo, 
           topic_label AS categoria, 
           citations AS referencias
    FROM papers
    WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
    ORDER BY citations DESC
    LIMIT 1;
""", con)
print("\n", resultado8)

# Top views en categorías específicas
resultado9 = pd.read_sql_query("""
    SELECT title AS titulo, 
           topic_label AS categoria, 
           views AS views
    FROM papers
    WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
    ORDER BY views DESC
    LIMIT 1;
""", con)
print("\n", resultado9)

con.close()