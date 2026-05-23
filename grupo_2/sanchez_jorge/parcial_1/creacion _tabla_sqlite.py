import pandas as pd
import numpy as np
import sqlite3
import os
   
#importamos los datos
ruta = r"C:\Users\jorge\OneDrive\Escritorio\Materias\Mineria\Taller1_Mineria\exel_webscraping.xlsx"
df_definitivo = pd.read_excel(ruta)
df_papers_final = df_definitivo.drop('references', axis=1)

# ruta
ruta_db = r"C:\Users\jorge\OneDrive\Escritorio\Materias\Mineria\Taller1_Mineria\sqlite_webscraping.db"

# borrar archivos cada vez que se ejecuta, para evitar problemas si se vuelve a ejecutar
if os.path.exists(ruta_db):
    try:
        os.remove(ruta_db)
    except PermissionError:
        print("ERROR: Cierra el programa que use la DB antes de correr el script.")

# conexion sql
conn = sqlite3.connect(ruta_db)
cursor = conn.cursor()

# Tabla Principal 
cursor.execute('''
    CREATE TABLE papers (
        paper_id INTEGER PRIMARY KEY,
        journal_name TEXT,
        title TEXT,
        publication_date DATE,
        year INTEGER,
        doi TEXT,
        url TEXT,
        abstract TEXT,
        authors_raw TEXT,
        n_authors INTEGER,
        citations INTEGER,
        views INTEGER,
        n_references INTEGER,
        topic_label TEXT
    )
''')

# Tabla Autores 
cursor.execute('''
    CREATE TABLE authors (
        author_id INTEGER PRIMARY KEY AUTOINCREMENT,
        author_name TEXT UNIQUE
    )
''')

# Tabla de Relación Paper-Autor 
cursor.execute('''
    CREATE TABLE paper_authors (
        paper_id INTEGER,
        author_id INTEGER,
        author_order INTEGER,
        FOREIGN KEY (paper_id) REFERENCES papers (paper_id) ON DELETE CASCADE,
        FOREIGN KEY (author_id) REFERENCES authors (author_id) ON DELETE CASCADE
    )
''')

# Tabla Referencias
cursor.execute('''
    CREATE TABLE references_table (
        reference_id INTEGER PRIMARY KEY AUTOINCREMENT,
        reference_text_normalized TEXT
    )
''')

# Tabla de Relación Paper-Referencia
cursor.execute('''
    CREATE TABLE paper_references (
        paper_id INTEGER,
        reference_id INTEGER,
        FOREIGN KEY (paper_id) REFERENCES papers (paper_id) ON DELETE CASCADE,
        FOREIGN KEY (reference_id) REFERENCES references_table (reference_id) ON DELETE CASCADE
    )
''')

conn.commit()

# ingresamos los datos a la tabla principal
df_papers_final.to_sql('papers', conn, if_exists='append', index=False)

#ingresamos los datos a las demas tablas sql, 
for i in range(len(df_papers_final)):

    #Autores
    p_id = df_papers_final['paper_id'].iloc[i]
    lista_autores = str(df_papers_final['authors_raw'].iloc[i]).split(',') # Separamos los nombres por coma
    
    for j in range(len(lista_autores)):
        nombre = lista_autores[j].strip()
        
        if nombre and nombre.lower() != "nan":
            # Insertar el autor en la tabla maestra
            cursor.execute('INSERT OR IGNORE INTO authors (author_name) VALUES (?)', (nombre,))
            # Obtener el ID que se le asignó
            cursor.execute('SELECT author_id FROM authors WHERE author_name = ?', (nombre,))
            a_id = cursor.fetchone()[0]
            # Crear el vínculo en la tabla intermedia
            cursor.execute('''
                INSERT INTO paper_authors (paper_id, author_id, author_order) 
                VALUES (?, ?, ?)
            ''', (p_id, a_id, j + 1))

    # Referencias
    lista_refs = str(df_definitivo['references'].iloc[i]) .split(' ; ')  # Usamos el separador ' ; '
    
    for k in range(len(lista_refs)):
        ref = lista_refs[k].strip()
        if ref and ref.lower() != "nan":
            # Guardar en tabla de referencias
            cursor.execute('INSERT OR IGNORE INTO references_table (reference_text_normalized) VALUES (?)', (ref,))
            # Recuperar el ID recién generado 
            r_id = cursor.lastrowid
            # Conectar con el paper
            cursor.execute('''
                INSERT INTO paper_references (paper_id, reference_id) 
                VALUES (?, ?)
            ''', (p_id, r_id))

conn.commit()
conn.close()

