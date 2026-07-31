"""
completar_abstracts.py
----------------------
Obtiene los abstracts faltantes de los artículos de 2026 usando la API
de Crossref, que los tiene para la mayoría de artículos MDPI.

Uso:
    python completar_abstracts.py

El script:
1. Identifica artículos sin abstract en la base SQLite.
2. Consulta Crossref por cada DOI.
3. Limpia las etiquetas JATS del abstract.
4. Actualiza la base de datos.
5. Muestra un resumen del proceso.
"""

import re
import time
import sqlite3
import requests

DB_PATH = "make_q1_2025.sqlite"
HEADERS = {
    "User-Agent": "MineriaDatos-Taller4/1.0 (mailto:estudiante@unal.edu.co)"
}
TIMEOUT = 20


def limpiar_abstract(texto: str) -> str:
    """Limpia etiquetas JATS y HTML del abstract de Crossref."""
    if not texto:
        return ""
    texto = re.sub(r"<[^>]+>", " ", texto)   # quitar etiquetas
    texto = re.sub(r"\s+", " ", texto).strip()
    return texto


def obtener_abstract_crossref(doi: str) -> str | None:
    """Consulta Crossref para obtener el abstract de un DOI."""
    url = f"https://api.crossref.org/works/{doi}"
    try:
        resp = requests.get(url, headers=HEADERS, timeout=TIMEOUT)
        if resp.status_code != 200:
            return None
        data = resp.json()
        abstract = data.get("message", {}).get("abstract", "")
        if abstract:
            return limpiar_abstract(abstract)
        return None
    except Exception:
        return None


def completar_abstracts(db_path: str = DB_PATH):
    """Proceso principal: busca y actualiza abstracts faltantes."""
    con = sqlite3.connect(db_path)
    cur = con.cursor()

    # Obtener artículos sin abstract que tengan DOI
    cur.execute("""
        SELECT paper_id, doi, title
        FROM papers
        WHERE (abstract IS NULL OR TRIM(abstract) = '')
        AND doi IS NOT NULL
        ORDER BY paper_id
    """)
    pendientes = cur.fetchall()
    total = len(pendientes)
    print(f"Artículos sin abstract con DOI: {total}")

    if total == 0:
        print("¡Todos los artículos ya tienen abstract!")
        con.close()
        return

    actualizados = 0
    no_encontrados = 0

    for i, (paper_id, doi, titulo) in enumerate(pendientes, 1):
        print(f"[{i}/{total}] {doi[:40]}...", end=" ", flush=True)

        abstract = obtener_abstract_crossref(doi)
        time.sleep(0.3)  # cortesía con Crossref

        if abstract and len(abstract) > 50:
            cur.execute(
                "UPDATE papers SET abstract = ? WHERE paper_id = ?",
                (abstract, paper_id)
            )
            con.commit()
            actualizados += 1
            print(f"✅ ({len(abstract)} chars)")
        else:
            no_encontrados += 1
            print("❌ no disponible")

    con.close()

    print()
    print("=" * 50)
    print(f"Proceso completado:")
    print(f"  Actualizados:     {actualizados}")
    print(f"  No encontrados:   {no_encontrados}")
    print(f"  Total procesados: {total}")

    # Verificar estado final
    con = sqlite3.connect(db_path)
    cur = con.cursor()
    cur.execute("SELECT COUNT(*) FROM papers WHERE abstract IS NULL OR TRIM(abstract) = ''")
    restantes = cur.fetchone()[0]
    cur.execute("SELECT COUNT(*) FROM papers")
    total_arts = cur.fetchone()[0]
    con.close()

    print(f"  Sin abstract ahora: {restantes} de {total_arts}")


if __name__ == "__main__":
    completar_abstracts()
