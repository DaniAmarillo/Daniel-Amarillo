import sqlite3
import os 
import urllib.request
import pandas as pd

DB_PATH="Chinook_Sqlite.sqlite"
DB_URL= (
     "https://github.com/lerocha/chinook-database/raw/master/"
    "ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
)

def descargar_db():
    if not os.path.exists(DB_PATH):
        print("Descargando base de datos Chinook...")
        urllib.request.urlretrieve(DB_URL, DB_PATH)
        print("Descarga completada. \n")

def ejecutar(conn, titulo, query):
    print("=" * 70)
    print(titulo)
    print("=" * 70)
    df = pd.read_sql_query(query, conn)
    print(df.to_string(index=False))
    print(f"\nFilas: {len(df)}\n")
    return df

def main():
    descargar_db()
    conn = sqlite3.connect(DB_PATH)
    q1 = """
    SELECT
        c.FirstName || ' ' || c.LastName AS Cliente,
        c.Country                        AS Pais,
        t.Name                           AS Pista,
        g.Name                           AS Genero,
        il.UnitPrice                     AS PrecioUnitario
    FROM Customer       c
    INNER JOIN Invoice      i  ON i.CustomerId = c.CustomerId
    INNER JOIN InvoiceLine  il ON il.InvoiceId = i.InvoiceId
    INNER JOIN Track        t  ON t.TrackId    = il.TrackId
    INNER JOIN Genre        g  ON g.GenreId    = t.GenreId
    LIMIT 15;
    """
    ejecutar(conn, "1) INNER JOIN - Ventas (15 filas)", q1)
    q2 = """
    SELECT
        t.TrackId,
        t.Name       AS Pista,
        a.Title      AS Album,
        ar.Name      AS Artista
    FROM Track            t
    LEFT JOIN InvoiceLine il ON il.TrackId  = t.TrackId
    LEFT JOIN Album       a  ON a.AlbumId   = t.AlbumId
    LEFT JOIN Artist      ar ON ar.ArtistId = a.ArtistId
    WHERE il.InvoiceLineId IS NULL
    ORDER BY ar.Name, t.Name
    LIMIT 20;   -- quita el LIMIT para ver todas
    """
    ejecutar(conn, "2) LEFT JOIN - Pistas nunca vendidas (primeras 20)", q2)
    q3 = """
    WITH IngresosPorArtista AS (
        SELECT
            ar.ArtistId,
            ar.Name                             AS Artista,
            SUM(il.UnitPrice * il.Quantity)     AS Ingresos
        FROM InvoiceLine il
        JOIN Track  t  ON t.TrackId    = il.TrackId
        JOIN Album  a  ON a.AlbumId    = t.AlbumId
        JOIN Artist ar ON ar.ArtistId  = a.ArtistId
        GROUP BY ar.ArtistId, ar.Name
    )
    SELECT Artista, ROUND(Ingresos, 2) AS Ingresos
    FROM IngresosPorArtista
    ORDER BY Ingresos DESC
    LIMIT 10;
    """
    ejecutar(conn, "3) CTE - Top 10 artistas por ingresos", q3)
    q4 = """
    WITH IngresosPorArtista AS (
        SELECT
            ar.ArtistId,
            ar.Name                             AS Artista,
            SUM(il.UnitPrice * il.Quantity)     AS Ingresos
        FROM InvoiceLine il
        JOIN Track  t  ON t.TrackId    = il.TrackId
        JOIN Album  a  ON a.AlbumId    = t.AlbumId
        JOIN Artist ar ON ar.ArtistId  = a.ArtistId
        GROUP BY ar.ArtistId, ar.Name
    ),
    TotalGeneral AS (
        SELECT SUM(Ingresos) AS Total FROM IngresosPorArtista
    )
    SELECT
        ipa.Artista,
        ROUND(ipa.Ingresos, 2)                            AS Ingresos,
        ROUND(100.0 * ipa.Ingresos / tg.Total, 2)         AS PorcentajeParticipacion,
        RANK() OVER (ORDER BY ipa.Ingresos DESC)          AS Ranking
    FROM IngresosPorArtista ipa
    CROSS JOIN TotalGeneral tg
    ORDER BY Ranking
    LIMIT 15;
    """
    ejecutar(conn, "4) CTE extendida - % participación + RANK()", q4)

    conn.close()


if __name__ == "__main__":
    main()
