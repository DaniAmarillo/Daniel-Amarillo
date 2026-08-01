import duckdb
import pandas as pd


CSV_PATH  = "db_2026.csv"
N_THREADS = 20
MEM_LIMIT = "32GB"


BLOQUES = {
    "Hipertension (I10-I15)":    "^I1[0-5]",
    "Diabetes (E10-E14)":        "^E1[0-4]",
    "Dislipidemia (E78)":        "^E78",
    "Obesidad (E66)":            "^E66",
    "Hipotiroidismo (E03)":      "^E03",
    "Asma (J45-J46)":            "^J4[56]",
    "EPOC (J44)":                "^J44",
    "Depresion (F32-F33)":       "^F3[23]",
    "Ansiedad (F40-F41)":        "^F4[01]",
    "T. desarrollo (F80-F89)":   "^F8",
    "Insuf. cardiaca (I50)":     "^I50",
    "ERC (N18)":                 "^N18",
    "Artrosis (M15-M19)":        "^M1[5-9]",
    "Cancer mama (C50)":         "^C50",
}


def main():
    con = duckdb.connect()
    con.execute(f"PRAGMA threads={N_THREADS}")
    con.execute(f"PRAGMA memory_limit='{MEM_LIMIT}'")
    con.execute(f"""
        CREATE VIEW tx AS
        SELECT CHAVE_FUNCIONAL AS id, NULLIF(TRIM(CID), '') AS cid
        FROM read_csv_auto('{CSV_PATH}', all_varchar=true, header=true)
    """)

    total = con.execute("SELECT COUNT(DISTINCT id) FROM tx").fetchone()[0]
    print(f"Beneficiarios totales: {total:,}\n")

    sel = ",\n".join(
        f"COUNT(DISTINCT CASE WHEN regexp_matches(cid, '{rx}') THEN id END) AS \"{name}\""
        for name, rx in BLOQUES.items()
    )
    res = con.execute(f"SELECT {sel} FROM tx").df().iloc[0]
    tabla = (res.to_frame("n_benef")
                .assign(pct=lambda d: (d["n_benef"] / total * 100).round(3))
                .sort_values("n_benef", ascending=False))
    print("=" * 55)
    print("PREVALENCIA DE ENFERMEDADES CANDIDATAS (beneficiario)")
    print("=" * 55)
    print(tabla.to_string())
    print()

    top = con.execute("""
        WITH d AS (
            SELECT DISTINCT id, substr(cid, 1, 3) AS cid3
            FROM tx WHERE cid IS NOT NULL
        )
        SELECT cid3, COUNT(DISTINCT id) AS n_benef
        FROM d GROUP BY cid3 ORDER BY n_benef DESC LIMIT 30
    """).df()
    top["pct"] = (top["n_benef"] / total * 100).round(3)
    print("=" * 55)
    print("TOP 30 CODIGOS CID (3 chars) POR # DE BENEFICIARIOS")
    print("=" * 55)
    print(top.to_string(index=False))

    con.close()


if __name__ == "__main__":
    main()