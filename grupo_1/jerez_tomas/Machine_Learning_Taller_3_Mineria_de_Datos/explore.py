import os
import re
import csv
import pandas as pd

CSV_PATH = "db_2026.csv"    
SAMPLE   = 200_000           
CHUNK    = 500_000           


pd.set_option("display.max_columns", None)
pd.set_option("display.width", 220)
pd.set_option("display.max_colwidth", 60)

HYPER = re.compile(r"^I1[0-5]")  


def detect_encoding_sep(path, n_bytes=200_000):
    for enc in ("utf-8", "latin-1"):
        try:
            with open(path, "r", encoding=enc) as f:
                sample = f.read(n_bytes)
            first = sample.splitlines()[0] if sample else ""
            try:
                sep = csv.Sniffer().sniff(sample, delimiters=";,|\t").delimiter
            except Exception:
                sep = ";" if first.count(";") >= first.count(",") else ","
            return enc, sep
        except UnicodeDecodeError:
            continue
    return "latin-1", ";"


def resolve(cols, target):
    """Nombre real de una columna, case-insensitive."""
    return {c.lower(): c for c in cols}.get(target.lower())


def main():
    if not os.path.exists(CSV_PATH):
        raise FileNotFoundError(f"No encuentro '{CSV_PATH}'. Ajusta CSV_PATH.")

    enc, sep = detect_encoding_sep(CSV_PATH)
    print("=" * 60)
    print(f"ENCODING detectado : {enc}")
    print(f"SEPARADOR detectado: '{sep}'")
    print(f"Tamaño archivo     : {os.path.getsize(CSV_PATH)/1e6:.1f} MB")
    print("=" * 60, "\n")


    df = pd.read_csv(CSV_PATH, sep=sep, encoding=enc, nrows=SAMPLE, dtype=str)
    print(f">>> Muestra: {df.shape[0]:,} filas x {df.shape[1]} columnas\n")

    print("=" * 60); print("COLUMNAS"); print("=" * 60)
    print(list(df.columns), "\n")

    print("=" * 60); print("HEAD (12 filas)"); print("=" * 60)
    print(df.head(12), "\n")

    print("=" * 60); print("% FALTANTES POR COLUMNA (muestra)"); print("=" * 60)
    print((df.isna().mean() * 100).round(2).sort_values(ascending=False), "\n")

    print("=" * 60); print("VALORES UNICOS POR COLUMNA (muestra)"); print("=" * 60)
    print(df.nunique().sort_values(ascending=False), "\n")

    key  = resolve(df.columns, "CHAVE_FUNCIONAL")
    cidc = resolve(df.columns, "CID")
    valc = resolve(df.columns, "VALOR_UTILIZACAO")

   
    if cidc:
        print("=" * 60); print("ANALISIS DEL CID (target I10-I15)"); print("=" * 60)
        cid = df[cidc].dropna().astype(str).str.strip().str.upper()
        print("Ejemplos de formato (40 unicos):")
        print(cid.unique()[:40], "\n")
        print("Largos de codigo:")
        print(cid.str.len().value_counts().head(8), "\n")
        print("Contiene '.'? ->", bool(cid.str.contains(r"\.").any()))
        i1x = sorted(cid[cid.str.match(HYPER)].unique())
        print(f"Codigos I10-I15 en la muestra ({len(i1x)}): {i1x}\n")
        print("Top 20 CID mas frecuentes (muestra):")
        print(cid.value_counts().head(20), "\n")

    cat_cols = ["SEXO_BENEFICIARIO", "CETIPO", "TIPO_BENEFICIARIO", "UTI",
                "INTERNADO", "PORTE_ANESTESICO", "TIPO_UNIDADE_PREST_HOSPITALAR",
                "UF_CNES_PREST_HOSPITALAR", "DESC_ESPECIALIDADE"]
    print("=" * 60); print("CATEGORICAS CLAVE (muestra)"); print("=" * 60)
    for name in cat_cols:
        col = resolve(df.columns, name)
        if col:
            print(f"--- {col}  (n_unique={df[col].nunique()}) ---")
            print(df[col].value_counts(dropna=False).head(12), "\n")

    if valc:
        print("=" * 60); print("VALOR_UTILIZACAO (formato)"); print("=" * 60)
        print("Ejemplos crudos:", df[valc].dropna().head(10).tolist())
        s = df[valc].dropna().astype(str)
        v1 = pd.to_numeric(s, errors="coerce")
        v2 = pd.to_numeric(
            s.str.replace(".", "", regex=False).str.replace(",", ".", regex=False),
            errors="coerce")
        print(f"%% NaN parseo directo         : {v1.isna().mean()*100:.1f}")
        print(f"%% NaN quitando miles+coma dec: {v2.isna().mean()*100:.1f}")
        best = v2 if v2.isna().mean() < v1.isna().mean() else v1
        print("describe (mejor parseo):")
        print(best.describe(), "\n")

    print("=" * 60); print("FECHAS (ejemplos crudos)"); print("=" * 60)
    for name in ["DT_UTILIZACAO", "DT_NASCIMENTO_BENEFICIARIO"]:
        col = resolve(df.columns, name)
        if col:
            print(f"--- {col} ---")
            print(df[col].dropna().astype(str).unique()[:6], "\n")

    print("CONTEO GLOBAL")
    if key and cidc:
        total, all_ben, hyper_ben = 0, set(), set()
        for chunk in pd.read_csv(CSV_PATH, sep=sep, encoding=enc,
                                 usecols=[key, cidc], dtype=str, chunksize=CHUNK):
            total += len(chunk)
            all_ben.update(chunk[key].dropna())
            sub = chunk[[key, cidc]].dropna(subset=[cidc]).copy()
            sub[cidc] = sub[cidc].astype(str).str.strip().str.upper()
            m = sub[cidc].str.match(HYPER)
            hyper_ben.update(sub.loc[m, key].dropna())
        prev = len(hyper_ben) / len(all_ben) * 100 if all_ben else 0
        print(f"Filas totales             : {total:,}")
        print(f"Beneficiarios unicos      : {len(all_ben):,}")
        print(f"Beneficiarios con I10-I15 : {len(hyper_ben):,}")
        print(f"PREVALENCIA (target=1)    : {prev:.2f}%")
    else:
        print("No pude resolver CHAVE_FUNCIONAL y/o CID; revisa las columnas de arriba.")

if __name__ == "__main__":
    main()