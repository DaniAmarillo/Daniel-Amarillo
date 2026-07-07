import re
import time
import os
import sqlite3
from datetime import date
import pandas as pd
import requests
import streamlit as st
import plotly.express as px
import plotly.graph_objects as go
from bs4 import BeautifulSoup
import undetected_chromedriver as uc
import atexit
# Configuración 
st.set_page_config(
    page_title="Dashboard – Journal of Finance",
    page_icon="",
    layout="wide",
    initial_sidebar_state="expanded",
)

st.markdown("""
<style>
html, body, [class*="css"] { font-family: 'Inter', sans-serif; }

[data-testid="stSidebar"] { background: #1e293b !important; color:#f1f5f9 !important; }
[data-testid="stSidebar"] .stMarkdown p { color: #f1f5f9 !important; }
[data-testid="stSidebar"] label { color: #94a3b8 !important; font-size: 0.78rem; text-transform: uppercase; letter-spacing: .05em; }
[data-testid="stSidebar"] input { background: #334155 !important; color: #ffffff !important; border-color: #475569 !important; }
[data-testid="stSidebar"] input::placeholder { color: #64748b !important; }
[data-testid="stSidebar"] [data-baseweb="select"] > div { background: #334155 !important; border-color: #475569 !important; color: #ffffff !important; }
[data-testid="stSidebar"] [data-baseweb="select"] * { color: #ffffff !important; }
[data-testid="stSidebar"] [data-baseweb="popover"] { background: #1e293b !important; }
[data-testid="stSidebar"] [role="option"] { background: #1e293b !important; color: #ffffff !important; }
[data-testid="stSidebar"] button { background: #2563eb !important; border-color: #2563eb !important; }
[data-testid="stSidebar"] button, [data-testid="stSidebar"] button * { color: #ffffff !important; }

.metric-card {
    background: #ffffff; border: 1px solid #e2e8f0; border-radius: 12px;
    padding: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,.06); text-align: center;
    min-height: 90px; display: flex; flex-direction: column; justify-content: center;
}
.metric-card .metric-value { font-size: 1.7rem; font-weight: 700; color: #1e40af; line-height: 1.1; }
.metric-card .metric-value-sm { font-size: 0.82rem; font-weight: 600; color: #1e40af; line-height: 1.3; word-break: break-word; }
.metric-card .metric-label { font-size: 0.72rem; color: #64748b; text-transform: uppercase; letter-spacing: .07em; margin-top: .3rem; }

.section-title { font-size: 1.05rem; font-weight: 600; color: #2563eb !important; border-left: 4px solid #2563eb; padding-left: .65rem; margin: 1.5rem 0 .8rem; }
.info-box    { background: #eff6ff; border-left: 4px solid #3b82f6; border-radius: 6px; padding: .6rem 1rem; font-size: .85rem; color: #1e3a8a; margin-bottom: .8rem; }
.success-box { background: #f0fdf4; border-left: 4px solid #22c55e; border-radius: 6px; padding: .6rem 1rem; font-size: .85rem; color: #14532d; margin-bottom: .8rem; }
.warning-box { background: #fefce8; border-left: 4px solid #eab308; border-radius: 6px; padding: .6rem 1rem; font-size: .85rem; color: #713f12; margin-bottom: .8rem; }

[data-testid="stTabs"] [role="tab"] { font-weight: 600; font-size: .88rem; }
[data-testid="stTabs"] [aria-selected="true"] { color: #2563eb !important; border-bottom: 3px solid #2563eb !important; }
.stDataFrame { border-radius: 8px; overflow: hidden; }
div[data-testid="stButton"] > button { border-radius: 8px; font-weight: 600; font-size: .85rem; padding: .45rem 1.1rem; transition: all .2s; }
</style>
""", unsafe_allow_html=True)

# Base de datos 
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DB_PATH  = os.path.join(BASE_DIR, "sqlite_webscraping.db")

@st.cache_resource
def get_connection():
    if not os.path.exists(DB_PATH):
        st.error(f"No se encontró la base de datos en '{DB_PATH}'.")
        st.stop()
    return sqlite3.connect(DB_PATH, check_same_thread=False)

def query_df(sql, params=()):
    return pd.read_sql_query(sql, get_connection(), params=params)

def execute_sql(sql, params=()):
    conn = get_connection()
    conn.execute(sql, params)
    conn.commit()

#  Clasificación temática 
def clasificar_tematica(resumen: str) -> str:
    texto = str(resumen).lower()
    if any(p in texto for p in [
        'machine learning', 'neural network', 'deep learning', 'random forest',
        'gradient boosting', 'natural language processing', 'large language model',
        'generative ai', 'gpt', 'llm', 'diffusion model', 'text analysis',
        'regression', 'econometrics', 'causal inference', 'instrumental variable',
        'difference-in-differences', 'panel data', 'bayesian', 'monte carlo',
        'simulation', 'principal component', 'factor analysis', 'clustering',
        'forecasting', 'time series', 'identification strategy',
    ]):
        return "IA - ML - Estadística"
    elif any(p in texto for p in [
        'trading', 'stock market', 'equity market', 'portfolio', 'asset pricing',
        'market microstructure', 'hedge fund', 'mutual fund', 'liquidity',
        'order flow', 'high-frequency', 'momentum', 'factor model', 'alpha',
        'informed trading', 'corporate bond', 'bond market', 'yield curve',
        'credit spread', 'arbitrage', 'market maker', 'price discovery',
        'futures', 'options', 'derivatives', 'short selling', 'stock return',
        'market return', 'asset allocation', 'investment strategy',
        'institutional investor', 'retail investor', 'return predictability',
        'cross-sectional', 'earnings announcement', 'analyst forecast',
    ]):
        return "Trading & Mercados"
    elif any(p in texto for p in [
        'default risk', 'credit risk', 'sovereign risk', 'systemic risk',
        'tail risk', 'volatility', 'value at risk', 'risk premium',
        'credit default swap', 'bankruptcy', 'financial distress', 'hedging',
        'risk management', 'insurance', 'reinsurance', 'catastrophe bond',
        'underwriting', 'moral hazard', 'adverse selection', 'deposit insurance',
        'collateral', 'counterparty risk', 'leverage', 'fire sale',
        'contagion', 'stress test', 'capital requirement', 'regulatory capital',
    ]):
        return "Riesgo & Seguros"
    return "Otros"

# Crossref helpers
_CROSSREF_HEADERS = {"User-Agent": "JournalFinanceDashboard/2.0 (mailto:dashboard@example.com)"}

_BASURA_KEYWORDS = [
    "issue information", "announcements", "american finance association",
    "report of the editor", "brattle group", "dimensional fund advisors",
    "annual membership", "fischer black prize", "winner of the",
    "editorial board", "table of contents",
]

def _es_basura(titulo: str) -> bool:
    t = titulo.strip().lower()
    return not t or any(kw in t for kw in _BASURA_KEYWORDS)

def _limpiar_abstract(raw: str) -> str:
    if not raw:
        return ""
    soup = BeautifulSoup(raw, "html.parser")
    for tag in soup.find_all(["jats:title", "title", "h1", "h2", "h3"]):
        tag.decompose()
    texto = soup.get_text(separator=" ").strip()
    texto = re.sub(r"^(Abstract|ABSTRACT|Summary|SUMMARY)\s*[:\-–¶]?\s*", "", texto).strip()
    texto = texto.replace("¶", " ").replace("\xa0", " ")
    return re.sub(r"\s{2,}", " ", texto)

def _doi_from_url(url: str) -> str:
    m = re.search(r"(?:doi\.org/|/doi/)(10\.[^?#\s\"']+)", url)
    return m.group(1) if m else ""

def _crossref_get(url: str) -> dict:
    try:
        r = requests.get(url, headers=_CROSSREF_HEADERS, timeout=30)
        r.raise_for_status()
        return r.json()
    except Exception:
        return {}

def obtener_articulos_crossref(rows: int = 60) -> pd.DataFrame:
    items = []
    for url in (
        f"https://api.crossref.org/journals/1540-6261/works?rows={rows}&sort=published&order=desc",
        f"https://api.crossref.org/works?filter=issn:1540-6261&rows={rows}&sort=published&order=desc",
    ):
        try:
            r = requests.get(url, headers=_CROSSREF_HEADERS, timeout=30)
            if r.status_code == 200:
                items = r.json().get("message", {}).get("items", [])
                if items:
                    break
        except Exception:
            continue

    if not items:
        raise RuntimeError("Crossref no devolvió artículos.")

    registros = []
    for item in items:
        titulo = (item.get("title") or [""])[0].strip()
        if _es_basura(titulo):
            continue
        doi     = item.get("DOI", "")
        autores = [
            f"{a.get('given','')} {a.get('family','')}".strip()
            for a in item.get("author", [])
        ]
        fecha = ""
        for campo in ("published-print", "published-online", "published"):
            partes = item.get(campo, {}).get("date-parts", [[]])[0]
            if partes:
                fecha = "-".join(str(p).zfill(2) for p in partes if p)
                break
        refs      = item.get("reference", [])
        ref_texts = " ; ".join(
            r.get("unstructured", "") or
            f"{r.get('author','')} {r.get('year','')} {r.get('article-title','')}".strip()
            for r in refs
        )
        registros.append({
            "title":            titulo,
            "publication_date": fecha,
            "doi":              doi,
            "url":              f"https://doi.org/{doi}" if doi else "",
            "authors_raw":      ", ".join(autores),
            "abstract":         _limpiar_abstract(item.get("abstract", "")),
            "citations":        item.get("is-referenced-by-count", 0),
            "n_references":     item.get("reference-count", 0),
            "references":       ref_texts,
            "views":            0,
        })

    if not registros:
        raise RuntimeError("Todos los registros fueron filtrados.")
    return pd.DataFrame(registros)

def obtener_detalle_crossref(doi: str) -> dict:
    if not doi:
        return {"abstract": "", "references": "", "citations": 0, "views": 0}
    item      = _crossref_get(f"https://api.crossref.org/works/{doi}").get("message", {})
    refs      = item.get("reference", [])
    ref_texts = " ; ".join(
        r.get("unstructured", "") or
        f"{r.get('author','')} {r.get('year','')} {r.get('article-title','')}".strip()
        for r in refs
    )
    return {
        "abstract":   _limpiar_abstract(item.get("abstract", "")),
        "references": ref_texts,
        "citations":  item.get("is-referenced-by-count", 0),
        "views":      0,
    }

# Selenium: driver y scraping de vistas 
@st.cache_resource
def get_driver():
    """Inicializa undetected_chromedriver una sola vez por sesión."""
    options = uc.ChromeOptions()
    options.add_argument("--no-first-run")
    options.add_argument("--no-service-autorun")
    options.add_argument("--password-store=basic")        
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    
    try:
        driver = uc.Chrome(options=options, version_main=149)
        atexit.register(driver.quit)  # línea nueva
        return driver
    
    except Exception:
        return None

def scrape_views(url: str) -> int:
    """
    Abre la página del artículo en Wiley con Selenium y extrae el número de vistas.
    Retorna 0 si no puede obtener el dato.
    """
    driver = get_driver()
    if driver is None or not url or "doi.org" not in url and "onlinelibrary" not in url:
        return 0
    try:
        # Construir URL de Wiley a partir del DOI si es necesario
        if "onlinelibrary.wiley.com" not in url:
            doi_part = url.replace("https://doi.org/", "").strip()
            wiley_url = f"https://onlinelibrary.wiley.com/doi/{doi_part}"
        else:
            wiley_url = url
        driver.get(wiley_url)
        time.sleep(4)
        soup = BeautifulSoup(driver.page_source, "html.parser")
        vistas = soup.select(".number-of-downloads span")
        if vistas:
            txt = vistas[0].get_text(strip=True).replace(",", "").replace(".", "")
            return int(txt) if txt.isdigit() else 0
        return 0
    except Exception:
        return 0

# Base de datos: escritura 
def _to_date_str(val) -> str | None:
    import datetime as _dt
    if val is None:
        return None
    if isinstance(val, (_dt.date, _dt.datetime)):
        return val.strftime("%Y-%m-%d")
    if isinstance(val, pd.Timestamp):
        return None if pd.isnull(val) else val.strftime("%Y-%m-%d")
    s = str(val).strip().split(" ")[0]
    if not s or s in ("None", "nan", "NaT"):
        return None
    parsed = pd.to_datetime(s, format="%Y-%m-%d", errors="coerce")
    if pd.isnull(parsed):
        parsed = pd.to_datetime(s, dayfirst=True, errors="coerce")
    return None if pd.isnull(parsed) else parsed.strftime("%Y-%m-%d")

def insert_paper(paper: dict):
    conn   = get_connection()
    cursor = conn.cursor()
    cursor.execute("SELECT COALESCE(MAX(paper_id),0)+1 FROM papers")
    pid      = cursor.fetchone()[0]
    pub_date = _to_date_str(paper.get("publication_date"))
    year_val = int(pub_date[:4]) if pub_date else paper.get("year", 2026)

    cursor.execute("""
        INSERT OR IGNORE INTO papers
        (paper_id, journal_name, title, publication_date, year, doi, url,
         abstract, authors_raw, n_authors, citations, views, n_references, topic_label)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    """, (pid, paper.get("journal_name", "Journal of Finance"), paper["title"],
          pub_date, year_val, paper.get("doi"), paper.get("url"),
          paper.get("abstract", ""), paper.get("authors_raw", ""),
          paper.get("n_authors", 0), paper.get("citations", 0), paper.get("views", 0),
          paper.get("n_references", 0), paper.get("topic_label", "Otros")))

    for j, nombre in enumerate(str(paper.get("authors_raw", "")).split(",")):
        nombre = nombre.strip()
        if nombre and nombre.lower() != "nan":
            cursor.execute("INSERT OR IGNORE INTO authors (author_name) VALUES (?)", (nombre,))
            cursor.execute("SELECT author_id FROM authors WHERE author_name=?", (nombre,))
            a_id = cursor.fetchone()[0]
            cursor.execute("INSERT INTO paper_authors (paper_id, author_id, author_order) VALUES (?,?,?)",
                           (pid, a_id, j + 1))

    for ref in str(paper.get("references", "")).split(" ; "):
        ref = ref.strip()
        if ref and ref.lower() != "nan":
            cursor.execute("INSERT OR IGNORE INTO references_table (reference_text_normalized) VALUES (?)", (ref,))
            r_id = cursor.lastrowid
            cursor.execute("INSERT INTO paper_references (paper_id, reference_id) VALUES (?,?)", (pid, r_id))

    conn.commit()

# Carga de datos 
@st.cache_data(ttl=30)
def load_papers() -> pd.DataFrame:
    df = query_df("SELECT * FROM papers")
    if "publication_date" in df.columns:
        df["publication_date"] = (
            df["publication_date"].astype(str).str.strip()
            .str.replace(r"\s\d{2}:\d{2}:\d{2}$", "", regex=True)
            .replace({"None": None, "nan": None, "": None})
        )
        df["publication_date"] = pd.to_datetime(df["publication_date"], format="%Y-%m-%d", errors="coerce")
    return df

# Sidebar 
def render_sidebar(df: pd.DataFrame):
    st.sidebar.markdown("## Filtros")
    st.sidebar.markdown("---")
    fecha_min = df["publication_date"].min().date() if not df["publication_date"].isna().all() else date(2025, 1, 1)
    fecha_max = date.today()
    st.sidebar.markdown("**Rango de fechas**")
    col1, col2 = st.sidebar.columns(2)
    d_inicio = col1.date_input("Desde", value=fecha_min, min_value=fecha_min, max_value=fecha_max, key="d_inicio")
    d_fin    = col2.date_input("Hasta", value=fecha_max, min_value=fecha_min, max_value=fecha_max, key="d_fin")
    temas      = ["Todos"] + sorted(df["topic_label"].dropna().unique().tolist())
    tema_sel   = st.sidebar.selectbox("Temática", temas, key="tema_sel")
    autor_query = st.sidebar.text_input("Buscar autor",  placeholder="Ej. Fama, Jensen…",  key="autor_query")
    doi_query   = st.sidebar.text_input("DOI exacto",   placeholder="10.1111/jofi.xxxxx", key="doi_query")
    kw_query    = st.sidebar.text_input("Palabras clave (título / resumen)", placeholder="Ej. liquidity risk", key="kw_query")
    st.sidebar.markdown("---")
    st.sidebar.button("Aplicar filtros",  use_container_width=True, key="aplicar_btn")
    if st.sidebar.button("Limpiar filtros", use_container_width=True, key="limpiar_btn"):
        for k in ["d_inicio", "d_fin", "tema_sel", "autor_query", "doi_query", "kw_query"]:
            st.session_state.pop(k, None)
        st.rerun()
    return d_inicio, d_fin, tema_sel, autor_query, doi_query, kw_query

def apply_filters(df, d_inicio, d_fin, tema_sel, autor_query, doi_query, kw_query) -> pd.DataFrame:
    mask      = pd.Series(True, index=df.index)
    ts_inicio = pd.Timestamp(d_inicio)
    ts_fin    = pd.Timestamp(d_fin) + pd.Timedelta(hours=23, minutes=59, seconds=59)
    mask &= df["publication_date"].isna() | df["publication_date"].between(ts_inicio, ts_fin)
    if tema_sel != "Todos":
        mask &= df["topic_label"] == tema_sel
    if autor_query.strip():
        mask &= df["authors_raw"].str.contains(autor_query.strip(), case=False, na=False)
    if doi_query.strip():
        mask &= df["doi"].str.contains(doi_query.strip(), case=False, na=False)
    if kw_query.strip():
        mask &= (df["title"].str.contains(kw_query.strip(), case=False, na=False) |
                 df["abstract"].str.contains(kw_query.strip(), case=False, na=False))
    return df[mask].copy()

# Indicadores 
def metric_card(col, value, label, small=False):
    css = "metric-value-sm" if small else "metric-value"
    col.markdown(f"""
    <div class="metric-card">
      <div class="{css}">{value}</div>
      <div class="metric-label">{label}</div>
    </div>""", unsafe_allow_html=True)

def render_indicators(df: pd.DataFrame):
    st.markdown('<div class="section-title">Indicadores descriptivos</div>', unsafe_allow_html=True)
    n = len(df)
    c1, c2, c3, c4, c5 = st.columns(5)
    metric_card(c1, n,                                            "Total artículos")
    metric_card(c2, round(df["n_authors"].mean(),  2) if n else 0, "Prom. autores")
    metric_card(c3, round(df["citations"].mean(),  2) if n else 0, "Prom. citas")
    metric_card(c4, round(df["n_references"].mean(), 2) if n else 0, "Prom. referencias")
    metric_card(c5, (df.loc[df["citations"].idxmax(), "title"][:35] + "…") if n else "–",
                "Más citado", small=True)
    if n:
        st.markdown("**Artículos por temática**")
        tbl = (df.groupby("topic_label")
               .agg(n_articulos=("paper_id","count"), prom_citas=("citations","mean"))
               .reset_index()
               .rename(columns={"topic_label":"Temática","n_articulos":"N° Artículos","prom_citas":"Prom. Citas"}))
        tbl["Prom. Citas"] = tbl["Prom. Citas"].round(2)
        st.dataframe(tbl, use_container_width=True, hide_index=True)

# Gráficos 
COLORES = ["#2563eb", "#16a34a", "#dc2626", "#9333ea", "#ea580c", "#0891b2"]

def _layout(title, **kw):
    base = dict(
        title=dict(text=title, font=dict(family="Inter", size=15, color="#0f172a"), x=0, xanchor="left", pad=dict(l=4)),
        plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
        font=dict(family="Inter", size=12, color="#1e293b"),
        margin=dict(t=56, b=44, l=60, r=24),
        xaxis=dict(showgrid=True, gridcolor="#e2e8f0", linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155")),
        yaxis=dict(showgrid=True, gridcolor="#e2e8f0", linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155")),
        hoverlabel=dict(bgcolor="#1e293b", font_color="#f1f5f9", bordercolor="#475569",
                        font_size=12, font_family="Inter"),
    )
    base.update(kw)
    return base

def chart_evolucion_temporal(df: pd.DataFrame):
    if df.empty: return
    tmp = (df.dropna(subset=["publication_date"])
           .assign(mes=lambda x: x["publication_date"].dt.to_period("M").astype(str))
           .groupby("mes").size().reset_index(name="n").sort_values("mes"))
    fig = go.Figure(go.Scatter(
        x=tmp["mes"], y=tmp["n"], mode="lines+markers",
        line=dict(color="#2563eb", width=2.5),
        marker=dict(size=7, color="#2563eb", line=dict(color="white", width=1.5)),
        fill="tozeroy", fillcolor="rgba(37,99,235,0.08)",
        hovertemplate="<b>%{x}</b><br>Artículos: %{y}<extra></extra>"))
    fig.update_layout(**_layout("Evolución temporal de publicaciones",
        hovermode="x unified",
        xaxis=dict(showgrid=False, title="Mes", linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155")),
        yaxis=dict(showgrid=True, gridcolor="#e2e8f0", title="N° Artículos",
                   linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155"))))
    st.plotly_chart(fig, use_container_width=True)

def chart_por_tematica(df: pd.DataFrame):
    if df.empty: return
    tmp = df.groupby("topic_label").size().reset_index(name="n").sort_values("n", ascending=False)
    fig = go.Figure(go.Bar(
        x=tmp["n"], y=tmp["topic_label"], orientation="h",
        marker=dict(color=COLORES[:len(tmp)], line=dict(color="white", width=0.5)),
        text=tmp["n"], textposition="outside",
        hovertemplate="<b>%{y}</b><br>Artículos: %{x}<extra></extra>"))
    fig.update_layout(**_layout("Artículos por categoría",
        margin=dict(t=56, b=44, l=130, r=60),
        xaxis=dict(showgrid=True, gridcolor="#e2e8f0", title="N° Artículos",
                   linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155")),
        yaxis=dict(autorange="reversed", showgrid=False, linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155", size=12))))
    st.plotly_chart(fig, use_container_width=True)

def chart_top_autores(df: pd.DataFrame, top_n: int = 10):
    if df.empty: return
    ae = (df["authors_raw"].dropna()
          .str.upper()
          .str.split(",")
          .explode()
          .str.strip()
          .loc[lambda s: s != ""]
          .value_counts()
          .head(top_n)
          .reset_index())
    ae.columns = ["Autor", "N° Artículos"]
    fig = go.Figure(go.Bar(
        x=ae["N° Artículos"], y=ae["Autor"], orientation="h",
        marker=dict(color="#7c3aed", line=dict(color="white", width=0.5)),
        text=ae["N° Artículos"], textposition="outside",
        hovertemplate="<b>%{y}</b><br>Artículos: %{x}<extra></extra>"))
    fig.update_layout(**_layout(f"Top {top_n} autores más publicados",
        margin=dict(t=56, b=44, l=170, r=60),
        xaxis=dict(showgrid=True, gridcolor="#e2e8f0", title="N° Artículos",
                   linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155")),
        yaxis=dict(autorange="reversed", showgrid=False, linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155", size=11))))
    st.plotly_chart(fig, use_container_width=True)

def chart_distribucion_citas(df: pd.DataFrame):
    if df.empty: return
    fig = go.Figure()
    for i, (label, grp) in enumerate(df.groupby("topic_label")):
        fig.add_trace(go.Box(y=grp["citations"], name=label,
                             marker_color=COLORES[i % len(COLORES)],
                             line_color=COLORES[i % len(COLORES)], boxmean=True))
    fig.update_layout(**_layout("Distribución de citas por temática",
        showlegend=True, legend=dict(font=dict(color="#1e293b")),
        yaxis=dict(showgrid=True, gridcolor="#e2e8f0", title="N° Citas",
                   linecolor="#cbd5e1", tickcolor="#cbd5e1",
                   title_font=dict(color="#334155"), tickfont=dict(color="#334155"))))
    st.plotly_chart(fig, use_container_width=True)

# Tabla 
def render_tabla(df: pd.DataFrame):
    st.markdown('<div class="section-title">Artículos filtrados</div>', unsafe_allow_html=True)
    cols = ["title", "authors_raw", "publication_date", "topic_label", "doi", "citations", "views"]
    labs = ["Título", "Autores", "Fecha", "Temática", "DOI", "Citas", "Vistas"]
    disp = df[cols].copy()
    disp.columns = labs
    disp["Fecha"]  = disp["Fecha"].dt.strftime("%Y-%m-%d").fillna("Early View")
    disp["Título"] = disp["Título"].str[:80] + "…"
    st.dataframe(disp, use_container_width=True, hide_index=True, height=420,
        column_config={
            "Título":  st.column_config.TextColumn("Título",  width="large"),
            "Autores": st.column_config.TextColumn("Autores", width="medium"),
            "Citas":   st.column_config.NumberColumn("Citas",  format="%d"),
            "Vistas":  st.column_config.NumberColumn("Vistas", format="%d"),
            "DOI":     st.column_config.LinkColumn("DOI"),
        })
    c1, c2 = st.columns(2)
    c1.download_button(
        " Descargar tabla filtrada (CSV)",
        data=disp.to_csv(index=False).encode("utf-8"),
        file_name=f"papers_filtrados_{date.today()}.csv",
        mime="text/csv",
        use_container_width=True,
    )
    try:
        with open(DB_PATH, "rb") as f:
            db_bytes = f.read()
        c2.download_button(
            " Descargar base de datos (SQLite)",
            data=db_bytes,
            file_name=f"sqlite_webscraping_{date.today()}.db",
            mime="application/octet-stream",
            use_container_width=True,
        )
    except Exception:
        c2.warning("No se pudo leer el archivo .db.")

# Scraping 
def render_scraping_tab():
    st.markdown('<div class="section-title">Actualización automática mediante scraping</div>',
                unsafe_allow_html=True)
    st.markdown("""<div class="info-box">
    Busca artículos nuevos del <strong>Journal of Finance</strong> que aún no estén en la base de datos.
    Si no hay nuevos, re-consulta los últimos 5 para actualizar citas y vistas.<br><br>
     <b>Citas</b>: obtenidas desde la <b>API de Crossref</b> (gratuita, sin bloqueos).<br>
     <b>Vistas</b>: obtenidas desde <b>Wiley Online Library</b> usando Selenium (puede tardar unos segundos por artículo).
    </div>""", unsafe_allow_html=True)
    col1, col2, _ = st.columns([1, 1, 3])
    if col1.button(" Buscar artículos nuevos", type="primary", use_container_width=True):
        _run_scraping_nuevos()
    if col2.button(" Verificar últimos 5", use_container_width=True):
        _verificar_ultimos_5()

def _run_scraping_nuevos():
    placeholder  = st.empty()
    progress_bar = st.progress(0)
    log_box      = st.empty()

    placeholder.markdown('<div class="info-box"> Consultando Crossref API…</div>', unsafe_allow_html=True)

    doi_existentes = set(query_df("SELECT doi   FROM papers")["doi"].dropna().tolist())
    url_existentes = set(query_df("SELECT url   FROM papers")["url"].dropna().tolist())
    tit_existentes = set(query_df("SELECT title FROM papers")["title"].dropna().str.strip().str.lower().tolist())

    progress_bar.progress(0.2)
    log_box.markdown(" Descargando artículos recientes del Journal of Finance…")

    try:
        df_total = obtener_articulos_crossref(rows=60)
    except Exception as e:
        placeholder.error(f" Error al consultar Crossref: {e}")
        progress_bar.progress(1.0)
        return

    progress_bar.progress(0.4)

    nuevos = []
    for _, row in df_total.iterrows():
        doi = str(row.get("doi", "")).strip()
        url = str(row.get("url", "")).strip()
        tit = str(row.get("title", "")).strip().lower()
        if (not doi or doi not in doi_existentes) and \
           (not url or url not in url_existentes) and \
           (not tit or tit not in tit_existentes):
            nuevos.append(row.to_dict())

    if not nuevos:
        placeholder.markdown(
            '<div class="warning-box"> No hay artículos nuevos. Usa "Verificar últimos 5" para actualizar citas.</div>',
            unsafe_allow_html=True)
        progress_bar.progress(1.0)
        return

    placeholder.markdown(
        f'<div class="info-box"> Encontrados <b>{len(nuevos)}</b> artículos nuevos. Insertando…</div>',
        unsafe_allow_html=True)

    log_lines = []
    for i, paper in enumerate(nuevos):
        progress_bar.progress(round(0.4 + (i + 1) / len(nuevos) * 0.6, 2))
        log_lines.append(f" {paper['title'][:60]}…")
        log_box.markdown("\n".join(log_lines[-8:]))
        pub_str = _to_date_str(paper.get("publication_date"))
        paper["publication_date"] = pub_str
        paper["year"]         = int(pub_str[:4]) if pub_str else 2026
        paper["journal_name"] = "Journal of Finance"
        paper["n_authors"]    = len([a for a in str(paper.get("authors_raw", "")).split(",") if a.strip()])
        paper["n_references"] = int(paper.get("n_references", 0)) or \
                                len([r for r in str(paper.get("references", "")).split(";") if r.strip()])
        paper["topic_label"]  = clasificar_tematica(paper.get("abstract", ""))
        paper.setdefault("citations", 0)
        # Obtener vistas desde Wiley con Selenium
        wiley_url = paper.get("url", "")
        paper["views"] = scrape_views(wiley_url)
        insert_paper(paper)

    progress_bar.progress(1.0)
    st.cache_data.clear()
    log_box.empty()
    placeholder.markdown(
        f'<div class="success-box"> Se agregaron <b>{len(nuevos)}</b> artículos nuevos.</div>',
        unsafe_allow_html=True)
    cols_show = ["title", "authors_raw", "publication_date", "topic_label", "citations", "views"]
    df_n = pd.DataFrame(nuevos)[[c for c in cols_show if c in pd.DataFrame(nuevos).columns]].copy()
    df_n.columns = ["Título", "Autores", "Fecha", "Temática", "Citas", "Vistas"][:len(df_n.columns)]
    st.dataframe(df_n, use_container_width=True, hide_index=True)
    time.sleep(8)
    st.rerun()

def _verificar_ultimos_5():
    ultimos = query_df(
        "SELECT paper_id, title, doi, url, citations, views FROM papers ORDER BY publication_date DESC LIMIT 5")
    if ultimos.empty:
        st.warning("No hay artículos en la base de datos.")
        return
    progress_bar = st.progress(0)
    status       = st.empty()
    resultados   = []
    for i, row in enumerate(ultimos.itertuples(index=False)):
        progress_bar.progress((i + 1) / len(ultimos))
        status.info(f"Verificando ({i+1}/{len(ultimos)}): {row.title[:60]}…")
        doi = str(row.doi).strip() if row.doi and str(row.doi).strip() not in ("", "None", "nan") else ""
        if not doi:
            doi = _doi_from_url(str(row.url or ""))
        try:
            detalles  = obtener_detalle_crossref(doi)
            new_views = scrape_views(str(row.url or ""))
            execute_sql("UPDATE papers SET citations=?, abstract=?, views=? WHERE paper_id=?",
                        (detalles["citations"], detalles["abstract"] or "", new_views, row.paper_id))
            resultados.append({
                "Título":         row.title[:65] + "…",
                "Citas antes":    row.citations,
                "Citas nuevas":   detalles["citations"],
                "Vistas antes":   row.views,
                "Vistas nuevas":  new_views,
            })
        except Exception:
            resultados.append({
                "Título": row.title[:65] + "…",
                "Citas antes": row.citations, "Citas nuevas": "—",
                "Vistas antes": getattr(row, "views", 0), "Vistas nuevas": "—",
            })
    progress_bar.progress(1.0)
    status.empty()
    st.cache_data.clear()
    st.success(" Citas y vistas actualizadas desde Crossref y Wiley.")
    st.dataframe(pd.DataFrame(resultados), use_container_width=True, hide_index=True)
    time.sleep(8)
    st.rerun()

# Gestión CRUD 
def render_crud_tab(df: pd.DataFrame):
    st.markdown('<div class="section-title">Gestión de registros</div>', unsafe_allow_html=True)
    tab_edit, tab_del = st.tabs(["Editar artículo", "Eliminar artículo"])

    with tab_edit:
        if df.empty:
            st.warning("No hay artículos con los filtros actuales.")
            return
        opciones = {f"[{r.paper_id}] {r.title[:80]}": r.paper_id for _, r in df.iterrows()}
        pid      = opciones[st.selectbox("Selecciona un artículo", list(opciones.keys()), key="crud_select")]
        reg      = df[df["paper_id"] == pid].iloc[0]
        TEMAS    = ["IA - ML - Estadística", "Trading & Mercados", "Riesgo & Seguros", "Otros"]
        with st.form("form_editar"):
            new_title    = st.text_input("Título",  value=reg["title"])
            new_abstract = st.text_area("Resumen",  value=reg.get("abstract", ""), height=120)
            new_tema     = st.selectbox("Temática", TEMAS,
                           index=TEMAS.index(reg["topic_label"]) if reg["topic_label"] in TEMAS else 3)
            new_citas    = st.number_input("Citas", value=int(reg["citations"]), min_value=0)
            if st.form_submit_button(" Guardar cambios", type="primary"):
                execute_sql("UPDATE papers SET title=?,abstract=?,topic_label=?,citations=? WHERE paper_id=?",
                            (new_title, new_abstract, new_tema, new_citas, pid))
                st.cache_data.clear()
                st.success(f"Artículo [{pid}] actualizado.")

    with tab_del:
        if df.empty:
            st.warning("No hay artículos con los filtros actuales.")
            return
        opciones_del = {f"[{r.paper_id}] {r.title[:80]}": r.paper_id for _, r in df.iterrows()}
        pid_del   = opciones_del[st.selectbox("Artículo a eliminar", list(opciones_del.keys()), key="crud_del")]
        confirmar = st.checkbox("Confirmo que deseo eliminar este artículo permanentemente")
        if st.button(" Eliminar artículo", type="primary", disabled=not confirmar):
            execute_sql("DELETE FROM papers WHERE paper_id=?", (pid_del,))
            st.cache_data.clear()
            st.success(f"Artículo [{pid_del}] eliminado.")
            st.rerun()

    st.markdown("---")
    st.markdown('<div class="section-title"> Exportar base de datos</div>', unsafe_allow_html=True)
    col1, col2 = st.columns([1, 2])
    try:
        with open(DB_PATH, "rb") as f:
            db_bytes = f.read()
        col1.download_button(
            " Descargar SQLite completa",
            data=db_bytes,
            file_name=f"sqlite_webscraping_{date.today()}.db",
            mime="application/octet-stream",
            use_container_width=True,
            type="primary",
        )
        col2.markdown(
            f"<div style='padding:.5rem 0;color:#475569;font-size:.83rem;'>"
            f"Tamaño: <b>{len(db_bytes)/1024:.1f} KB</b> · "
            f"Artículos totales: <b>{query_df('SELECT COUNT(*) as n FROM papers').iloc[0,0]}</b>"
            f"</div>", unsafe_allow_html=True)
    except Exception as e:
        st.error(f"No se pudo leer el archivo .db: {e}")

#  Main 
def main():
    st.markdown("""
    <div style="background:linear-gradient(135deg,#1e3a8a 0%,#2563eb 100%);
                border-radius:12px;padding:1.5rem 2rem;margin-bottom:1.5rem;color:white;">
      <h1 style="margin:0;font-size:1.7rem;font-weight:700;"> Dashboard — Journal of Finance</h1>
      <p style="margin:.4rem 0 0;font-size:.9rem;opacity:.85;">
        Minería de Datos · Taller 2 · Jorge Andres Sanchez Duarte · Universidad Nacional de Colombia
      </p>
    </div>""", unsafe_allow_html=True)

    df_raw = load_papers()
    d_inicio, d_fin, tema_sel, autor_query, doi_query, kw_query = render_sidebar(df_raw)
    df = apply_filters(df_raw, d_inicio, d_fin, tema_sel, autor_query, doi_query, kw_query)

    n_total, n_fil = len(df_raw), len(df)
    st.markdown(f'<div class="info-box">Mostrando <b>{n_fil}</b> de <b>{n_total}</b> artículos.</div>',
                unsafe_allow_html=True)

    tab1, tab2, tab3, tab4, tab5 = st.tabs(
        [" Resumen", " Visualizaciones", " Datos", " Scraping", " Gestión"])

    with tab1:
        render_indicators(df)
        st.markdown('<div class="section-title">Vista rápida</div>', unsafe_allow_html=True)
        cl, cr = st.columns(2)
        with cl: chart_evolucion_temporal(df)
        with cr: chart_por_tematica(df)

    with tab2:
        st.markdown('<div class="section-title">Visualizaciones interactivas</div>', unsafe_allow_html=True)
        top_n = st.slider("N° de autores en el ranking", 5, 20, 10, key="top_n_slider")
        ca, cb = st.columns(2)
        with ca: chart_top_autores(df, top_n)
        with cb: chart_distribucion_citas(df)
        if not df.empty:
            st.markdown("**Vistas vs Citas por artículo (scatter)**")
            fig_s = px.scatter(df, x="citations", y="views", color="topic_label",
                hover_data=["title"],
                labels={"citations": "Citas", "views": "Vistas", "topic_label": "Tematica"},
                color_discrete_sequence=COLORES, template="plotly_white")
            fig_s.update_layout(
                title=dict(text="Vistas vs Citas por articulo",
                           font=dict(family="Inter", size=15, color="#0f172a"), x=0, xanchor="left"),
                font=dict(family="Inter", size=12, color="#1e293b"),
                plot_bgcolor="#ffffff", paper_bgcolor="#ffffff",
                legend=dict(orientation="h", y=1.08, font=dict(color="#1e293b")),
                margin=dict(t=60, b=44, l=60, r=24),
                hoverlabel=dict(bgcolor="#1e293b", font_color="#f1f5f9", bordercolor="#475569",
                                font_size=12, font_family="Inter"))
            st.plotly_chart(fig_s, use_container_width=True)

    with tab3: render_tabla(df)
    with tab4: render_scraping_tab()
    with tab5: render_crud_tab(df)

    st.markdown("---")
    st.markdown("<div style='text-align:center;color:#94a3b8;font-size:.78rem;'>"
                "Minería de Datos · Taller 2 · 2026 · Universidad Nacional de Colombia</div>",
                unsafe_allow_html=True)

if __name__ == "__main__":
    main()
