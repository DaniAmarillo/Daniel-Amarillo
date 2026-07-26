# -*- coding: utf-8 -*-
"""
Taller 2 — Minería de Datos (2016325)
Dashboard analítico (KDD) sobre artículos de *ACM Transactions on Graphics*.

Arquitectura:
  · app.py      → interfaz Streamlit (ligera: streamlit + pandas + plotly).
  · db.py       → acceso/normalización de la base SQLite del Taller 1.
  · scraper.py  → actualización por scraping con Selenium + undetected-chromedriver
                    sobre ACM (ejecución local; requiere Google Chrome instalado).

"""

import os
from pathlib import Path

import pandas as pd
import plotly.express as px
import streamlit as st

import db
import scraper

# ──────────────────────────────────────────────────────────────────────────────
#  Configuración general
# ──────────────────────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="ACM TOG · Dashboard",
    page_icon="◼",
    layout="wide",
    initial_sidebar_state="expanded",
)

DB_PATH = str(Path(__file__).parent / "tog_q1_2025.sqlite")


def en_streamlit_cloud() -> bool:
    """
    Heurística para saber si la app corre en Streamlit Community Cloud.
    Allí el repositorio se clona bajo /mount/src/<repo>, una ruta que no existe
    en una máquina local (Windows, macOS o Linux). Ante la duda, asume local.
    """
    try:
        ruta = str(Path(__file__).resolve())
    except Exception:
        ruta = ""
    if ruta.startswith("/mount/src"):
        return True
    if os.path.isdir("/mount/src"):
        return True
    if "streamlit" in os.environ.get("HOSTNAME", "").lower():
        return True
    return False

# Color SOLO para las gráficas (el resto del tablero es B/N).
TOPIC_COLORS = {
    "Machine Learning": "#2563eb",
    "IA Generativa":    "#dc2626",
    "Estadística":      "#16a34a",
    "Otro":             "#6b7280",
}
PALETTE = ["#2563eb", "#dc2626", "#16a34a", "#d97706",
           "#7c3aed", "#0891b2", "#db2777", "#65a30d"]
ACENTO = "#1a1a1a"          # acento "tinta" para gráficas de una sola serie
FONT_SERIF = "Computer Modern Serif, Latin Modern Roman, Georgia, 'Times New Roman', serif"

# ──────────────────────────────────────────────────────────────────────────────
#  Estilo (CSS) — estética LaTeX en blanco y negro
# ──────────────────────────────────────────────────────────────────────────────
st.markdown(
    """
    <style>
    @import url('https://cdn.jsdelivr.net/gh/aaaakshat/cm-web-fonts@latest/fonts.css');

    :root { --tinta: #1a1a1a; --regla: #1a1a1a; --gris: #6b6b6b; }

    html, body, [class*="css"], .stMarkdown, .stText, p, span, div, label,
    h1, h2, h3, h4, h5, h6, .stDataFrame, button, input, textarea, select {
        font-family: 'Computer Modern Serif', 'Latin Modern Roman', Georgia,
                     'Times New Roman', serif !important;
    }
    /* Los iconos Material deben conservar SU fuente; si se les fuerza serif se
       muestra el texto literal (p. ej. "keyboard_arrow_right") encimado sobre
       la etiqueta del expander. */
    span[data-testid="stIconMaterial"], [data-testid="stIconMaterial"] {
        font-family: 'Material Symbols Rounded', 'Material Symbols Outlined',
                     'Material Icons' !important;
    }

    .stApp { background: #ffffff; color: var(--tinta); }
    /* Ocultar SOLO elementos concretos del chrome (Deploy, menú, footer). No se
       oculta el header ni el toolbar como contenedores: ahí se renderiza el botón
       para mostrar/ocultar el sidebar, y ocultar el contenedor lo elimina. */
    #MainMenu, footer,
    [data-testid="stMainMenu"],
    [data-testid="stAppDeployButton"],
    [data-testid="stStatusWidget"] { display: none !important; }
    header[data-testid="stHeader"], [data-testid="stAppHeader"] { background: transparent; }
    /* El control del sidebar (mostrar/ocultar) siempre visible. */
    [data-testid="stSidebarCollapseButton"],
    [data-testid="stExpandSidebarButton"] {
        visibility: visible !important; display: inline-flex !important; opacity: 1 !important;
    }
    .block-container { padding-top: 2.2rem; max-width: 1180px; }

    /* ── Encabezado tipo portada de artículo ── */
    .latex-header {
        text-align: center;
        border-top: 2.5px solid var(--regla);
        border-bottom: 1px solid var(--regla);
        padding: 1.1rem 0 0.9rem 0;
        margin-bottom: 1.6rem;
    }
    .latex-title  { font-size: 2.0rem; font-weight: 700; letter-spacing: .2px; line-height: 1.2; }
    .latex-sub    { font-size: 1.0rem; font-style: italic; color: var(--gris); margin-top: .35rem; }
    .latex-meta   { font-size: .85rem; color: var(--gris); margin-top: .55rem; }

    /* ── Títulos de sección numerados ── */
    .sec {
        font-size: 1.28rem; font-weight: 700;
        border-bottom: 1px solid var(--regla);
        padding-bottom: .35rem; margin: 1.9rem 0 1.0rem 0;
    }
    .sec .num { font-weight: 700; margin-right: .5rem; }

    /* ── Tarjetas de indicadores (esquinas rectas, regla fina) ── */
    .cards { display: flex; flex-wrap: wrap; gap: 0; margin: .2rem 0 .4rem 0;
             border: 1px solid var(--regla); }
    .card  { flex: 1 1 0; min-width: 150px; padding: .95rem 1.0rem;
             border-right: 1px solid var(--regla); text-align: center; }
    .card:last-child { border-right: none; }
    .card .v { font-size: 1.7rem; font-weight: 700; line-height: 1.1; }
    .card .l { font-size: .76rem; text-transform: uppercase; letter-spacing: .8px;
               color: var(--gris); margin-top: .35rem; }

    .callouts { display: flex; gap: 1rem; margin-top: 1rem; }
    .callout  { flex: 1; border: 1px solid var(--regla); padding: .85rem 1rem; }
    .callout .l { font-size: .74rem; text-transform: uppercase; letter-spacing: .8px;
                  color: var(--gris); }
    .callout .t { font-size: 1.0rem; font-weight: 600; margin-top: .25rem; line-height: 1.3; }
    .callout .n { font-size: .85rem; color: var(--gris); margin-top: .3rem; }

    /* ── Sidebar ── */
    section[data-testid="stSidebar"] { background: #fafafa; border-right: 1px solid var(--regla); }
    section[data-testid="stSidebar"] .sidebar-title {
        font-size: 1.05rem; font-weight: 700; border-bottom: 1px solid var(--regla);
        padding-bottom: .3rem; margin-bottom: .6rem;
    }

    /* ── Chips del multiselect (antes: texto negro sobre fondo negro) ── */
    span[data-baseweb="tag"] {
        background-color: #ececec !important;
        border: 1px solid var(--regla) !important;
        border-radius: 0 !important;
    }
    span[data-baseweb="tag"], span[data-baseweb="tag"] * {
        color: var(--tinta) !important; fill: var(--tinta) !important;
    }

    /* ── Inputs y botones rectos, monocromos ── */
    .stButton > button {
        border: 1px solid var(--regla); border-radius: 0; background: #ffffff;
        color: var(--tinta) !important; font-weight: 600;
    }
    .stButton > button:hover { background: var(--tinta) !important; }
    .stButton > button:hover, .stButton > button:hover * { color: #ffffff !important; }
    /* El botón primario también debe ser legible (texto oscuro sobre claro). */
    .stButton > button[kind="primary"],
    button[data-testid="stBaseButton-primary"] {
        background: #ffffff !important; color: var(--tinta) !important;
    }
    .stButton > button[kind="primary"]:hover,
    button[data-testid="stBaseButton-primary"]:hover {
        background: var(--tinta) !important; color: #ffffff !important;
    }
    div[data-baseweb="input"] input, div[data-baseweb="textarea"] textarea { border-radius: 0; }

    /* ── Tabla estilo booktabs ── */
    .stDataFrame { border-top: 1.5px solid var(--regla); border-bottom: 1.5px solid var(--regla); }
    </style>
    """,
    unsafe_allow_html=True,
)

# ──────────────────────────────────────────────────────────────────────────────
#  Carga de datos (con clave de versión para invalidar caché tras el scraping)
# ──────────────────────────────────────────────────────────────────────────────
if "data_version" not in st.session_state:
    st.session_state.data_version = 0
if "scrape_result" not in st.session_state:
    st.session_state.scrape_result = None


@st.cache_data(show_spinner=False)
def cargar(db_path: str, version: int) -> pd.DataFrame:
    """Carga normalizada de la BD. `version` solo sirve de clave de caché."""
    return db.load_all_papers(db_path)


df = cargar(DB_PATH, st.session_state.data_version)

# ──────────────────────────────────────────────────────────────────────────────
#  Encabezado
# ──────────────────────────────────────────────────────────────────────────────
journal = df["journal_name"].dropna().mode()
journal = journal.iloc[0] if not journal.empty else "ACM Transactions on Graphics"
st.markdown(
    f"""
    <div class="latex-header">
        <div class="latex-title">Análisis de {journal}</div>
        <div class="latex-sub">Johan Farith Canelo Gonzalez</div>
        <div class="latex-sub">Dashboard del proceso KDD &mdash; Taller&nbsp;2, Minería de Datos (2016325)</div>
        <div class="latex-meta">Adquisición &middot; Almacenamiento &middot; Consulta &middot; Visualización</div>
    </div>
    """,
    unsafe_allow_html=True,
)

# ──────────────────────────────────────────────────────────────────────────────
#  Sidebar — filtros interactivos
# ──────────────────────────────────────────────────────────────────────────────
st.sidebar.markdown('<div class="sidebar-title">Filtros</div>', unsafe_allow_html=True)

# (1) Rango de fechas
fechas_validas = df["fecha_dt"].dropna()
if not fechas_validas.empty:
    fmin = fechas_validas.min().date()
    fmax = fechas_validas.max().date()
    if fmin == fmax:                                   # evita slider degenerado
        rango = (fmin, fmax)
        st.sidebar.caption(f"Fecha única en los datos: {fmin}")
    else:
        rango = st.sidebar.slider(
            "Rango de fechas", min_value=fmin, max_value=fmax,
            value=(fmin, fmax), format="DD/MM/YYYY",
        )
else:
    rango = None

# (2) Tema / categoría
temas = sorted(df["topic_label"].dropna().unique().tolist())
sel_temas = st.sidebar.multiselect("Tema / categoría", temas, default=temas)

# (3) Autor (búsqueda parcial)
f_autor = st.sidebar.text_input("Autor contiene", placeholder="p. ej. Wang")

# (4) DOI (búsqueda parcial)
f_doi = st.sidebar.text_input("DOI contiene", placeholder="10.1145/…")

# (5) Palabras clave / título
f_kw = st.sidebar.text_input("Título o palabra clave", placeholder="p. ej. neural rendering")

st.sidebar.markdown("---")
st.sidebar.caption(
    f"Base: **{len(df)}** artículos · "
    f"{int(df['year'].dropna().min())}–{int(df['year'].dropna().max())}"
)

# ── Aplicación de filtros ──
mask = pd.Series(True, index=df.index)
if rango is not None:
    ini = pd.Timestamp(rango[0])
    fin = pd.Timestamp(rango[1]) + pd.Timedelta(days=1)   # incluye el día final
    # Conserva filas sin fecha parseable para no "perder" artículos.
    mask &= df["fecha_dt"].isna() | df["fecha_dt"].between(ini, fin)
if sel_temas:
    mask &= df["topic_label"].isin(sel_temas)
if f_autor.strip():
    mask &= df["authors_raw"].fillna("").str.contains(f_autor.strip(), case=False, regex=False)
if f_doi.strip():
    mask &= df["doi"].fillna("").str.contains(f_doi.strip(), case=False, regex=False)
if f_kw.strip():
    kw = f_kw.strip()
    en_titulo = df["title"].fillna("").str.contains(kw, case=False, regex=False)
    en_resumen = df["abstract"].fillna("").str.contains(kw, case=False, regex=False)
    mask &= (en_titulo | en_resumen)

dff = df[mask].copy()

# ──────────────────────────────────────────────────────────────────────────────
#  1 · Indicadores
# ──────────────────────────────────────────────────────────────────────────────
st.markdown('<div class="sec"><span class="num">1</span>Indicadores</div>', unsafe_allow_html=True)

if dff.empty:
    st.warning("Ningún artículo cumple los filtros seleccionados.")
else:
    total      = len(dff)
    prom_aut   = dff["n_authors"].mean()
    prom_cit   = dff["citations"].mean()
    prom_ref   = dff["n_references"].mean()
    tot_desc   = int(dff["downloads"].sum())

    cards = f"""
    <div class="cards">
        <div class="card"><div class="v">{total:,}</div><div class="l">Artículos</div></div>
        <div class="card"><div class="v">{prom_aut:.2f}</div><div class="l">Autores / art.</div></div>
        <div class="card"><div class="v">{prom_cit:.1f}</div><div class="l">Citas / art.</div></div>
        <div class="card"><div class="v">{prom_ref:.1f}</div><div class="l">Referencias / art.</div></div>
        <div class="card"><div class="v">{tot_desc:,}</div><div class="l">Descargas totales</div></div>
    </div>
    """
    st.markdown(cards, unsafe_allow_html=True)

    mas_citado = dff.loc[dff["citations"].idxmax()]
    mas_desc   = dff.loc[dff["downloads"].idxmax()]
    callouts = f"""
    <div class="callouts">
        <div class="callout">
            <div class="l">Artículo más citado</div>
            <div class="t">{mas_citado['title']}</div>
            <div class="n">{int(mas_citado['citations']):,} citas · {mas_citado['topic_label']}</div>
        </div>
        <div class="callout">
            <div class="l">Artículo más descargado</div>
            <div class="t">{mas_desc['title']}</div>
            <div class="n">{int(mas_desc['downloads']):,} descargas · {mas_desc['topic_label']}</div>
        </div>
    </div>
    """
    st.markdown(callouts, unsafe_allow_html=True)

# ──────────────────────────────────────────────────────────────────────────────
#  2 · Visualizaciones  (Plotly, con color)
# ──────────────────────────────────────────────────────────────────────────────
st.markdown('<div class="sec"><span class="num">2</span>Visualizaciones</div>', unsafe_allow_html=True)


def estilo(fig, alto=320):
    fig.update_layout(
        template="simple_white",
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=dict(family=FONT_SERIF, color="#1a1a1a", size=13),
        margin=dict(l=10, r=10, t=42, b=10),
        height=alto,
        legend=dict(orientation="h", yanchor="bottom", y=1.0, xanchor="left", x=0),
    )
    fig.update_xaxes(showgrid=False, linecolor="#1a1a1a")
    fig.update_yaxes(showgrid=True, gridcolor="#ececec", linecolor="#1a1a1a")
    return fig


if dff.empty:
    st.info("Sin datos para graficar con los filtros actuales.")
else:
    c1, c2 = st.columns(2)

    # (a) Evolución temporal de publicaciones (por mes)
    with c1:
        tmp = dff.dropna(subset=["fecha_dt"]).copy()
        if not tmp.empty:
            tmp["mes"] = tmp["fecha_dt"].dt.to_period("M").dt.to_timestamp()
            serie = tmp.groupby("mes").size().reset_index(name="Artículos")
            fig = px.area(serie, x="mes", y="Artículos", markers=True,
                          title="Evolución temporal de publicaciones")
            fig.update_traces(line_color=PALETTE[0], fillcolor="rgba(37,99,235,0.12)",
                              marker_color=PALETTE[0])
            fig.update_xaxes(title_text="")
            st.plotly_chart(estilo(fig), width='stretch')
        else:
            st.caption("Sin fechas válidas para la serie temporal.")

    # (b) Artículos por categoría
    with c2:
        cat = dff["topic_label"].value_counts().reset_index()
        cat.columns = ["Tema", "Artículos"]
        fig = px.bar(cat, x="Tema", y="Artículos", color="Tema",
                     color_discrete_map=TOPIC_COLORS,
                     title="Artículos por categoría")
        fig.update_layout(showlegend=False)
        fig.update_xaxes(title_text="")
        st.plotly_chart(estilo(fig), width='stretch')

    c3, c4 = st.columns(2)

    # (c) Top autores (se calcula desde autores_clean del df filtrado)
    with c3:
        autores = (
            dff["autores_clean"].fillna("")
            .str.split(r"\s*;\s*").explode().str.strip()
        )
        autores = autores[(autores != "") & (autores != "N/A")]
        if not autores.empty:
            top = autores.value_counts().head(12).reset_index()
            top.columns = ["Autor", "Artículos"]
            top = top.sort_values("Artículos")
            fig = px.bar(top, x="Artículos", y="Autor", orientation="h",
                         title="Top autores")
            fig.update_traces(marker_color=PALETTE[4])
            fig.update_yaxes(title_text="")
            st.plotly_chart(estilo(fig, alto=360), width='stretch')
        else:
            st.caption("Sin autores para los filtros actuales.")

    # (d) Distribución de citas
    with c4:
        fig = px.histogram(dff, x="citations", nbins=30,
                           title="Distribución de citas")
        fig.update_traces(marker_color=PALETTE[1])
        fig.update_xaxes(title_text="Citas")
        fig.update_yaxes(title_text="Artículos")
        st.plotly_chart(estilo(fig, alto=360), width='stretch')

# ──────────────────────────────────────────────────────────────────────────────
#  3 · Artículos  (tabla interactiva)
# ──────────────────────────────────────────────────────────────────────────────
st.markdown('<div class="sec"><span class="num">3</span>Artículos</div>', unsafe_allow_html=True)

if dff.empty:
    st.info("La tabla no tiene filas con los filtros actuales.")
else:
    tabla = dff.sort_values("citations", ascending=False)[
        ["title", "autores_clean", "fecha_dt", "topic_label",
         "url", "citations", "downloads", "n_references"]
    ].copy()
    tabla.columns = ["Título", "Autores", "Fecha", "Tema",
                     "DOI", "Citas", "Descargas", "Referencias"]
    st.dataframe(
        tabla,
        width='stretch',
        hide_index=True,
        height=460,
        column_config={
            "Título": st.column_config.TextColumn(width="large"),
            "Autores": st.column_config.TextColumn(width="medium"),
            "Fecha": st.column_config.DateColumn(format="DD/MM/YYYY"),
            "DOI": st.column_config.LinkColumn(
                "DOI", display_text=r"(?:doi\.org/|doi/)(10\..+)$"
            ),
            "Citas": st.column_config.NumberColumn(format="%d"),
            "Descargas": st.column_config.NumberColumn(format="%d"),
            "Referencias": st.column_config.NumberColumn(format="%d"),
        },
    )
    st.caption(f"{len(tabla):,} artículos tras aplicar los filtros.")

# ──────────────────────────────────────────────────────────────────────────────
#  4 · Consulta SQL personalizada (solo lectura)
# ──────────────────────────────────────────────────────────────────────────────
st.markdown('<div class="sec"><span class="num">4</span>Consulta SQL</div>', unsafe_allow_html=True)

with st.expander("Ejecutar una consulta SELECT sobre la base"):
    consulta = st.text_area(
        "Consulta (solo se permite un único SELECT)",
        value=("SELECT title, year, citations, downloads\n"
               "FROM papers\n"
               "ORDER BY citations DESC\n"
               "LIMIT 10;"),
        height=140,
    )
    if st.button("Ejecutar consulta"):
        try:
            res = db.run_select(DB_PATH, consulta)
            st.dataframe(res, width='stretch', hide_index=True)
            st.caption(f"{len(res):,} filas devueltas.")
        except Exception as e:
            st.error(f"No se pudo ejecutar: {e}")

# ──────────────────────────────────────────────────────────────────────────────
#  5 · Actualización mediante scraping
# ──────────────────────────────────────────────────────────────────────────────
st.markdown('<div class="sec"><span class="num">5</span>Actualización (scraping)</div>',
            unsafe_allow_html=True)

st.markdown(
    "Busca artículos publicados después de los ya almacenados, usando el scraper de "
    "navegador (Selenium + undetected-chromedriver) sobre ACM. Si encuentra nuevos, "
    "los guarda en SQLite e informa cuántos; si no hay, reconsulta los últimos 5 para "
    "verificar cambios en sus métricas."
)

CHROME_REQUERIDO = 149

if en_streamlit_cloud():
    # En la nube el scraper no puede funcionar: los servidores de Streamlit no tienen
    # navegador y Cloudflare bloquea las IP de datacenter al entrar a ACM. Se muestran
    # las instrucciones para ejecutarlo en local.
    st.warning(
        "El scraping solo funciona **en local**. Esta versión, desplegada en Streamlit "
        "Cloud, sirve como dashboard de consulta y visualización: el navegador Chrome y "
        "la evasión de Cloudflare que necesita el scraper no están disponibles en los "
        "servidores de Streamlit."
    )
    st.markdown(
        "Para ejecutar la actualización por scraping, clona el repositorio y córrelo en "
        f"tu máquina. Necesitas **Google Chrome {CHROME_REQUERIDO}** instalado:"
    )
    st.code(
        "git clone https://github.com/SJOEC/Strealit-ACM-TOG.git\n"
        "cd <carpeta-del-repositorio>\n"
        "pip install -r requirements.txt -r requirements-scraper.txt\n"
        "python -m streamlit run app.py",
        language="bash",
    )
    st.caption(
        "Al ejecutarla en local aparecerá aquí el botón para buscar artículos nuevos."
    )
else:
    # ── Ejecución local: verificación de Chrome + botón de scraping ──
    chrome_v = scraper.detectar_chrome_version()
    if chrome_v == CHROME_REQUERIDO:
        st.success(f"Google Chrome {chrome_v} detectado.")
    elif chrome_v is None:
        st.warning(
            f"No pude verificar Google Chrome. El scraper necesita **Chrome "
            f"{CHROME_REQUERIDO}** instalado; instálalo antes de ejecutar el scraping."
        )
    else:
        st.warning(
            f"Detecté Google Chrome {chrome_v}, pero el scraper funciona con la versión "
            f"**{CHROME_REQUERIDO}**. Instala Chrome {CHROME_REQUERIDO} para que funcione."
        )

    if st.button("Buscar artículos nuevos"):
        with st.status("Ejecutando scraping…", expanded=True) as status:
            try:
                resultado = scraper.actualizar(
                    DB_PATH, metodo="navegador", headless=False,
                    version_main=CHROME_REQUERIDO, progress=status.write,
                )
                status.update(label="Scraping finalizado.", state="complete")
            except Exception as e:
                resultado = None
                status.update(label="El scraping falló.", state="error")
                st.error(str(e))

        if resultado is not None:
            st.session_state.scrape_result = resultado
            st.session_state.data_version += 1   # invalida la caché de datos
            st.rerun()                            # recarga el tablero con los datos nuevos

# Resultado persistente tras el rerun
res = st.session_state.scrape_result
if res:
    metodo_txt = "navegador (ACM)"
    if res.get("modo") == "nuevos":
        nuevos = res.get("nuevos", [])
        st.success(f"Se encontraron y guardaron {len(nuevos)} artículo(s) nuevo(s) "
                   f"(modo {metodo_txt}).")
        if nuevos:
            tn = pd.DataFrame([{
                "Título": n.get("titulo", ""),
                "Fecha": n.get("fecha", ""),
                "Autores": n.get("autores", ""),
                "DOI": n.get("doi", ""),
                "Citas": n.get("num_citas", 0),
                "Referencias": n.get("num_referencias", 0),
            } for n in nuevos])
            st.dataframe(tn, width='stretch', hide_index=True)
    else:
        act = res.get("actualizados", [])
        st.info(f"No había artículos nuevos. Se reconsultaron {len(act)} artículo(s) "
                f"(modo {metodo_txt}).")
        if act:
            ta = pd.DataFrame([{
                "Título": a.get("title", ""),
                "DOI": a.get("doi", ""),
                "Citas antes": a.get("citas_antes", 0),
                "Citas ahora": a.get("citas_ahora", 0),
                "Descargas antes": a.get("descargas_antes", 0),
                "Descargas ahora": a.get("descargas_ahora", 0),
            } for a in act])
            st.dataframe(ta, width='stretch', hide_index=True)

    if st.button("Ocultar resultado"):
        st.session_state.scrape_result = None
        st.rerun()

st.markdown(
    "<div style='margin-top:2rem;border-top:1px solid #1a1a1a;padding-top:.5rem;"
    "font-size:.8rem;color:#6b6b6b;'>Universidad Nacional de Colombia · Minería de Datos "
    "2016325 · Proceso KDD: adquisición, almacenamiento, consulta y visualización.</div>",
    unsafe_allow_html=True,
)
