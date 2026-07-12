import streamlit as st
import database as db
import ui_components as ui
from icons import icon_inline
from shared import init_session_state, render_filters_sidebar


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "ACERCA DE // INFO",
    "Información técnica del proyecto"
)

st.markdown(
    f'## {icon_inline("info", "TALLER 2 · MINERÍA DE DATOS", size=22)}',
    unsafe_allow_html=True,
)

st.markdown(
    """
    Aplicación interactiva en **Streamlit** que cumple con los requisitos
    del Taller 2 de la asignatura *Minería de Datos* (cód. 2016325) de la
    Universidad Nacional de Colombia.

    Funciona como un **dashboard analítico** orientado al proceso KDD:
    integra adquisición (web scraping), almacenamiento (SQLite),
    consulta (SQL + filtros) y visualización (Plotly).
    """
)

ui.scan_line()

st.markdown(
    f'## {icon_inline("settings", "STACK TECNOLÓGICO", size=22)}',
    unsafe_allow_html=True,
)

c1, c2 = st.columns(2)
with c1:
    st.markdown(
        """
        **Backend / Procesamiento:**
        - `Python 3.10+`
        - `sqlite3` (incluido en Python)
        - `requests` + `beautifulsoup4` — scraping HTML
        - `pandas` — manipulación tabular
        """
    )
with c2:
    st.markdown(
        """
        **Frontend / Visualización:**
        - `streamlit` — framework de la app
        - `plotly` — gráficos interactivos
        - `OpenAlex API` — fuente bibliográfica fallback
        - CSS custom — tema cyberpunk neón
        """
    )

ui.scan_line()


st.markdown(
    f'## {icon_inline("database", "ESQUEMA DE LA BASE DE DATOS", size=22)}',
    unsafe_allow_html=True,
)

st.code("""
papers (paper_id, journal_name, title, publication_date, year, doi, url,
        abstract, authors_raw, n_authors, citations, downloads, altmetric,
        n_references, topic_label, article_type)

authors (author_id, author_name)
paper_authors (paper_id, author_id, author_order)

refs (reference_id, reference_text, reference_text_normalized)
paper_refs (paper_id, reference_id, ref_order)
""", language="sql")


with db.get_conn() as conn:
    counts = {
        "papers":        conn.execute("SELECT COUNT(*) FROM papers").fetchone()[0],
        "authors":       conn.execute("SELECT COUNT(*) FROM authors").fetchone()[0],
        "paper_authors": conn.execute("SELECT COUNT(*) FROM paper_authors").fetchone()[0],
        "refs":          conn.execute("SELECT COUNT(*) FROM refs").fetchone()[0],
        "paper_refs":    conn.execute("SELECT COUNT(*) FROM paper_refs").fetchone()[0],
    }

st.markdown("### Conteos actuales")
cols = st.columns(5)
flavors = ["", "accent", "magenta", "success", "warning"]
for i, (k, v) in enumerate(counts.items()):
    with cols[i]:
        ui.kpi_card(k, ui.fmt_int(v), flavor=flavors[i])

ui.scan_line()

st.markdown(
    f'## {icon_inline("activity", "CUMPLIMIENTO DEL TALLER", size=22)}',
    unsafe_allow_html=True,
)

st.markdown(
    """
    | Requisito | Implementación |
    |-----------|----------------|
    | **2.1.1** Conexión a SQLite | `database.py` con context manager + cache |
    | **2.1.2** Sidebar con filtros | Fechas, categoría, autor, DOI, keyword |
    | **2.1.3** ≥3 widgets interactivos | date_input, multiselect, selectbox, text_input, button, dataframe, tabs |
    | **2.1.4** ≥5 indicadores | 5 KPIs principales + 2 papers destacados + desglose temático |
    | **2.1.5** ≥2 gráficos plotly | **5 gráficos**: línea temporal, donut, top autores, histograma, scatter |
    | **2.1.6** Tabla interactiva | DataFrame con ordenamiento, búsqueda y export CSV |
    | **2.1.7** Botón scraping | Híbrido Nature + OpenAlex con detección de papers nuevos / actualización |
    """
)

ui.scan_line()

st.markdown(
    '<div class="small-footer">'
    'NATURE.MI DASHBOARD · v1.0 · TALLER 2 MINERÍA DE DATOS · '
    'UNIVERSIDAD NACIONAL DE COLOMBIA · 2026'
    '</div>',
    unsafe_allow_html=True
)
