
import streamlit as st
import database as db
import ui_components as ui
from icons import svg, icon_inline
from shared import init_session_state, render_filters_sidebar


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "NATURE.MI // DASHBOARD",
    "Sistema de minería de datos · Nature Machine Intelligence · 2025+"
)

st.markdown(
    f"""
    {ui.status_badge("SYSTEM ONLINE", "online")} &nbsp;&nbsp;
    {ui.status_badge("DB CONNECTED", "online")} &nbsp;&nbsp;
    {ui.status_badge("SCRAPER READY", "online")}
    """,
    unsafe_allow_html=True,
)

ui.scan_line()

papers_df = db.get_all_papers()
kpis = db.compute_kpis(papers_df)

st.markdown(
    f'## {icon_inline("bar-chart", "RESUMEN GENERAL", size=22)}',
    unsafe_allow_html=True,
)

c1, c2, c3, c4 = st.columns(4)
with c1:
    ui.kpi_card("Total artículos", ui.fmt_int(kpis["total_papers"]),
                sub="en base de datos")
with c2:
    ui.kpi_card("Categorías", ui.fmt_int(len(kpis["by_topic"])),
                sub="temáticas detectadas", flavor="accent")
with c3:
    ui.kpi_card("Promedio citas", ui.fmt_float(kpis["avg_citations"], 1),
                sub="por artículo", flavor="success")
with c4:
    ui.kpi_card("Descargas totales", ui.fmt_int(kpis["total_downloads"]),
                sub="(Accesses)", flavor="magenta")

st.markdown("")


st.markdown(
    f'## {icon_inline("info", "SOBRE EL PROYECTO", size=22)}',
    unsafe_allow_html=True,
)
st.markdown(
    """
    Este dashboard interactivo permite explorar la producción científica
    publicada por **Nature Machine Intelligence** durante 2025+. Los datos
    fueron recolectados mediante *web scraping* en el **Taller 1** y se
    almacenan en una base de datos SQLite con cinco tablas relacionales.

    El dashboard funciona como una interfaz al proceso **KDD** (Knowledge
    Discovery in Databases): integra recolección, almacenamiento,
    consulta y visualización.
    """
)

st.markdown("### Navegación")

col_a, col_b = st.columns(2)
with col_a:
    st.markdown(
        f"""
        - {icon_inline("activity", "<b>Indicadores</b>", size=14)} — KPIs principales y papers destacados
        - {icon_inline("trending-up", "<b>Visualizaciones</b>", size=14)} — 5 gráficos interactivos con Plotly
        - {icon_inline("database", "<b>Explorar</b>", size=14)} — tabla filtrable con todos los artículos
        """,
        unsafe_allow_html=True,
    )
with col_b:
    st.markdown(
        f"""
        - {icon_inline("refresh", "<b>Actualizar BD</b>", size=14)} — scraping para encontrar papers nuevos
        - {icon_inline("info", "<b>Acerca de</b>", size=14)} — información técnica del proyecto
        """,
        unsafe_allow_html=True,
    )

ui.scan_line()

if kpis["by_topic"]:
    st.markdown("### Distribución temática actual")
    cols = st.columns(len(kpis["by_topic"]))
    flavors = ["", "accent", "success", "magenta"]
    for i, (cat, n) in enumerate(kpis["by_topic"].items()):
        with cols[i]:
            ui.kpi_card(cat, ui.fmt_int(n), sub="artículos",
                        flavor=flavors[i % len(flavors)])
