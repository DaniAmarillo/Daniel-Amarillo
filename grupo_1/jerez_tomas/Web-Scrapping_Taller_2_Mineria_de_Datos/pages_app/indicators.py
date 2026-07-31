
import streamlit as st
import database as db
import ui_components as ui
from icons import icon_inline
from shared import init_session_state, render_filters_sidebar, get_filtered_df


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "INDICADORES // KPI",
    "Métricas calculadas sobre la selección actual"
)

filtered_df = get_filtered_df()
kpis = db.compute_kpis(filtered_df)

st.caption(
    f"// MOSTRANDO {ui.fmt_int(kpis['total_papers'])} ARTÍCULOS "
    "FILTRADOS DEL TOTAL EN LA BASE DE DATOS //"
)

ui.scan_line()

st.markdown(
    f'## {icon_inline("activity", "INDICADORES PRINCIPALES", size=22)}',
    unsafe_allow_html=True,
)

c1, c2, c3, c4, c5 = st.columns(5)
with c1:
    ui.kpi_card("Total artículos", ui.fmt_int(kpis["total_papers"]),
                sub="en el filtro actual")
with c2:
    ui.kpi_card("Promedio autores", ui.fmt_float(kpis["avg_authors"], 1),
                sub="por artículo", flavor="accent")
with c3:
    ui.kpi_card("Promedio citas", ui.fmt_float(kpis["avg_citations"], 1),
                sub="por artículo", flavor="success")
with c4:
    ui.kpi_card("Promedio refs", ui.fmt_float(kpis["avg_references"], 1),
                sub="por artículo", flavor="warning")
with c5:
    ui.kpi_card("Descargas totales", ui.fmt_int(kpis["total_downloads"]),
                sub="(Accesses)", flavor="magenta")


ui.scan_line()
st.markdown(
    f'## {icon_inline("diamond-filled", "ARTÍCULOS DESTACADOS", size=22)}',
    unsafe_allow_html=True,
)

if kpis["most_cited"] and kpis["most_cited"]["citations"]:
    col_a, col_b = st.columns(2)
    with col_a:
        mc = kpis["most_cited"]
        ui.featured_paper(
            "ARTÍCULO MÁS CITADO",
            mc["title"] or "(sin título)",
            f'{ui.fmt_int(mc["citations"])} CITAS · DOI: {mc["doi"] or "—"}',
            icon="trophy",
        )
    with col_b:
        md = kpis["most_downloaded"]
        ui.featured_paper(
            "ARTÍCULO MÁS DESCARGADO",
            md["title"] or "(sin título)",
            f'{ui.fmt_int(md["downloads"])} ACCESSES · DOI: {md["doi"] or "—"}',
            icon="fire",
        )
else:
    st.info("Sin datos suficientes para mostrar artículos destacados.")

ui.scan_line()
st.markdown(
    f'## {icon_inline("bars-stack", "DESGLOSE POR CATEGORÍA", size=22)}',
    unsafe_allow_html=True,
)

if kpis["by_topic"]:
    cols = st.columns(max(len(kpis["by_topic"]), 1))
    flavors = ["", "accent", "success", "magenta"]
    for i, (cat, n) in enumerate(kpis["by_topic"].items()):
        pct = 100 * n / kpis["total_papers"] if kpis["total_papers"] else 0
        with cols[i]:
            ui.kpi_card(cat, ui.fmt_int(n), sub=f"{pct:.1f}% del total",
                        flavor=flavors[i % len(flavors)])
else:
    st.info("No hay artículos en la selección actual.")
