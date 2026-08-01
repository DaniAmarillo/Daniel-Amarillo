

import streamlit as st
import ui_components as ui
from icons import svg
from shared import init_session_state, render_filters_sidebar, get_filtered_df


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "VISUALIZACIONES // PLOTLY",
    "Gráficos interactivos sobre la selección actual"
)

filtered_df = get_filtered_df()
st.caption(
    f"// MOSTRANDO {len(filtered_df)} ARTÍCULOS · USA LOS FILTROS DEL "
    "SIDEBAR PARA EXPLORAR SUBCONJUNTOS //"
)

ui.scan_line()

tab1, tab2, tab3, tab4, tab5 = st.tabs([
    "◆  LÍNEA TEMPORAL",
    "◇  POR CATEGORÍA",
    "▰  TOP AUTORES",
    "▲  CITAS",
    "◈  CITAS vs DESCARGAS",
])

with tab1:
    st.plotly_chart(ui.chart_publications_timeline(filtered_df),
                    use_container_width=True)
    st.caption(
        "Distribución mensual de publicaciones. Permite ver picos de "
        "actividad y la cadencia editorial de la revista."
    )

with tab2:
    c1, c2 = st.columns(2)
    with c1:
        st.plotly_chart(ui.chart_topic_distribution(filtered_df),
                        use_container_width=True)
    with c2:
        st.plotly_chart(ui.chart_downloads_by_topic(filtered_df),
                        use_container_width=True)
    st.caption(
        "Izquierda: proporción de artículos por categoría temática. "
        "Derecha: descargas (accesses) totales agregadas por categoría."
    )

with tab3:
    st.plotly_chart(ui.chart_top_authors(filtered_df, n=10),
                    use_container_width=True)
    st.caption(
        "Top 10 autores por número de artículos publicados en la selección. "
        "Los autores aparecen tantas veces como artículos en los que figuran."
    )

with tab4:
    st.plotly_chart(ui.chart_citations_histogram(filtered_df),
                    use_container_width=True)
    st.caption(
        "Distribución de citas por artículo. Permite identificar la "
        "concentración del impacto (long tail) o la dispersión."
    )

with tab5:
    st.plotly_chart(ui.chart_downloads_vs_citations(filtered_df),
                    use_container_width=True)
    st.caption(
        "Relación entre descargas y citas, coloreado por categoría temática. "
        "Útil para detectar artículos virales (mucha descarga, pocas citas) "
        "vs. artículos académicos (muchas citas, descargas moderadas)."
    )
