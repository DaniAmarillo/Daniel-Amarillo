
import streamlit as st
import ui_components as ui
from icons import svg, icon_inline
from shared import init_session_state, render_filters_sidebar, get_filtered_df


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "EXPLORAR // ARTÍCULOS",
    "Tabla interactiva con todos los datos disponibles"
)

filtered_df = get_filtered_df()
st.caption(
    f"// {len(filtered_df)} ARTÍCULOS EN LA SELECCIÓN · ORDENA CON LOS "
    "ENCABEZADOS · USA LA BÚSQUEDA DE LA TABLA //"
)

ui.scan_line()

display_df = filtered_df.copy()
display_df["publication_date"] = display_df["publication_date"].dt.strftime("%Y-%m-%d")

columns_to_show = [
    "title", "authors_raw", "publication_date", "topic_label",
    "doi", "citations", "downloads", "n_references", "url",
]

st.dataframe(
    display_df[columns_to_show],
    use_container_width=True,
    hide_index=True,
    column_config={
        "title":            st.column_config.TextColumn("TÍTULO", width="large"),
        "authors_raw":      st.column_config.TextColumn("AUTORES", width="medium"),
        "publication_date": st.column_config.TextColumn("FECHA", width="small"),
        "topic_label":      st.column_config.TextColumn("CATEGORÍA", width="small"),
        "doi":              st.column_config.TextColumn("DOI", width="medium"),
        "citations":        st.column_config.NumberColumn("CITAS", format="%d"),
        "downloads":        st.column_config.NumberColumn("DESCARGAS", format="%d"),
        "n_references":     st.column_config.NumberColumn("REFS", format="%d"),
        "url":              st.column_config.LinkColumn("URL", display_text="abrir"),
    },
    height=560,
)

ui.scan_line()

col_a, col_b = st.columns([2, 1])
with col_a:
    st.markdown(
        f'### {icon_inline("download", "Exportar selección", size=18)}',
        unsafe_allow_html=True,
    )
    st.caption(
        "Descarga la tabla filtrada actual en formato CSV con todas las "
        "columnas mostradas arriba."
    )
with col_b:
    csv = display_df[columns_to_show].to_csv(index=False).encode("utf-8")
    st.download_button(
        "DESCARGAR CSV",
        data=csv,
        file_name=f"papers_filtrados_{len(filtered_df)}.csv",
        mime="text/csv",
        use_container_width=True,
    )
