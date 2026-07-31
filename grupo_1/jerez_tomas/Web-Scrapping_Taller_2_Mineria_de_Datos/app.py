
import streamlit as st

import database as db

st.set_page_config(
    page_title="Nature MI · Cyberpunk Dashboard",
    page_icon="",
    layout="wide",
    initial_sidebar_state="expanded",
)

if not db.db_exists():
    st.error(
        " No se encontró el archivo `revista_q1_2025.sqlite` en la raíz "
        "del proyecto. Asegúrate de copiarlo desde el Taller 1."
    )
    st.stop()

pages = {
    "": [
        st.Page("pages_app/home.py", title="Inicio",
                icon="🏠", default=True),
    ],
    "ANÁLISIS": [
        st.Page("pages_app/indicators.py", title="Indicadores", icon="📊"),
        st.Page("pages_app/visualizations.py", title="Visualizaciones", icon="📈"),
        st.Page("pages_app/explore.py", title="Explorar artículos", icon="🗂️"),
        st.Page("pages_app/search.py", title="Buscador", icon="🔎"),
    ],
    "ACCIONES": [
        st.Page("pages_app/update_db.py", title="Actualizar BD", icon="🔄"),
        st.Page("pages_app/about.py", title="Acerca de", icon="ℹ️"),
    ],
}

nav = st.navigation(pages, position="sidebar", expanded=True)
nav.run()
