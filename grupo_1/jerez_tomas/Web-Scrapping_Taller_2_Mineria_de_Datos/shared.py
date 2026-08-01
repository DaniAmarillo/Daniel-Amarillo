

import streamlit as st
import pandas as pd

import database as db
from icons import svg, icon_inline


def init_session_state():
    defaults = {
        "date_range": None,
        "topics_selected": None,
        "author_filter": None,
        "doi_filter": "",
        "keyword_filter": "",
    }
    for k, v in defaults.items():
        if k not in st.session_state:
            st.session_state[k] = v


def _filter_label(icon_name: str, text: str) -> str:
    """Construye un label HTML con icono SVG + texto.
    Útil cuando un input no permite renderizar HTML en su label."""
    return f"{text}"


def render_filters_sidebar():
    papers_df = db.get_all_papers()

    if not papers_df["publication_date"].dropna().empty:
        date_min = papers_df["publication_date"].min().date()
        date_max = papers_df["publication_date"].max().date()
    else:
        date_min = pd.Timestamp("2025-01-01").date()
        date_max = pd.Timestamp("2025-12-31").date()

    with st.sidebar:
        st.markdown(
            f'<div style="display:flex;align-items:center;gap:0.5em;'
            f'font-family:Orbitron,monospace;font-weight:700;'
            f'color:#00f0ff;font-size:1.1rem;letter-spacing:1.5px;'
            f'margin-bottom:1rem;text-shadow:0 0 8px rgba(0,240,255,0.5);">'
            f'{svg("filter", size=16, css_class="accent")} FILTROS</div>',
            unsafe_allow_html=True,
        )


        st.markdown(
            f'<div style="margin-top:0.5rem;display:flex;align-items:center;'
            f'gap:0.4em;color:#9d8bb8;font-size:0.85rem;">'
            f'{svg("calendar", size=14)} Rango de fechas</div>',
            unsafe_allow_html=True,
        )
        if st.session_state.date_range is None:
            st.session_state.date_range = (date_min, date_max)
        st.session_state.date_range = st.date_input(
            "Rango de fechas",
            value=st.session_state.date_range,
            min_value=date_min, max_value=date_max,
            key="date_input_widget",
            label_visibility="collapsed",
        )

        topics_available = db.get_topic_list()
        if st.session_state.topics_selected is None:
            st.session_state.topics_selected = topics_available
        st.markdown(
            f'<div style="margin-top:0.7rem;display:flex;align-items:center;'
            f'gap:0.4em;color:#9d8bb8;font-size:0.85rem;">'
            f'{svg("tag", size=14)} Categoría temática</div>',
            unsafe_allow_html=True,
        )
        st.session_state.topics_selected = st.multiselect(
            "Categoría",
            options=topics_available,
            default=st.session_state.topics_selected,
            key="topics_widget",
            label_visibility="collapsed",
        )

        authors_available = ["(todos)"] + db.get_author_list()
        idx = 0
        if st.session_state.author_filter and st.session_state.author_filter in authors_available:
            idx = authors_available.index(st.session_state.author_filter)
        st.markdown(
            f'<div style="margin-top:0.7rem;display:flex;align-items:center;'
            f'gap:0.4em;color:#9d8bb8;font-size:0.85rem;">'
            f'{svg("user", size=14)} Autor</div>',
            unsafe_allow_html=True,
        )
        choice = st.selectbox(
            "Autor", options=authors_available, index=idx,
            key="author_widget", label_visibility="collapsed",
        )
        st.session_state.author_filter = None if choice == "(todos)" else choice

        st.markdown(
            f'<div style="margin-top:0.7rem;display:flex;align-items:center;'
            f'gap:0.4em;color:#9d8bb8;font-size:0.85rem;">'
            f'{svg("link", size=14)} DOI (búsqueda parcial)</div>',
            unsafe_allow_html=True,
        )
        st.session_state.doi_filter = st.text_input(
            "DOI",
            value=st.session_state.doi_filter,
            placeholder="ej: 10.1038/s42256-025-",
            key="doi_widget", label_visibility="collapsed",
        )

        st.markdown(
            f'<div style="margin-top:0.7rem;display:flex;align-items:center;'
            f'gap:0.4em;color:#9d8bb8;font-size:0.85rem;">'
            f'{svg("search", size=14)} Palabra clave</div>',
            unsafe_allow_html=True,
        )
        st.session_state.keyword_filter = st.text_input(
            "Keyword",
            value=st.session_state.keyword_filter,
            placeholder="ej: protein, diffusion...",
            key="keyword_widget", label_visibility="collapsed",
        )

        st.markdown("")
        if st.button("LIMPIAR FILTROS", use_container_width=True):
            for k in ["date_range", "topics_selected", "author_filter",
                      "doi_filter", "keyword_filter"]:
                if k in st.session_state:
                    del st.session_state[k]
            st.rerun()

        st.markdown("---")
        st.markdown(
            '<div class="small-footer">'
            '<b>TALLER 2 · MINERÍA DATOS</b><br>'
            'UNAL · 2026'
            '</div>',
            unsafe_allow_html=True
        )


def get_filtered_df() -> pd.DataFrame:
    papers_df = db.get_all_papers()
    date_range = st.session_state.get("date_range")
    return db.filter_papers(
        papers_df,
        date_range=date_range if isinstance(date_range, tuple) and len(date_range) == 2 else None,
        topics=st.session_state.get("topics_selected"),
        author=st.session_state.get("author_filter"),
        doi=(st.session_state.get("doi_filter") or "").strip() or None,
        keyword=(st.session_state.get("keyword_filter") or "").strip() or None,
    )
