

import datetime as _dt
import streamlit as st

import database as db
import ui_components as ui
from icons import svg, icon_inline
from scraper import (
    list_articles_in_year, scrape_article, scrape_article_by_id, classify_paper,
)
from shared import init_session_state, render_filters_sidebar


init_session_state()
ui.inject_css()
render_filters_sidebar()


ui.cyber_header(
    "ACTUALIZAR // SCRAPING",
    "Búsqueda automática de artículos nuevos en Nature MI"
)

papers_df = db.get_all_papers()
kpis = db.compute_kpis(papers_df)

c1, c2, c3 = st.columns(3)
with c1:
    ui.kpi_card("Artículos en BD", ui.fmt_int(kpis["total_papers"]),
                sub="actualmente almacenados", flavor="accent")
with c2:
    last_date = papers_df["publication_date"].max()
    last_str = last_date.strftime("%Y-%m-%d") if last_date is not None else "—"
    ui.kpi_card("Último artículo", last_str,
                sub="fecha de publicación", flavor="magenta")
with c3:
    current_year = _dt.date.today().year
    ui.kpi_card("Año a consultar", str(current_year),
                sub="año actual", flavor="success")

ui.scan_line()

st.markdown(
    f'## {icon_inline("refresh", "BUSCAR ARTÍCULOS NUEVOS", size=22)}',
    unsafe_allow_html=True,
)

st.markdown(
    f"""
    La aplicación buscará artículos publicados en **{current_year}** en
    Nature Machine Intelligence:

    1. **Si encuentra artículos nuevos:** los descargará, clasificará y
       almacenará automáticamente en la base de datos SQLite.
    2. **Si no encuentra nuevos:** reconsultará los **5 artículos más
       recientes** para verificar si sus métricas (citas, descargas) han
       cambiado y las actualizará.

    El sistema utiliza una **estrategia híbrida**: intenta primero Nature.com
    directamente y, si la IP del servidor es bloqueada (caso común en
    despliegues cloud), hace *fallback* automático a la API pública de
    **OpenAlex**.
    """
)

st.markdown("")

btn_col = st.columns([1, 2, 1])[1]
with btn_col:
    do_scrape = st.button(
        "EJECUTAR BÚSQUEDA",
        use_container_width=True, type="primary",
    )

if do_scrape:
    log_box = st.container()
    with log_box:
        with st.spinner("Conectando con Nature Machine Intelligence..."):
            articles_listed, source = list_articles_in_year(current_year)

        st.info(f"`Fuente usada: {source.upper()}` · "
                f"`{len(articles_listed)} artículos vistos`")

        existing_ids = db.get_existing_paper_ids()
        truly_new = [a for a in articles_listed if a["paper_id"] not in existing_ids]

        new_count = 0
        updated_count = 0
        errors = []

        if truly_new:
            st.success(f"Se detectaron **{len(truly_new)}** artículos nuevos.")
            progress = st.progress(0.0, text="Descargando...")
            for i, art in enumerate(truly_new, start=1):
                paper = scrape_article(art)
                if paper:
                    topic = classify_paper(paper)
                    if db.insert_paper(paper, topic):
                        new_count += 1
                else:
                    errors.append(art["paper_id"])
                progress.progress(i / len(truly_new),
                                   text=f"Procesando {i}/{len(truly_new)}: "
                                        f"{art.get('paper_id', '?')}")
            progress.empty()

            st.success(f"**{new_count} artículos** añadidos a la base de datos.")
            if errors:
                st.warning(f"Falló el scraping de: `{', '.join(errors)}`")
        else:
            st.info(
                f"No se encontraron artículos nuevos para {current_year}. "
                "Reconsultando los **5 artículos más recientes** para "
                "actualizar sus métricas..."
            )
            recent_ids = db.get_recent_paper_ids(5)
            progress = st.progress(0.0, text="Verificando...")
            for i, pid in enumerate(recent_ids, start=1):
                paper = scrape_article_by_id(pid)
                if paper and db.update_paper_metrics(pid, paper):
                    updated_count += 1
                elif not paper:
                    errors.append(pid)
                progress.progress(
                    i / len(recent_ids),
                    text=f"Verificando {i}/{len(recent_ids)}: {pid}"
                )
            progress.empty()

            st.success(
                f"**{updated_count}/{len(recent_ids)}** artículos "
                "con métricas actualizadas."
            )
            if errors:
                st.warning(f"No se pudo reconsultar: `{', '.join(errors)}`")

        db.clear_all_caches()

        ui.scan_line()
        st.markdown("### Resumen final")
        rc1, rc2 = st.columns(2)
        with rc1:
            ui.kpi_card("Nuevos artículos", ui.fmt_int(new_count),
                        sub="añadidos a la BD", flavor="success")
        with rc2:
            ui.kpi_card("Métricas actualizadas", ui.fmt_int(updated_count),
                        sub="papers reconsultados", flavor="accent")

        st.info(
            "Navega a otras páginas (Indicadores, Visualizaciones, Explorar) "
            "para ver los cambios reflejados."
        )

ui.scan_line()


with st.expander("Detalles técnicos del scraper", expanded=False):
    st.markdown(
        """
        **Modo de funcionamiento (estrategia híbrida):**

        - **Fuente primaria:** Nature.com — `nature.com/natmachintell/research-articles?year=YYYY`
        - **Fuente fallback:** OpenAlex API — `api.openalex.org/works?filter=primary_location.source.id:...`

        **¿Por qué híbrido?** Nature.com bloquea peticiones desde IPs de
        datacenter (Streamlit Cloud, AWS, GCP, etc.) devolviendo `HTTP 403`.
        OpenAlex no tiene esa restricción y mantiene un catálogo abierto y
        actualizado de todas las publicaciones académicas.

        **Datos obtenidos por cada fuente:**

        | Campo | Nature | OpenAlex |
        |-------|:---:|:---:|
        | Título, autores, abstract | sí | sí |
        | DOI, fecha | sí | sí |
        | Subjects / conceptos | sí | sí |
        | Citas | sí | sí |
        | **Accesses (descargas)** | sí | no |
        | **Altmetric** | sí | no |
        | Referencias en texto plano | sí | no (solo IDs) |

        **Clasificación temática:** se aplica el mismo clasificador del
        Taller 1 (regex jerárquicas sobre título + abstract + subjects).
        Asigna `IA Generativa` > `Machine Learning` > `Estadística` > `Otros`.
        """
    )
