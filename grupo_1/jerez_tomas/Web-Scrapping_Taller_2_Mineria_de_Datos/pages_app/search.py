"""
Página: Buscador (Taller 4)
Sistema de recuperación de información sobre el corpus de Nature MI.

Ofrece tres estrategias (BM25 léxica, LSA semántica con reducción
dimensional, e híbrida RRF) y un modo de comparación lado a lado.

El índice se carga UNA sola vez desde `search_index.joblib` gracias a
@st.cache_resource: la aplicación nunca reconstruye la matriz vectorial
ni reentrena el SVD al hacer una búsqueda.
"""

from pathlib import Path

import streamlit as st

import ui_components as ui
from icons import svg, icon_inline
from shared import init_session_state, render_filters_sidebar

from search_engine import SearchEngine, STRATEGIES

BASE = Path(__file__).resolve().parent.parent
INDEX_PATH = BASE / "search_index.joblib"
DB_PATH = BASE / "revista_q1_2025.sqlite"

init_session_state()
ui.inject_css()
render_filters_sidebar()


# ---------------------------------------------------------------------------
# Carga del índice (una vez por sesión de servidor)
# ---------------------------------------------------------------------------
@st.cache_resource(show_spinner="Cargando índice de búsqueda…")
def get_engine() -> SearchEngine | None:
    """Carga el índice precalculado. Si no existe, lo construye al vuelo
    (fallback de conveniencia; en producción siempre debería existir)."""
    if INDEX_PATH.exists():
        return SearchEngine.load(INDEX_PATH)
    if DB_PATH.exists():
        return SearchEngine.build(DB_PATH, n_components=60)
    return None


# ---------------------------------------------------------------------------
# Estilos propios de esta página
# ---------------------------------------------------------------------------
st.markdown("""
<style>
    .result-card {
        background: linear-gradient(135deg, rgba(26,13,46,.85) 0%, rgba(36,22,64,.92) 100%);
        border: 1px solid #3d1f6b;
        border-left: 3px solid #bd00ff;
        border-radius: 4px;
        padding: 1rem 1.15rem;
        margin-bottom: .85rem;
        transition: all .18s ease;
    }
    .result-card:hover {
        border-color: #bd00ff;
        box-shadow: 0 0 18px rgba(189,0,255,.35);
        transform: translateX(2px);
    }
    .result-rank {
        display: inline-flex; align-items: center; justify-content: center;
        width: 26px; height: 26px; border-radius: 3px;
        background: linear-gradient(135deg,#bd00ff 0%,#7b2cbf 100%);
        color: #0a0612; font-family: 'Orbitron', monospace; font-weight: 700;
        font-size: .82rem; margin-right: .6rem; flex-shrink: 0;
    }
    .result-title {
        color: #e8d8ff; font-family: 'Orbitron', monospace; font-weight: 600;
        font-size: 1rem; line-height: 1.35;
    }
    .result-meta {
        color: #9d8bb8; font-size: .76rem; margin: .5rem 0 .4rem 0;
        letter-spacing: .3px;
    }
    .result-snippet {
        color: #c4b3dd; font-size: .84rem; line-height: 1.5;
        border-left: 2px solid #3d1f6b; padding-left: .7rem; margin-top: .55rem;
    }
    .score-chip {
        display: inline-block; padding: .12rem .55rem; border-radius: 3px;
        background: rgba(0,240,255,.12); border: 1px solid #00f0ff;
        color: #00f0ff; font-size: .74rem; font-weight: 600;
        font-family: 'JetBrains Mono', monospace;
    }
    .topic-chip {
        display: inline-block; padding: .12rem .55rem; border-radius: 3px;
        background: rgba(189,0,255,.12); border: 1px solid #bd00ff;
        color: #bd00ff; font-size: .72rem; font-family: 'JetBrains Mono', monospace;
    }
    .strategy-head {
        color: #00f0ff; font-family: 'Orbitron', monospace; font-weight: 700;
        font-size: .82rem; letter-spacing: 1.5px; text-transform: uppercase;
        border-bottom: 1px solid #3d1f6b; padding-bottom: .4rem; margin-bottom: .8rem;
        text-shadow: 0 0 6px rgba(0,240,255,.5);
    }

</style>
""", unsafe_allow_html=True)


# El chevron de los expanders sufre el mismo problema que los iconos del menú:
# Streamlit lo renderiza como ligadura de la fuente "Material Symbols Rounded",
# que no carga de forma fiable, y entonces se ve el texto crudo
# ("keyboard_arrow_right"). Las reglas CSS propias no ganan a Emotion, así que
# lo resolvemos aplicando estilo INLINE por JavaScript, igual que con el menú.
st.components.v1.html("""
<script>
(function () {
  function fixExpanders() {
    const doc = window.parent.document;
    doc.querySelectorAll('summary [data-testid="stIconMaterial"]').forEach(el => {
      if (el.dataset.chevronFixed === '1') return;
      el.style.cssText = 'font-size:0!important;color:transparent!important;' +
        'width:18px!important;height:18px!important;display:inline-block!important;' +
        'position:relative!important;';
      const glyph = doc.createElement('span');
      glyph.textContent = '\\u25B8';
      glyph.style.cssText = "position:absolute;top:50%;left:50%;" +
        "transform:translate(-50%,-50%);font-size:1rem;color:#bd00ff;" +
        "font-family:'JetBrains Mono',monospace;";
      el.appendChild(glyph);
      const details = el.closest('details');
      if (details) {
        const sync = () => { glyph.textContent = details.open ? '\\u25BE' : '\\u25B8'; };
        details.addEventListener('toggle', sync);
        sync();
      }
      el.dataset.chevronFixed = '1';
    });
  }
  fixExpanders();
  [150, 500, 1200].forEach(ms => setTimeout(fixExpanders, ms));
  try {
    new MutationObserver(fixExpanders)
      .observe(window.parent.document.body, { childList: true, subtree: true });
  } catch (e) { /* sin permisos: no es crítico */ }
})();
</script>
""", height=0)


ui.cyber_header(
    "BUSCADOR // RETRIEVAL",
    "Consulta en lenguaje natural sobre 138 artículos · BM25 · LSA · Híbrida RRF"
)

engine = get_engine()
if engine is None:
    st.error(
        "No se encontró `search_index.joblib` ni `revista_q1_2025.sqlite`. "
        "Ejecuta `python build_index.py` antes de usar el buscador."
    )
    st.stop()

cfg = engine.config


# ---------------------------------------------------------------------------
# Formulario de búsqueda
# ---------------------------------------------------------------------------
EXAMPLES = [
    "protein ligand docking with deep learning",
    "making AI decisions transparent and understandable",
    "drug discovery",
    "E(3)-equivariant interatomic potentials for atomistic simulation",
    "software that writes and understands human text",
]

if "search_query" not in st.session_state:
    st.session_state.search_query = ""

# El text_input NO lleva `key=`. Si la llevara, Streamlit tomaría el estado del
# widget como fuente de verdad y prohibiría modificarlo desde los botones de
# ejemplo (que se renderizan después). Sin key, el valor mostrado lo controla
# `value=` en cada ejecución, así que basta con actualizar `search_query` y
# hacer rerun para que el campo se rellene solo.
#
# Tampoco se usa st.form: la consulta cuesta ~1 ms sobre el índice
# precalculado, de modo que un rerun por pulsación de Enter es asumible y la
# interacción resulta más natural.
query = st.text_input(
    "Consulta",
    value=st.session_state.search_query,
    placeholder="Escribe tu consulta y pulsa Enter — ej: generative models for designing new proteins",
    label_visibility="collapsed",
)
if query != st.session_state.search_query:
    st.session_state.search_query = query

c1, c2 = st.columns([3, 1])
with c1:
    strategy_label = st.selectbox(
        "Estrategia de recuperación",
        options=list(STRATEGIES.values()) + ["Comparar las tres"],
        index=2,
    )
with c2:
    top_k = st.selectbox("Resultados a mostrar", options=[5, 10, 20], index=1)

st.caption("Consultas de ejemplo (las cinco usadas en la evaluación del taller):")
ex_cols = st.columns(len(EXAMPLES))
for col, ex in zip(ex_cols, EXAMPLES):
    with col:
        if st.button(ex[:24] + ("…" if len(ex) > 24 else ""),
                     key=f"ex_{ex[:14]}", use_container_width=True, help=ex):
            st.session_state.search_query = ex
            st.rerun()

active_query = (st.session_state.search_query or "").strip()

label_to_key = {v: k for k, v in STRATEGIES.items()}
compare_mode = strategy_label == "Comparar las tres"
strategy_key = None if compare_mode else label_to_key[strategy_label]

ui.scan_line()


# ---------------------------------------------------------------------------
# Renderizado de resultados
# ---------------------------------------------------------------------------
def render_result(r, compact: bool = False) -> None:
    """Tarjeta de un resultado con los campos exigidos por el taller."""
    authors = r.authors or "—"
    if compact and len(authors) > 60:
        authors = authors.split(";")[0].strip() + " et al."
    elif len(authors) > 150:
        authors = "; ".join(authors.split(";")[:4]).strip() + " et al."

    date = (r.publication_date or "").replace("/", "-")
    link = r.url or (f"https://doi.org/{r.doi}" if r.doi else "")
    doi_html = (f'<a href="{link}" target="_blank" style="color:#00f0ff;'
                f'text-decoration:none">{r.doi or "abrir"} ↗</a>'
                if link else "—")

    snippet = r.snippet or (r.abstract[:220] + "…" if r.abstract else "")
    if compact and len(snippet) > 180:
        snippet = snippet[:180].rsplit(" ", 1)[0] + "…"

    st.markdown(
        f"""
        <div class="result-card">
          <div style="display:flex; align-items:flex-start;">
            <span class="result-rank">{r.rank}</span>
            <span class="result-title">{r.title}</span>
          </div>
          <div class="result-meta">{authors}</div>
          <div class="result-meta">
            <span class="topic-chip">{r.topic_label or "—"}</span>
            &nbsp; {date} &nbsp;·&nbsp; DOI: {doi_html}
            &nbsp;·&nbsp; <span class="score-chip">score {r.score:.4f}</span>
          </div>
          <div class="result-snippet">{snippet}</div>
        </div>
        """,
        unsafe_allow_html=True,
    )


if not active_query:
    st.info(
        "Escribe una consulta en lenguaje natural. El buscador indexa el "
        "**título**, el **resumen** y las **palabras clave** de cada artículo; "
        "no requiere que uses los términos exactos del texto."
    )
else:
    if compare_mode:
        st.markdown(
            f'### {icon_inline("search", f"Comparación de estrategias — {active_query!r}", size=18)}',
            unsafe_allow_html=True,
        )
        st.caption(
            "Los puntajes NO son comparables entre columnas: BM25 vive en "
            "[0, ∞), LSA en [-1, 1] y RRF en [0, ~0.03]. Lo comparable es el "
            "ORDEN de los resultados."
        )
        cols = st.columns(3)
        for col, (key, label) in zip(cols, STRATEGIES.items()):
            with col:
                st.markdown(f'<div class="strategy-head">{label}</div>',
                            unsafe_allow_html=True)
                results = engine.search(active_query, key, top_k=top_k)
                if not results:
                    st.caption("Sin resultados.")
                for r in results:
                    render_result(r, compact=True)
    else:
        results = engine.search(active_query, strategy_key, top_k=top_k)
        st.markdown(
            f'### {icon_inline("search", f"{len(results)} resultados — {STRATEGIES[strategy_key]}", size=18)}',
            unsafe_allow_html=True,
        )
        if not results:
            st.warning(
                "Ningún artículo obtuvo puntaje positivo. Prueba con otros "
                "términos: el corpus cubre IA aplicada a biología, química, "
                "materiales, robótica y modelos de lenguaje."
            )
        for r in results:
            render_result(r)


# ---------------------------------------------------------------------------
# Ficha técnica del índice
# ---------------------------------------------------------------------------
ui.scan_line()
with st.expander("Ficha técnica del sistema de recuperación", expanded=False):
    c1, c2, c3, c4 = st.columns(4)
    with c1:
        ui.kpi_card("Documentos", ui.fmt_int(cfg["n_documents"]),
                    sub="indexados")
    with c2:
        ui.kpi_card("Vocabulario", ui.fmt_int(cfg["tfidf_vocabulary"]),
                    sub="términos TF-IDF", flavor="accent")
    with c3:
        ui.kpi_card("Componentes", ui.fmt_int(cfg["lsa_components"]),
                    sub="del Truncated SVD", flavor="success")
    with c4:
        ui.kpi_card("Varianza", f'{cfg["lsa_explained_variance"]*100:.1f}%',
                    sub="explicada por LSA", flavor="magenta")

    st.markdown(f"""
| Aspecto | Detalle |
|---|---|
| **Campos indexados** | Título (×{cfg['title_boost']}), resumen, palabras clave (×{cfg['subjects_boost']}) |
| **Procesamiento** | Minúsculas · normalización NFKD · tokenización por regex · stopwords (inglés + retórica académica) · stemming Snowball |
| **Representación léxica** | Bolsa de palabras, {cfg['bm25_vocabulary']:,} términos |
| **Representación semántica** | TF-IDF {cfg['tfidf_shape'][0]}×{cfg['tfidf_shape'][1]} (densidad {cfg['tfidf_density']*100:.2f}%) → Truncated SVD de {cfg['lsa_components']} dims |
| **Reducción dimensional** | {cfg['tfidf_shape'][1]} → {cfg['lsa_components']} ({cfg['tfidf_shape'][1]/cfg['lsa_components']:.1f}× menos dimensiones) |
| **BM25** | k₁ = {cfg['bm25_k1']}, b = {cfg['bm25_b']}, IDF de Robertson |
| **Fusión híbrida** | Reciprocal Rank Fusion, k = 60 |
| **Desempate** | Puntaje → citas (desc.) → paper_id (asc.) |

**Cómo decide cada método que un artículo es relevante**

- **BM25** cuenta coincidencias exactas de términos, penaliza los términos
  frecuentes en el corpus (IDF), satura la contribución de repetir mucho un
  mismo término (k₁) y normaliza por longitud del documento (b). No entiende
  sinónimos: si la palabra no está, el artículo no puntúa.
- **LSA** proyecta documentos y consulta a un espacio latente de
  {cfg['lsa_components']} dimensiones obtenido por SVD. Cada dimensión es una
  combinación de términos que co-ocurren, así que dos textos pueden parecerse
  aunque no compartan ninguna palabra. La similitud es el coseno.
- **RRF** ignora los puntajes y suma el inverso de las posiciones que cada
  método asignó, de modo que premia a los documentos bien rankeados por ambos.
""")
