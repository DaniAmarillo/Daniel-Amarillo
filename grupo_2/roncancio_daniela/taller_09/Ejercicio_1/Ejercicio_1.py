import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go

# ── Page config ────────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="Ejercicio 1: Comparador de Distribuciones",
    page_icon="📊",
    layout="wide",
)

# ── Custom CSS ─────────────────────────────────────────────────────────────────

st.markdown("""
<style>
@import url('https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=DM+Sans:wght@300;400;600&display=swap');
 
html, body, [class*="css"] {
    font-family: 'DM Sans', sans-serif;
}
 
h1, h2, h3 {
    font-family: 'Space Mono', monospace !important;
}
 
.stApp {
    background: #0d1117;
    color: #e6edf3;
}
 
/* Header banner */
.header-banner {
    background: linear-gradient(135deg, #1a1f2e 0%, #0d1117 50%, #111827 100%);
    border: 1px solid #30363d;
    border-radius: 12px;
    padding: 2rem 2.5rem;
    margin-bottom: 2rem;
    position: relative;
    overflow: hidden;
}
.header-banner::before {
    content: '';
    position: absolute;
    top: -50%;
    right: -10%;
    width: 300px;
    height: 300px;
    background: radial-gradient(circle, rgba(88,166,255,0.08) 0%, transparent 70%);
    pointer-events: none;
}
.header-title {
    font-family: 'Space Mono', monospace;
    font-size: 1.8rem;
    font-weight: 700;
    color: #e6edf3;
    margin: 0 0 0.4rem 0;
    letter-spacing: -0.5px;
}
.header-sub {
    color: #8b949e;
    font-size: 0.95rem;
    margin: 0;
}
 
/* Upload cards */
.upload-label {
    font-family: 'Space Mono', monospace;
    font-size: 0.75rem;
    font-weight: 700;
    letter-spacing: 2px;
    text-transform: uppercase;
    margin-bottom: 0.5rem;
    display: block;
}
.label-a { color: #58a6ff; }
.label-b { color: #f78166; }
 
/* Stats table */
.stats-card {
    background: #161b22;
    border: 1px solid #30363d;
    border-radius: 10px;
    padding: 1.5rem;
}
.stats-title {
    font-family: 'Space Mono', monospace;
    font-size: 0.7rem;
    letter-spacing: 2px;
    text-transform: uppercase;
    color: #8b949e;
    margin-bottom: 1rem;
}
 
/* Info box */
.info-box {
    background: #161b22;
    border: 1px solid #30363d;
    border-left: 3px solid #58a6ff;
    border-radius: 8px;
    padding: 1rem 1.2rem;
    font-size: 0.88rem;
    color: #8b949e;
    margin-top: 1rem;
}
 
/* Metric override */
div[data-testid="metric-container"] {
    background: #161b22;
    border: 1px solid #30363d;
    border-radius: 8px;
    padding: 0.8rem 1rem;
}
div[data-testid="metric-container"] label {
    color: #8b949e !important;
    font-size: 0.75rem !important;
    font-family: 'Space Mono', monospace !important;
    letter-spacing: 1px;
    text-transform: uppercase;
}
div[data-testid="metric-container"] div[data-testid="metric-value"] {
    color: #e6edf3 !important;
    font-family: 'Space Mono', monospace !important;
}
 
/* Selectbox label */
.stSelectbox label {
    font-family: 'Space Mono', monospace !important;
    font-size: 0.78rem !important;
    color: #8b949e !important;
    letter-spacing: 1px;
    text-transform: uppercase;
}
 
/* Divider */
hr { border-color: #30363d; }
</style>
""", unsafe_allow_html=True)

 # ── Header ─────────────────────────────────────────────────────────────────────
st.markdown("""
<div class="header-banner">
    <p class="header-title">📊 Comparador de Distribuciones</p>
    <p class="header-sub">Carga dos datasets CSV y compara la distribución de cualquier variable numérica lado a lado.</p>
</div>
""", unsafe_allow_html=True)

# ── File uploaders ─────────────────────────────────────────────────────────────
col_up1, col_up2 = st.columns(2)
 
with col_up1:
    st.markdown('<span class="upload-label label-a">▶ Dataset A</span>', unsafe_allow_html=True)
    file_a = st.file_uploader("Dataset A", type=["csv"], key="file_a", label_visibility="collapsed")
 
with col_up2:
    st.markdown('<span class="upload-label label-b">▶ Dataset B</span>', unsafe_allow_html=True)
    file_b = st.file_uploader("Dataset B", type=["csv"], key="file_b", label_visibility="collapsed")

# ── Load dataframes ─────────────────────────────────────────────────────────────
df_a = pd.read_csv(file_a, sep=None, engine="python") if file_a else None
df_b = pd.read_csv(file_b, sep=None, engine="python") if file_b else None

    # ── Preview when only one file is loaded ───────────────────────────────────────
if (df_a is not None) != (df_b is not None):
    solo_df = df_a if df_a is not None else df_b
    solo_label = "A" if df_a is not None else "B"
    st.markdown(f"#### Vista previa — Dataset {solo_label}")
    st.dataframe(solo_df.head(10), use_container_width=True)
 
# ── Main comparison (both files loaded) ────────────────────────────────────────
if df_a is not None and df_b is not None:
 
    # Find common numeric columns
    num_a = set(df_a.select_dtypes(include="number").columns)
    num_b = set(df_b.select_dtypes(include="number").columns)
    common_cols = sorted(num_a & num_b)
 
    if not common_cols:
        st.error("⚠️ Los dos datasets no comparten columnas numéricas en común.")
        st.stop()
 
    st.markdown("---")
 
    # Variable selector + bin slider
    sel_col1, sel_col2 = st.columns([2, 1])
    with sel_col1:
        variable = st.selectbox("Variable a comparar", common_cols)
    with sel_col2:
        n_bins = st.slider("Número de bins", min_value=10, max_value=100, value=40, step=5)
 
    # ── Histogram ──────────────────────────────────────────────────────────────
    series_a = df_a[variable].dropna()
    series_b = df_b[variable].dropna()
 
    fig = go.Figure()
 
    fig.add_trace(go.Histogram(
        x=series_a,
        name="Dataset A",
        nbinsx=n_bins,
        opacity=0.6,
        marker_color="#58a6ff",
        marker_line=dict(color="#1f6feb", width=0.5),
    ))
 
    fig.add_trace(go.Histogram(
        x=series_b,
        name="Dataset B",
        nbinsx=n_bins,
        opacity=0.6,
        marker_color="#f78166",
        marker_line=dict(color="#da3633", width=0.5),
    ))
 
    fig.update_layout(
        barmode="overlay",
        title=dict(
            text=f"Distribución de <b>{variable}</b>",
            font=dict(family="Space Mono", size=15, color="#e6edf3"),
            x=0.01,
        ),
        paper_bgcolor="#161b22",
        plot_bgcolor="#0d1117",
        font=dict(family="DM Sans", color="#8b949e"),
        legend=dict(
            bgcolor="#161b22",
            bordercolor="#30363d",
            borderwidth=1,
            font=dict(color="#e6edf3"),
        ),
        xaxis=dict(
            title=variable,
            gridcolor="#21262d",
            linecolor="#30363d",
            tickfont=dict(color="#8b949e"),
        ),
        yaxis=dict(
            title="Frecuencia",
            gridcolor="#21262d",
            linecolor="#30363d",
            tickfont=dict(color="#8b949e"),
        ),
        margin=dict(l=20, r=20, t=50, b=20),
        height=420,
    )
 
    st.plotly_chart(fig, use_container_width=True)
 
    # ── Comparative stats table ────────────────────────────────────────────────
    st.markdown("---")
    st.markdown("#### Estadísticas comparativas")
 
    def stats_dict(s, label):
        return {
            "Dataset": label,
            "N": f"{len(s):,}",
            "Media": f"{s.mean():.4f}",
            "Mediana": f"{s.median():.4f}",
            "Desv. Estándar": f"{s.std():.4f}",
            "Mínimo": f"{s.min():.4f}",
            "Máximo": f"{s.max():.4f}",
            "Q1 (25%)": f"{s.quantile(0.25):.4f}",
            "Q3 (75%)": f"{s.quantile(0.75):.4f}",
        }
 
    stats_df = pd.DataFrame([stats_dict(series_a, "Dataset A"), stats_dict(series_b, "Dataset B")])
    stats_df = stats_df.set_index("Dataset")
 
    st.dataframe(
        stats_df.style.set_properties(**{
            "background-color": "#161b22",
            "color": "#e6edf3",
            "border-color": "#30363d",
        }),
        use_container_width=True,
    )
 
# ── No files loaded ─────────────────────────────────────────────────────────────
if df_a is None and df_b is None:
    st.markdown("""
    <div class="info-box" style="text-align:center; padding: 2.5rem; border-left: 3px solid #30363d;">
        <p style="font-size:2rem; margin:0">📂</p>
        <p style="color:#e6edf3; font-family:'Space Mono',monospace; margin: 0.5rem 0 0.25rem 0;">
            Carga dos archivos CSV para comenzar
        </p>
        <p style="margin:0; font-size:0.85rem;">
            Usa los selectores de arriba para cargar Dataset A y Dataset B simultáneamente.
        </p>
    </div>
    """, unsafe_allow_html=True)
 
