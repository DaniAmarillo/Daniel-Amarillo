import streamlit as st
import numpy as np
import plotly.graph_objects as go
from scipy.stats import norm

# ── Configuración ──────────────────────────────────────────────────────────────
st.set_page_config(page_title="Tamaño Muestral", page_icon="🔢", layout="wide")

st.title("🔢 Calculadora de Tamaño Muestral")
st.caption("Estima la proporción con distintos niveles de confianza y margen de error.")

# ── Fórmula ──────────────────────s──────────────────────────────────────────────
def calcular_n(p, e, z):
    return int(np.ceil((z**2 * p * (1 - p)) / e**2))

# ── Sidebar ────────────────────────────────────────────────────────────────────
st.sidebar.header("Parámetros")

p = st.sidebar.slider("Proporción esperada (p)", 0.01, 0.99, 0.50, 0.01)
e = st.sidebar.slider("Margen de error (e)", 0.01, 0.20, 0.05, 0.01)
nc = st.sidebar.radio("Nivel de confianza", ["90%", "95%", "99%"], index=1)

nivel = int(nc[:-1]) / 100
z = norm.ppf(1 - (1 - nivel) / 2)
n = calcular_n(p, e, z)

# ── Métrica principal ──────────────────────────────────────────────────────────
st.markdown("---")
col1, col2, col3 = st.columns(3)
col1.metric("Tamaño de muestra (n)", f"{n:,}")
col2.metric("Nivel de confianza", nc)
col3.metric("Margen de error", f"{e:.0%}")

# ── Pestañas ───────────────────────────────────────────────────────────────────
st.markdown("---")
tab1, tab2, tab3 = st.tabs(["📉 n vs Margen de error", "📈 n vs Proporción", "📐 Fórmula"])

# Pestaña 1: n vs margen de error
with tab1:
    errores = np.linspace(0.01, 0.20, 100)
    ns = [calcular_n(p, ei, z) for ei in errores]

    fig1 = go.Figure()
    fig1.add_trace(go.Scatter(
        x=errores, y=ns,
        mode="lines", line=dict(color="#58a6ff", width=2),
        fill="tozeroy", fillcolor="rgba(88,166,255,0.07)"
    ))
    fig1.add_vline(x=e, line=dict(color="orange", dash="dash", width=1.5),
                   annotation_text=f"e = {e:.2f}", annotation_position="top right")
    fig1.update_layout(
        xaxis_title="Margen de error (e)",
        yaxis_title="Tamaño de muestra (n)",
        height=380, margin=dict(l=10, r=10, t=30, b=10)
    )
    st.plotly_chart(fig1, use_container_width=True)

# Pestaña 2: n vs proporción
with tab2:
    proporciones = np.linspace(0.01, 0.99, 100)
    ns2 = [calcular_n(pi, e, z) for pi in proporciones]

    fig2 = go.Figure()
    fig2.add_trace(go.Scatter(
        x=proporciones, y=ns2,
        mode="lines", line=dict(color="#3fb950", width=2),
        fill="tozeroy", fillcolor="rgba(63,185,80,0.07)"
    ))
    fig2.add_vline(x=p, line=dict(color="orange", dash="dash", width=1.5),
                   annotation_text=f"p = {p:.2f}", annotation_position="top right")
    fig2.update_layout(
        xaxis_title="Proporción esperada (p)",
        yaxis_title="Tamaño de muestra (n)",
        height=380, margin=dict(l=10, r=10, t=30, b=10)
    )
    st.plotly_chart(fig2, use_container_width=True)

# Pestaña 3: fórmula LaTeX
with tab3:
    st.markdown("### Fórmula utilizada")
    st.latex(r"n = \frac{z^2 \cdot p \cdot (1 - p)}{e^2}")

    st.markdown("### Valores actuales")
    st.latex(rf"n = \frac{{{z}^2 \cdot {p} \cdot (1 - {p})}}{{{e}^2}} = {n}")

    st.markdown("### Donde")
    st.markdown(f"""
    - **z** = {z} → valor crítico para un nivel de confianza del {nc}  
    - **p** = {p} → proporción esperada  
    - **e** = {e} → margen de error aceptado  
    - **n** = {n:,} → tamaño de muestra necesario  
    """)