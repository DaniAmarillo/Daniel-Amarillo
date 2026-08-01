import streamlit as st
import numpy as np
import plotly.graph_objects as go
import scipy.stats as stats

# --- Configuración de la página ---
st.set_page_config(page_title="Calculadora de Tamaño Muestral", layout="wide")

# --- CSS Avanzado para estética Premium ---
st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@500;600;700&display=swap');
    html, body, [class*="css"] { font-family: 'Inter', sans-serif; }
    .cover-page {
        background: linear-gradient(135deg, #10ac84 0%, #1dd1a1 100%);
        color: white; padding: 25px 30px; border-radius: 12px; margin-bottom: 25px;
        box-shadow: 0 8px 20px rgba(16, 172, 132, 0.2);
    }
    .cover-page h1 { font-family: 'Poppins', sans-serif; color: white; font-weight: 700; margin: 0 0 10px 0; font-size: 2.2rem;}
    .cover-page p { margin: 0; opacity: 0.9; font-size: 1.05rem; }
    
    /* Hacer el st.metric mucho más grande y destacado */
    div[data-testid="stMetricValue"] {
        font-size: 4rem !important;
        color: #10ac84;
        font-family: 'Poppins', sans-serif;
        font-weight: 700;
    }
    div[data-testid="stMetricLabel"] {
        font-size: 1.2rem !important;
        font-weight: 600;
        color: #2f3640;
    }
    </style>
""", unsafe_allow_html=True)

# --- Encabezado ---
st.markdown("""
    <div class="cover-page">
        <h1>Calculadora de Tamaño Muestral</h1>
        <p>Herramienta para estimar proporciones con precisión estadística.</p>
    </div>
""", unsafe_allow_html=True)

# --- 1. Panel Lateral (Sidebar) para Controles ---
with st.sidebar:
    st.header("Parámetros del Estudio")
    
    # Mapeo de porcentajes a valores decimales para nivel de confianza
    dict_confianza = {"90%": 0.90, "95%": 0.95, "99%": 0.99}
    nivel_confianza_str = st.radio("1. Nivel de Confianza:", options=list(dict_confianza.keys()), index=1) # Por defecto 95%
    nivel_confianza = dict_confianza[nivel_confianza_str]
    
    st.divider()
    
    p_esperada = st.slider("2. Proporción Esperada (p)", min_value=0.01, max_value=0.99, value=0.50, step=0.01, 
                           help="Si no tienes idea, déjalo en 0.50 (50%) que es el escenario más conservador (varianza máxima).")
    
    st.divider()
    
    margen_error = st.slider("3. Margen de Error (E)", min_value=0.01, max_value=0.20, value=0.05, step=0.01,
                             help="El error máximo aceptable (Ej. 0.05 = ±5%).")

# --- 2. Función de Cálculo Principal ---
def calcular_n(p, e, conf):
    # Nivel de significancia (alpha)
    alpha = 1 - conf
    # Valor Z usando la función cuantil (ppf) de la distribución normal estándar
    z = stats.norm.ppf(1 - alpha/2)
    # Fórmula del tamaño muestral
    n = (z**2 * p * (1 - p)) / (e**2)
    return np.ceil(n) # Siempre redondeamos hacia arriba para asegurar la muestra

# Calculamos el valor actual con los sliders
n_calculado = calcular_n(p_esperada, margen_error, nivel_confianza)

# --- 3. Resultado Destacado (st.metric) ---
col1, col2, col3 = st.columns([1, 2, 1])
with col2:
    st.markdown("<div style='text-align: center;'>", unsafe_allow_html=True)
    st.metric(label="Muestra Necesaria (Personas/Casos)", value=f"{int(n_calculado):,}".replace(",", "."))
    st.markdown("</div>", unsafe_allow_html=True)

st.divider()

# --- 4. Sistema de Pestañas (Tabs) ---
tab1, tab2, tab3 = st.tabs(["📉 Curva de Error", "📊 Curva de Proporción", "🧮 Entendiendo la Fórmula"])

# --- Pestaña 1: Sensibilidad del Margen de Error ---
with tab1:
    st.subheader("Sensibilidad: Tamaño Muestral vs Margen de Error")
    st.write(f"Manteniendo la proporción en **{p_esperada}** y la confianza en **{nivel_confianza_str}**.")
    
    # Generar datos simulando distintos márgenes de error
    errores_sim = np.linspace(0.01, 0.20, 50)
    n_sim_error = [calcular_n(p_esperada, e, nivel_confianza) for e in errores_sim]
    
    fig1 = go.Figure()
    fig1.add_trace(go.Scatter(x=errores_sim, y=n_sim_error, mode='lines', name='Curva', line=dict(color='#10ac84', width=3)))
    
    # Añadir un punto rojo marcando exactamente dónde está el usuario ahora mismo
    fig1.add_trace(go.Scatter(x=[margen_error], y=[n_calculado], mode='markers', name='Tu selección', 
                              marker=dict(color='#e55039', size=12, symbol='star')))
    
    fig1.update_layout(xaxis_title="Margen de Error (E)", yaxis_title="Tamaño Muestral (n)", 
                       plot_bgcolor='rgba(0,0,0,0)', hovermode="x unified")
    fig1.update_xaxes(showgrid=True, gridcolor='#f1f2f6')
    fig1.update_yaxes(showgrid=True, gridcolor='#f1f2f6')
    st.plotly_chart(fig1, use_container_width=True)

# --- Pestaña 2: Sensibilidad de la Proporción ---
with tab2:
    st.subheader("Sensibilidad: Tamaño Muestral vs Proporción Esperada")
    st.write(f"Manteniendo el margen de error en **{margen_error}** y la confianza en **{nivel_confianza_str}**.")
    
    # Generar datos simulando distintas proporciones
    props_sim = np.linspace(0.01, 0.99, 50)
    n_sim_prop = [calcular_n(p, margen_error, nivel_confianza) for p in props_sim]
    
    fig2 = go.Figure()
    fig2.add_trace(go.Scatter(x=props_sim, y=n_sim_prop, mode='lines', line=dict(color='#341f97', width=3)))
    
    # Punto rojo marcando la selección actual
    fig2.add_trace(go.Scatter(x=[p_esperada], y=[n_calculado], mode='markers', name='Tu selección', 
                              marker=dict(color='#e55039', size=12, symbol='star')))
    
    fig2.update_layout(xaxis_title="Proporción Esperada (p)", yaxis_title="Tamaño Muestral (n)", 
                       plot_bgcolor='rgba(0,0,0,0)', hovermode="x unified", showlegend=False)
    fig2.update_xaxes(showgrid=True, gridcolor='#f1f2f6')
    fig2.update_yaxes(showgrid=True, gridcolor='#f1f2f6')
    st.plotly_chart(fig2, use_container_width=True)

# --- Pestaña 3: Explicación y LaTeX ---
with tab3:
    st.subheader("La Matemática detrás de la Magia")
    st.write("La fórmula utilizada asume una población infinita (o muy grande) para estimar una proporción. Esta es la ecuación exacta:")
    
    # st.latex renderiza matemáticas hermosas!
    st.latex(r'''
        n = \frac{Z^2 \cdot p(1-p)}{E^2}
    ''')
    
    st.markdown("""
    **Donde:**
    * $n$ = Tamaño de muestra necesario.
    * $Z$ = Valor Z asociado al nivel de confianza elegido (calculado dinámicamente con `scipy.stats.norm.ppf`).
    * $p$ = Proporción esperada del evento (varianza máxima en 0.50).
    * $E$ = Margen de error tolerado.
    
    *Nota: En estadística siempre redondeamos **hacia arriba** (función techo / `np.ceil`) porque no puedes encuestar a una fracción de persona y redondear hacia abajo aumentaría tu error marginal.*
    """)