import streamlit as st
import pandas as pd
import plotly.graph_objects as go

# --- Configuración de la página ---
st.set_page_config(page_title="Comparador de Distribuciones", layout="wide")

# --- 1. Inyección de CSS Personalizado ---
st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500&family=Poppins:ital,wght@0,400;0,600;0,700;1,500&display=swap');
    
    /* Aplicar fuente general */
    html, body, [class*="css"] {
        font-family: 'Inter', sans-serif;
    }
    
    /* Estilo de la portada (Cover Page) */
    .cover-page {
        background: linear-gradient(135deg, #4834D4 0%, #686DE0 50%, #22A6B3 100%);
        color: white;
        padding: 40px 30px;
        border-radius: 16px;
        margin-bottom: 30px;
        box-shadow: 0 10px 25px rgba(72, 52, 212, 0.15);
        text-align: center;
    }
    
    .cover-page h1 {
        font-family: 'Poppins', sans-serif;
        color: white;
        font-size: 2.5rem;
        margin-bottom: 10px;
        font-weight: 700;
    }
    
    .cover-page p {
        font-size: 1.1rem;
        opacity: 0.9;
    }
    </style>
    """, unsafe_allow_html=True)

# --- 2. Portada ---
st.markdown("""
    <div class="cover-page">
        <h1>Comparador de Distribuciones</h1>
        <p>Sube dos archivos CSV para comparar la distribución de sus variables numéricas</p>
    </div>
""", unsafe_allow_html=True)

# --- 3. Carga de Archivos ---
uploaded_files = st.file_uploader(
    "Sube exactamente 2 archivos CSV", 
    type=["csv"], 
    accept_multiple_files=True
)

if not uploaded_files:
    st.info("Sube archivos CSV para comenzar la magia.")
elif len(uploaded_files) == 1:
    st.warning(" Has subido solo un archivo. Falta uno más para comparar.")
elif len(uploaded_files) > 2:
    st.warning(" Has subido más de dos archivos. Solo usaremos los dos primeros.")
    uploaded_files = uploaded_files[:2]

# --- 4. Procesamiento y Visualización ---
if uploaded_files and len(uploaded_files) == 2:
    
    df1 = pd.read_csv(uploaded_files[0])
    df2 = pd.read_csv(uploaded_files[1])
    
    nombre1 = uploaded_files[0].name
    nombre2 = uploaded_files[1].name

    num_cols1 = set(df1.select_dtypes(include=['number']).columns)
    num_cols2 = set(df2.select_dtypes(include=['number']).columns)
    columnas_comunes = list(num_cols1.intersection(num_cols2))

    if not columnas_comunes:
        st.error("No se encontraron variables numéricas en común.")
    else:
        st.divider() # Línea divisoria elegante
        
        # Dividimos la pantalla: 1/3 para controles, 2/3 para el gráfico
        col_controles, col_grafico = st.columns([1, 2])
        
        with col_controles:
            st.subheader("⚙️ Configuración")
            variable_seleccionada = st.selectbox(
                "Selecciona la variable a comparar:", 
                options=sorted(columnas_comunes)
            )
            
            # Tarjetas de estadísticas elegantes usando st.metric
            st.markdown("### 📊 Estadísticas Rápidas")
            
            st.markdown(f"**{nombre1}**")
            m1, m2, m3 = st.columns(3)
            m1.metric("Media", f"{df1[variable_seleccionada].mean():.2f}")
            m2.metric("Mediana", f"{df1[variable_seleccionada].median():.2f}")
            m3.metric("Desv. Est", f"{df1[variable_seleccionada].std():.2f}")
            
            st.markdown(f"**{nombre2}**")
            m4, m5, m6 = st.columns(3)
            m4.metric("Media", f"{df2[variable_seleccionada].mean():.2f}")
            m5.metric("Mediana", f"{df2[variable_seleccionada].median():.2f}")
            m6.metric("Desv. Est", f"{df2[variable_seleccionada].std():.2f}")

with col_grafico:
            fig = go.Figure()
            
            # Añadimos nbinsx para forzar que los datos se agrupen en "cajas" más anchas
            fig.add_trace(go.Histogram(
                x=df1[variable_seleccionada], 
                name=nombre1,
                opacity=0.6, 
                marker_color='#4834D4',
                nbinsx=8 # Agrupa en aprox 8 columnas
            ))
            
            fig.add_trace(go.Histogram(
                x=df2[variable_seleccionada], 
                name=nombre2,
                opacity=0.6, 
                marker_color='#22A6B3',
                nbinsx=8 # Agrupa en aprox 8 columnas
            ))
            
            # barmode='overlay' es el encargado de que se encimen
            fig.update_layout(
                barmode='overlay', 
                title=dict(text=f"Distribución superpuesta: {variable_seleccionada}", font=dict(family="Poppins")),
                xaxis_title=variable_seleccionada,
                yaxis_title="Frecuencia",
                legend=dict(x=0.01, y=0.99, bgcolor='rgba(255,255,255,0.8)'),
                margin=dict(l=0, r=0, t=40, b=0) 
            )
            
            st.plotly_chart(fig, use_container_width=True)