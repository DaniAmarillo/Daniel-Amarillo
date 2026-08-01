import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from datetime import timedelta

# --- Configuración de la página ---
st.set_page_config(page_title="Explorador de Series de Tiempo", layout="wide")

# --- CSS Avanzado para estética Premium ---
st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@500;600;700&display=swap');
    
    html, body, [class*="css"] { font-family: 'Inter', sans-serif; }
    
    /* Portada Premium */
    .cover-page {
        background: linear-gradient(135deg, #1e3799 0%, #4a69bd 100%);
        color: white; padding: 25px 30px; border-radius: 12px; margin-bottom: 25px;
        box-shadow: 0 8px 20px rgba(30, 55, 153, 0.2);
    }
    .cover-page h1 { font-family: 'Poppins', sans-serif; color: white; font-weight: 700; margin: 0 0 10px 0; font-size: 2.2rem;}
    .cover-page p { margin: 0; opacity: 0.9; font-size: 1.05rem; }
    
    /* Estilizar las tarjetas de métricas */
    div[data-testid="stMetric"] {
        background-color: #f8f9fa;
        border-left: 5px solid #4a69bd;
        padding: 15px 20px;
        border-radius: 8px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.04);
        transition: transform 0.2s ease;
    }
    div[data-testid="stMetric"]:hover {
        transform: translateY(-2px);
    }
    </style>
""", unsafe_allow_html=True)

# --- Encabezado ---
st.markdown("""
    <div class="cover-page">
        <h1>Explorador Temporal </h1>
        <p>Sube tu dataset, descubre patrones visuales.</p>
    </div>
""", unsafe_allow_html=True)

# --- Inicializar la "Memoria" ---
if 'anotaciones' not in st.session_state:
    st.session_state.anotaciones = []

# --- 1. Carga del Archivo ---
uploaded_file = st.file_uploader("Sube un archivo CSV con tus datos temporales", type=["csv"])

if not uploaded_file:
    st.info("Esperando archivo CSV...")
else:
    df = pd.read_csv(uploaded_file)
    
    # --- 2. Configuración de Columnas ---
    with st.expander("Configuración de Datos (Abre para cambiar variables)", expanded=True):
        col1, col2 = st.columns(2)
        with col1:
            columna_fecha = st.selectbox("Columna de Fechas:", options=df.columns)
        with col2:
            columnas_numericas = df.select_dtypes(include=['number']).columns
            columna_valor = st.selectbox("Columna de Valores:", options=columnas_numericas)

    try:
        df[columna_fecha] = pd.to_datetime(df[columna_fecha])
        conversion_exitosa = True
    except Exception:
        conversion_exitosa = False
        st.error("La columna seleccionada no tiene un formato de fecha válido.")

    if conversion_exitosa:
        # --- 3. Interfaz Lateral (Sidebar) ---
        with st.sidebar:
            st.markdown("### Filtros y Herramientas")
            
            st.markdown("**1. Rango de Visualización**")
            min_date = df[columna_fecha].min().date()
            max_date = df[columna_fecha].max().date()
            
            rango_fechas = st.date_input(
                "Selecciona el periodo:",
                value=(min_date, max_date),
                min_value=min_date, max_value=max_date
            )
            
            st.divider()
            
            # --- FORMULARIO DE ANOTACIONES ACTUALIZADO ---
            st.markdown("**2. Nueva Anotación**")
            with st.form("form_anotar", clear_on_submit=True):
                fecha_anotacion = st.date_input("Fecha exacta:", min_value=min_date, max_value=max_date)
                texto_anotacion = st.text_input("Describe el evento:")
                
                # ¡Nuevo componente: Selector de Color!
                color_anotacion = st.color_picker("Elige un color para la etiqueta:", "#e55039")
                
                submit_btn = st.form_submit_button("📌 Fijar en el Gráfico", use_container_width=True)
                
                if submit_btn and texto_anotacion:
                    # Guardamos también el color en la memoria
                    st.session_state.anotaciones.append({
                        'fecha': fecha_anotacion, 
                        'texto': texto_anotacion,
                        'color': color_anotacion
                    })
                    st.success("Guardado!")
                    st.rerun()

        # --- 4. Filtrado de Datos ---
        if len(rango_fechas) == 2:
            fecha_inicio, fecha_fin = rango_fechas
            mask = (df[columna_fecha].dt.date >= fecha_inicio) & (df[columna_fecha].dt.date <= fecha_fin)
            df_filtrado = df.loc[mask]

            # --- 5. Dashboard Superior ---
            st.markdown("<br>", unsafe_allow_html=True)
            m1, m2, m3, m4 = st.columns(4)
            m1.metric("Registros Visibles", f"{len(df_filtrado)}")
            m2.metric(f"Promedio ({columna_valor})", f"{df_filtrado[columna_valor].mean():.2f}")
            m3.metric("Valor Máximo", f"{df_filtrado[columna_valor].max():.2f}")
            m4.metric("Valor Mínimo", f"{df_filtrado[columna_valor].min():.2f}")
            st.markdown("<br>", unsafe_allow_html=True)

            # --- 6. Gráfico Principal ---
            fig = go.Figure()
            
            fig.add_trace(go.Scatter(
                x=df_filtrado[columna_fecha], 
                y=df_filtrado[columna_valor],
                mode='lines',
                name=columna_valor,
                line=dict(color='#4a69bd', width=2.5),
                fill='tozeroy', 
                fillcolor='rgba(74, 105, 189, 0.15)' 
            ))

            valor_maximo_grafico = df_filtrado[columna_valor].max()
            
            # --- APLICACIÓN DINÁMICA DE COLORES EN EL GRÁFICO ---
            for ann in st.session_state.anotaciones:
                if fecha_inicio <= ann['fecha'] <= fecha_fin:
                    # Obtenemos el color (o usamos rojo por defecto si hay notas viejas guardadas)
                    color_etiqueta = ann.get('color', '#e55039')
                    
                    fig.add_vline(x=ann['fecha'], line_dash="dot", line_color=color_etiqueta, opacity=0.9, line_width=2)
                    fig.add_annotation(
                        x=ann['fecha'], y=valor_maximo_grafico, text=ann['texto'],
                        showarrow=True, arrowhead=2, arrowcolor=color_etiqueta, arrowsize=1.5,
                        font=dict(color="white", family="Inter", size=11), 
                        bgcolor=color_etiqueta, borderpad=5, borderwidth=1, bordercolor="white"
                    )

            fig.update_layout(
                title=dict(text=f"Evolución: {columna_valor}", font=dict(family="Poppins", size=18, color="#2f3640")),
                xaxis_title="", 
                yaxis_title=columna_valor,
                plot_bgcolor='rgba(0,0,0,0)',
                paper_bgcolor='rgba(0,0,0,0)',
                margin=dict(l=10, r=10, t=50, b=10),
                hovermode="x unified", 
                xaxis=dict(showgrid=False, linecolor='#dcdde1'),
                yaxis=dict(showgrid=True, gridcolor='#f1f2f6', linecolor='#dcdde1')
            )
            
            st.plotly_chart(fig, use_container_width=True)

            # --- 7. Gestor de Anotaciones ---
            st.markdown("<hr style='margin-top: 10px; margin-bottom: 20px; opacity: 0.3;'>", unsafe_allow_html=True)
            with st.expander("Gestión de Anotaciones", expanded=False):
                if not st.session_state.anotaciones:
                    st.info("No hay eventos registrados en la memoria.")
                else:
                    for i, ann in enumerate(st.session_state.anotaciones):
                        col_a, col_b, col_c = st.columns([2, 6, 2])
                        color_etiqueta = ann.get('color', '#e55039')
                        
                        col_a.write(f"📅 **{ann['fecha']}**")
                        # Usamos HTML para mostrar un puntito con el color seleccionado
                        col_b.markdown(f"<span style='color:{color_etiqueta}'>⬤</span> 📌 {ann['texto']}", unsafe_allow_html=True)
                        
                        if col_c.button("Eliminar", key=f"del_{i}", use_container_width=True):
                            st.session_state.anotaciones.pop(i)
                            st.rerun()
        else:
            st.warning("Selecciona un rango de fechas válido en el panel lateral.")