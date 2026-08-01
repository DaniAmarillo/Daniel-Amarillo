import streamlit as st
import pandas as pd
import plotly.graph_objects as go

# --- Configuración de la página ---
st.set_page_config(page_title="Data Cleaner", layout="wide")

# --- CSS Avanzado ---
st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@500;600;700&display=swap');
    html, body, [class*="css"] { font-family: 'Inter', sans-serif; }
    .cover-page {
        background: linear-gradient(135deg, #ff9f43 0%, #ee5253 100%);
        color: white; padding: 25px 30px; border-radius: 12px; margin-bottom: 25px;
        box-shadow: 0 8px 20px rgba(238, 82, 83, 0.2);
    }
    .cover-page h1 { font-family: 'Poppins', sans-serif; color: white; font-weight: 700; margin: 0 0 10px 0; font-size: 2.2rem;}
    .cover-page p { margin: 0; opacity: 0.9; font-size: 1.05rem; }
    </style>
""", unsafe_allow_html=True)

# --- Encabezado ---
st.markdown("""
    <div class="cover-page">
        <h1> Data Cleaner</h1>
        <p>Detecta valores faltantes (NaN), elige tu estrategia de limpieza y descarga tu dataset listo para usar.</p>
    </div>
""", unsafe_allow_html=True)

# --- 1. Carga del Archivo ---
uploaded_file = st.file_uploader(" Sube tu archivo CSV para comenzar", type=["csv"])

if uploaded_file is not None:
    # Leemos el CSV original
    df_original = pd.read_csv(uploaded_file)
    df_clean = df_original.copy()
    
    st.divider()
    
    # --- 2. Diagnóstico de Datos Faltantes ---
    st.subheader("🔍 Diagnóstico de Valores Faltantes (NaN)")
    
    # Calculamos la cantidad y el porcentaje de NaNs por columna
    nan_counts = df_original.isna().sum()
    nan_percent = round((df_original.isna().sum() / len(df_original)) * 100, 2)
    
    # Creamos un DataFrame de resumen
    df_resumen = pd.DataFrame({
        'Valores Faltantes': nan_counts,
        '% del Total': nan_percent
    })
    
    # Filtramos para mostrar solo las columnas que tienen al menos 1 NaN
    df_resumen_sucias = df_resumen[df_resumen['Valores Faltantes'] > 0]
    
    col_diag1, col_diag2 = st.columns([1, 2])
    
    with col_diag1:
        if df_resumen_sucias.empty:
            st.success("¡Felicidades! Tu dataset no tiene valores faltantes.")
        else:
            st.write("Columnas que requieren tu atención:")
            # Entre más alto el %, más rojo (Reds) se pone.
            styler = df_resumen_sucias.style.background_gradient(subset=['% del Total'], cmap='Reds', vmin=0, vmax=100)
            st.dataframe(styler, use_container_width=True)
            
    # --- 3. Estrategia de Limpieza ---
    if not df_resumen_sucias.empty:
        st.divider()
        st.subheader(" Panel de Limpieza")
        st.write("Elige cómo quieres tratar los datos faltantes de cada columna:")
        
        opciones_limpieza = ["Mantener igual", "Eliminar filas", "Rellenar con Media (Promedio)", 
                             "Rellenar con Mediana", "Rellenar con Moda", "Rellenar con Cero (0)", "Rellenar con 'Desconocido'"]
        
        # Diccionario para guardar las decisiones del usuario
        estrategias = {}
        
        # Creamos columnas dinámicas para los selectores
        cols = st.columns(3)
        for i, col_name in enumerate(df_resumen_sucias.index):
            with cols[i % 3]: # Distribuimos los selectores en las 3 columnas
                estrategias[col_name] = st.selectbox(f"Columna: **{col_name}**", options=opciones_limpieza, key=f"sel_{col_name}")
        
        # --- 4. Aplicar Limpieza ---
        # Botón para ejecutar las acciones elegidas
        if st.button("Aplicar Limpieza", type="primary"):
            for col, estrategia in estrategias.items():
                if estrategia == "Eliminar filas":
                    df_clean = df_clean.dropna(subset=[col])
                
                elif estrategia == "Rellenar con Media (Promedio)":
                    if pd.api.types.is_numeric_dtype(df_clean[col]):
                        df_clean[col] = df_clean[col].fillna(df_clean[col].mean())
                    else:
                        st.error(f"La columna '{col}' no es numérica. No se puede calcular la media.")
                
                elif estrategia == "Rellenar con Mediana":
                    if pd.api.types.is_numeric_dtype(df_clean[col]):
                        df_clean[col] = df_clean[col].fillna(df_clean[col].median())
                    else:
                        st.error(f"La columna '{col}' no es numérica. No se puede calcular la mediana.")
                
                elif estrategia == "Rellenar con Moda":
                    df_clean[col] = df_clean[col].fillna(df_clean[col].mode()[0])
                
                elif estrategia == "Rellenar con Cero (0)":
                    df_clean[col] = df_clean[col].fillna(0)
                    
                elif estrategia == "Rellenar con 'Desconocido'":
                    df_clean[col] = df_clean[col].fillna("Desconocido")

            st.success("¡Datos limpiados exitosamente!")
            
            # --- 5. Visualización del Antes y Después ---
            st.divider()
            st.subheader("Comparativa: Antes vs Después")
            
            nan_counts_clean = df_clean.isna().sum()
            
            fig = go.Figure()
            # Barras del estado original
            fig.add_trace(go.Bar(
                x=nan_counts.index, y=nan_counts.values,
                name='Antes (Sucios)', marker_color='#ee5253'
            ))
            # Barras del estado limpio
            fig.add_trace(go.Bar(
                x=nan_counts_clean.index, y=nan_counts_clean.values,
                name='Después (Limpios)', marker_color='#10ac84'
            ))
            
            fig.update_layout(
                barmode='group',
                title="Conteo de Valores Faltantes por Columna",
                yaxis_title="Cantidad de NaNs",
                plot_bgcolor='rgba(0,0,0,0)',
                margin=dict(t=40, b=0)
            )
            st.plotly_chart(fig, use_container_width=True)
            
            # --- 6. Descarga del Resultado (st.download_button) ---
            st.subheader("💾 Exportar Resultado")
            st.write("Visualiza una muestra de tu trabajo final y descárgalo.")
            st.dataframe(df_clean.head(), use_container_width=True)
            
            # Convertimos el DataFrame limpio a formato CSV (en texto codificado)
            csv = df_clean.to_csv(index=False).encode('utf-8')
            
            st.download_button(
                label="📥 Descargar CSV Limpio",
                data=csv,
                file_name="dataset_limpio.csv",
                mime="text/csv",
                type="primary"
            )
else:
    st.info("Sube un dataset para comenzar el proceso de limpieza.")