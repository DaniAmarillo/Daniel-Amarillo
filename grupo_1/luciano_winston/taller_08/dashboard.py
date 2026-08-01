import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px

# --- Configuración de la página ---
st.set_page_config(page_title="Dashboard", layout="wide")

st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@500;600;700&display=swap');
    html, body, [class*="css"] { font-family: 'Inter', sans-serif; }
    .cover-page {
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        color: white; padding: 25px 30px; border-radius: 12px; margin-bottom: 25px;
    }
    .cover-page h1 { font-family: 'Poppins', sans-serif; color: white; font-weight: 700; margin: 0 0 10px 0; }
    </style>
""", unsafe_allow_html=True)

# --- 1. Inicializar Variables en la Memoria (Session State) ---
if 'rol' not in st.session_state:
    st.session_state.rol = 'visitante'

if 'df_activo' not in st.session_state:
    st.session_state.df_activo = None

# ¡Nueva variable! Memoriza el nombre del archivo para detectar cambios
if 'nombre_archivo_actual' not in st.session_state:
    st.session_state.nombre_archivo_actual = None


# --- 2. Sistema de Autenticación (Sidebar) ---
with st.sidebar:
    st.header("Autenticación")
    
    if st.session_state.rol == 'visitante':
        st.info("Rol actual: **Visitante** (Solo visualización)")
        pwd_ingresada = st.text_input("Contraseña de Analista:", type="password")
        btn_login = st.button("Ingresar", use_container_width=True)
        
        if btn_login:
            if pwd_ingresada == st.secrets["password_analista"]:
                st.session_state.rol = 'analista'
                st.success("¡Acceso concedido!")
                st.rerun()
            else:
                st.error("Contraseña incorrecta.")
    
    elif st.session_state.rol == 'analista':
        st.success("Rol actual: **Analista** (Control Total)")
        btn_logout = st.button("Cerrar Sesión", use_container_width=True)
        
        if btn_logout:
            st.session_state.rol = 'visitante'
            st.session_state.df_activo = None
            st.session_state.nombre_archivo_actual = None # Limpiamos al salir
            st.rerun()

# --- Encabezado Principal ---
st.markdown(f"""
    <div class="cover-page">
        <h1>Dashboard Dinámico</h1>
        <p>Vista actual: <b>{st.session_state.rol.capitalize()}</b></p>
    </div>
""", unsafe_allow_html=True)


# --- 3. Cargador de Archivos ---
uploaded_file = st.file_uploader("Sube un archivo CSV para activar el Dashboard", type=["csv"])

if uploaded_file is not None:
    # LÓGICA DE DETECCIÓN DE CAMBIO:
    # Si el archivo que está en el uploader se llama diferente al que teníamos en memoria...
    if uploaded_file.name != st.session_state.nombre_archivo_actual:
        # Forzamos la lectura del nuevo archivo y actualizamos la memoria
        df_subido = pd.read_csv(uploaded_file)
        
        # Estandarizar fechas si existen
        for col in df_subido.columns:
            if 'fecha' in col.lower() or 'date' in col.lower():
                df_subido[col] = pd.to_datetime(df_subido[col])
                
        # Guardamos el nuevo dataframe y registramos el nuevo nombre de archivo
        st.session_state.df_activo = df_subido
        st.session_state.nombre_archivo_actual = uploaded_file.name
        st.rerun() # Recargamos para que todo el dashboard dibuje los nuevos datos de inmediato


# --- 4. Renderizado Condicional ---
if st.session_state.df_activo is not None:
    df = st.session_state.df_activo
    
    columnas_numericas = df.select_dtypes(include=['number']).columns.tolist()
    columnas_fecha = df.select_dtypes(include=['datetime64[ns]', 'object']).columns.tolist()
    
    st.divider()
    st.subheader(" Visualización de Gráficos")
    
    if len(columnas_numericas) >= 2:
        col1, col2, col3 = st.columns(3)
        
        var_x_num = columnas_numericas[0]
        var_y_num = columnas_numericas[1] if len(columnas_numericas) > 1 else columnas_numericas[0]
        var_temporal = columnas_fecha[0] if columnas_fecha else df.index
        
        with col1:
            fig_dist = px.histogram(df, x=var_x_num, title=f"Distribución de {var_x_num}", color_discrete_sequence=['#3498db'])
            st.plotly_chart(fig_dist, use_container_width=True)
            
        with col2:
            fig_corr = px.scatter(df, x=var_x_num, y=var_y_num, title=f"Correlación: {var_x_num} vs {var_y_num}", color_discrete_sequence=['#e74c3c'])
            st.plotly_chart(fig_corr, use_container_width=True)
            
        with col3:
            fig_trend = px.line(df, x=var_temporal, y=var_y_num, title=f"Tendencia de {var_y_num} en el Tiempo", color_discrete_sequence=['#2ecc71'])
            st.plotly_chart(fig_trend, use_container_width=True)
    else:
        st.warning("El archivo cargado necesita tener al menos 2 columnas numéricas para generar el dashboard automático.")

    st.divider()

    # --- 5. Bloque Exclusivo: Panel del Analista ---
    if st.session_state.rol == 'analista':
        st.subheader("Caja de Herramientas del Analista")
        
        col_tabla, col_form = st.columns([2, 1])
        
        with col_tabla:
            st.write("**Explorador de Datos Cargados:**")
            st.dataframe(df, use_container_width=True)
            
            csv_data = df.to_csv(index=False).encode('utf-8')
            st.download_button(
                label="Descargar Dataset Modificado",
                data=csv_data,
                file_name="dataset_analista_pro.csv",
                mime="text/csv",
                type="primary"
            )
            
        with col_form:
            st.write("**Formulario: Inyectar Fila Manual**")
            
            with st.form("form_dinamico", clear_on_submit=True):
                nuevos_valores = {}
                
                for col_name in df.columns:
                    if pd.api.types.is_numeric_dtype(df[col_name]):
                        nuevos_valores[col_name] = st.number_input(f"{col_name}:", value=0)
                    elif pd.api.types.is_datetime64_any_dtype(df[col_name]):
                        nuevos_valores[col_name] = pd.to_datetime(st.date_input(f"{col_name}:"))
                    else:
                        nuevos_valores[col_name] = st.text_input(f"{col_name}:", value="Dato Manual")
                        
                submit_fila = st.form_submit_button("Insertar Registro", use_container_width=True)
                
                if submit_fila:
                    nueva_fila_df = pd.DataFrame([nuevos_valores])
                    st.session_state.df_activo = pd.concat([st.session_state.df_activo, nueva_fila_df], ignore_index=True)
                    st.success("¡Registro agregado a la memoria! Actualizando gráficos...")
                    st.rerun()
else:
    # Mensaje si no hay ningún archivo cargado (limpiamos el estado al iniciar)
    st.session_state.nombre_archivo_actual = None
    st.info("Por favor, sube un archivo para visualizar el comportamiento de los datos.")