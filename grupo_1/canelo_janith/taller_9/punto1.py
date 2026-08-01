import streamlit as st
import pandas as pd
import plotly.graph_objects as go


# Configuración de la página
st.set_page_config(page_title="Comparador de Datasets", layout="wide")

st.title("📊 Comparador de Distribuciones")
st.markdown("""
Sube dos archivos CSV para comparar la distribución de sus variables numéricas comunes.
""")

# 1. Componente: st.file_uploader con múltiples archivos
uploaded_files = st.file_uploader(
    "Selecciona dos archivos CSV", 
    type="csv", 
    accept_multiple_files=True
)

# Investigando el componente: manejamos el caso de que no sean exactamente 2 archivos
if len(uploaded_files) == 0:
    st.info("Esperando archivos... Por favor, sube dos archivos CSV para comenzar.")
elif len(uploaded_files) == 1:
    st.warning("Has subido solo un archivo. Necesitas subir **dos** para realizar la comparación.")
elif len(uploaded_files) > 2:
    st.error("Has subido demasiados archivos. Por favor, selecciona solo **dos**.")
else:
    # 2. Carga de datos
    df1 = pd.read_csv(uploaded_files[0])
    df2 = pd.read_csv(uploaded_files[1])
    
    name1 = uploaded_files[0].name
    name2 = uploaded_files[1].name

    # 3. Identificar columnas numéricas comunes
    cols_num1 = set(df1.select_dtypes(include=['number']).columns)
    cols_num2 = set(df2.select_dtypes(include=['number']).columns)
    common_cols = sorted(list(cols_num1.intersection(cols_num2)))

    if not common_cols:
        st.error("No se encontraron columnas numéricas comunes entre los dos datasets.")
    else:
        # 4. Selector de variable
        selected_col = st.selectbox("Selecciona la variable a comparar:", common_cols)

        # 5. Visualización con Plotly
        fig = go.Figure()

        # Histograma Dataset 1
        fig.add_trace(go.Histogram(
            x=df1[selected_col],
            name=name1,
            opacity=0.6,
            marker_color='#636EFA'
        ))

        # Histograma Dataset 2
        fig.add_trace(go.Histogram(
            x=df2[selected_col],
            name=name2,
            opacity=0.6,
            marker_color='#EF553B'
        ))

        # Ajustar el modo de superposición
        fig.update_layout(
            barmode='overlay',
            title=f"Distribución de {selected_col}",
            xaxis_title=selected_col,
            yaxis_title="Frecuencia",
            legend_title="Datasets",
            template="plotly_white"
        )

        st.plotly_chart(fig, use_container_width=True)

        # 6. Tabla comparativa de estadísticas
        st.subheader("📋 Estadísticas Descriptivas")
        
        stats = pd.DataFrame({
            "Métrica": ["Media", "Mediana", "Desviación Estándar"],
            name1: [
                df1[selected_col].mean(),
                df1[selected_col].median(),
                df1[selected_col].std()
            ],
            name2: [
                df2[selected_col].mean(),
                df2[selected_col].median(),
                df2[selected_col].std()
            ]
        })

        st.table(stats)