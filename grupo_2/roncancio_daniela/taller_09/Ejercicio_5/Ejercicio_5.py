import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.express as px
from io import StringIO

# ── Configuración ──────────────────────────────────────────────────────────────
st.set_page_config(page_title="Dashboard con Roles", page_icon="🔐", layout="wide")

# ── Session state ──────────────────────────────────────────────────────────────
if "rol" not in st.session_state:
    st.session_state.rol = "visitante"

# ── Cargar archivo ─────────────────────────────────────────────────────────────
archivo = st.file_uploader("Sube un CSV", type=["csv"])

def leer_csv(file):
    contenido = file.read().decode("utf-8")
    file.seek(0)
    sep = ";" if ";" in contenido[:500] else ","
    return pd.read_csv(file, sep=sep)

if not archivo:
    st.info("Sube un archivo CSV para comenzar.")
    st.stop()

if "df" not in st.session_state or st.session_state.get("archivo") != archivo.name:
    st.session_state.df = leer_csv(archivo)
    st.session_state.archivo = archivo.name

df = st.session_state.df

# ── Login ──────────────────────────────────────────────────────────────────────
st.title("🔐 Dashboard con Autenticación")

if st.session_state.rol == "visitante":
    st.info("Estás como **visitante**. Ingresa la contraseña para acceder como analista.")
    with st.form("login_form"):
        password = st.text_input("Contraseña", type="password")
        submitted = st.form_submit_button("Ingresar")
        if submitted:
            try:
                clave_correcta = st.secrets["password"]
            except Exception:
                clave_correcta = "analista123"  # clave por defecto si no hay secrets.toml
            if password == clave_correcta:
                st.session_state.rol = "analista"
                st.rerun()
            else:
                st.error("Contraseña incorrecta.")
else:
    col1, col2 = st.columns([8, 2])
    col1.success(f"Sesión activa: **analista**")
    if col2.button("Cerrar sesión"):
        st.session_state.rol = "visitante"
        st.rerun()

st.markdown("---")

# ── Gráficos (ambos roles) ─────────────────────────────────────────────────────
st.subheader("📊 Resumen del dataset")

num_cols  = df.select_dtypes("number").columns.tolist()
date_cols = [c for c in df.columns if pd.api.types.is_datetime64_any_dtype(df[c])]

# Intentar parsear fechas si no hay columna datetime
if not date_cols:
    for c in df.columns:
        try:
            df[c] = pd.to_datetime(df[c].astype(str).str.strip(), format="mixed")
            date_cols.append(c)
            break
        except Exception:
            pass

c1, c2, c3 = st.columns(3)

# 1. Distribución de la primera columna numérica
with c1:
    if num_cols:
        col_hist = num_cols[0]
        st.markdown(f"**Distribución de {col_hist}**")
        fig1 = go.Figure(go.Histogram(
            x=df[col_hist], nbinsx=30,
            marker_color="#58a6ff", opacity=0.8
        ))
        fig1.update_layout(height=280, margin=dict(l=5, r=5, t=10, b=5), showlegend=False)
        st.plotly_chart(fig1, use_container_width=True)

# 2. Correlación entre las dos primeras columnas numéricas
with c2:
    if len(num_cols) >= 2:
        cx, cy = num_cols[0], num_cols[1]
        st.markdown(f"**{cx} vs {cy}**")
        fig2 = go.Figure(go.Scatter(
            x=df[cx], y=df[cy], mode="markers",
            marker=dict(color="#3fb950", opacity=0.6, size=5)
        ))
        fig2.update_layout(
            height=280, margin=dict(l=5, r=5, t=10, b=5),
            xaxis_title=cx, yaxis_title=cy
        )
        st.plotly_chart(fig2, use_container_width=True)

# 3. Tendencia temporal
with c3:
    if date_cols and num_cols:
        col_d, col_v = date_cols[0], num_cols[0]
        st.markdown(f"**Tendencia de {col_v}**")
        df_t = df[[col_d, col_v]].dropna().sort_values(col_d)
        fig3 = go.Figure(go.Scatter(
            x=df_t[col_d], y=df_t[col_v],
            mode="lines", line=dict(color="#f0883e", width=2)
        ))
        fig3.update_layout(height=280, margin=dict(l=5, r=5, t=10, b=5))
        st.plotly_chart(fig3, use_container_width=True)
    else:
        st.info("No se detectó columna de fechas para la tendencia.")

# ── Sección analista ───────────────────────────────────────────────────────────
if st.session_state.rol == "analista":
    st.markdown("---")
    st.subheader("🔍 Acceso analista")

    # Tabla completa
    st.markdown("**Tabla completa**")
    st.dataframe(df, use_container_width=True)

    # Botón de descarga
    csv = df.to_csv(index=False).encode("utf-8")
    st.download_button(
        label="⬇️ Descargar CSV",
        data=csv,
        file_name="datos.csv",
        mime="text/csv"
    )

    # Formulario para agregar fila
    st.markdown("**Agregar nueva fila**")
    with st.form("nueva_fila"):
        nuevos_valores = {}
        cols_form = st.columns(len(df.columns))
        for i, col in enumerate(df.columns):
            if pd.api.types.is_numeric_dtype(df[col]):
                nuevos_valores[col] = cols_form[i].number_input(col, value=float(df[col].mean()))
            elif pd.api.types.is_datetime64_any_dtype(df[col]):
                nuevos_valores[col] = pd.Timestamp(cols_form[i].date_input(col))
            else:
                nuevos_valores[col] = cols_form[i].text_input(col)

        if st.form_submit_button("Agregar fila"):
            nueva = pd.DataFrame([nuevos_valores])
            st.session_state.df = pd.concat([st.session_state.df, nueva], ignore_index=True)
            st.success("Fila agregada.")
            st.rerun()