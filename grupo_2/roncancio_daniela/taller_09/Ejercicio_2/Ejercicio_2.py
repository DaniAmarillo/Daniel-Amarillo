import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go

# ── Configuración ──────────────────────────────────────────────────────────────
st.set_page_config(page_title="Series de Tiempo", page_icon="📈", layout="wide")

st.title("📈 Explorador de Series de Tiempo")

# ── Session state ──────────────────────────────────────────────────────────────
if "anotaciones" not in st.session_state:
    st.session_state.anotaciones = []

# ── Cargar archivo ─────────────────────────────────────────────────────────────
archivo = st.file_uploader("Sube un CSV con una columna de fechas", type=["csv"])

def leer_csv(file):
    contenido = file.read().decode("utf-8")
    file.seek(0)
    sep = ";" if ";" in contenido[:500] else ","
    return pd.read_csv(file, sep=sep)

if not archivo:
    st.info("Sube un archivo CSV para comenzar.")
    st.stop()

try:
    df = leer_csv(archivo)
except Exception as e:
    st.error(f"Error al leer el archivo: {e}")
    st.stop()

# ── Seleccionar columnas ───────────────────────────────────────────────────────
col_fecha = st.selectbox("Columna de fecha", df.columns)
col_valor = st.selectbox("Variable numérica", df.select_dtypes("number").columns)

df[col_fecha] = pd.to_datetime(df[col_fecha].astype(str).str.strip(), format="mixed")
df = df[[col_fecha, col_valor]].dropna().sort_values(col_fecha)

fecha_min = df[col_fecha].min().date()
fecha_max = df[col_fecha].max().date()

# ── Rango de fechas ────────────────────────────────────────────────────────────
col1, col2 = st.columns(2)
inicio = col1.date_input("Desde", value=fecha_min, min_value=fecha_min, max_value=fecha_max)
fin    = col2.date_input("Hasta", value=fecha_max, min_value=fecha_min, max_value=fecha_max)

df_f = df[(df[col_fecha].dt.date >= inicio) & (df[col_fecha].dt.date <= fin)]

# ── Gráfico ────────────────────────────────────────────────────────────────────
fig = go.Figure()

fig.add_trace(go.Scatter(
    x=df_f[col_fecha], y=df_f[col_valor],
    mode="lines", name=col_valor,
    line=dict(color="#3fb950", width=2),
    fill="tozeroy", fillcolor="rgba(63,185,80,0.07)"
))

# Líneas verticales por cada anotación
for a in st.session_state.anotaciones:
    if inicio <= a["fecha"] <= fin:
        fig.add_vline(
            x=pd.Timestamp(a["fecha"]).value / 1e6,
            line=dict(color="orange", width=1.5, dash="dash"),
            annotation_text=a["etiqueta"],
            annotation_position="top"
        )

fig.update_layout(height=400, margin=dict(l=10, r=10, t=30, b=10))
st.plotly_chart(fig, use_container_width=True)

# ── Agregar anotación ──────────────────────────────────────────────────────────
st.markdown("---")
st.subheader("➕ Agregar anotación")

c1, c2, c3 = st.columns([2, 3, 1])
nueva_fecha    = c1.date_input("Fecha", value=inicio, min_value=fecha_min, max_value=fecha_max, key="nf")
nueva_etiqueta = c2.text_input("Etiqueta", placeholder="Ej: Pico, Crisis, Evento...")

if c3.button("Guardar", use_container_width=True):
    if nueva_etiqueta.strip():
        st.session_state.anotaciones.append({"fecha": nueva_fecha, "etiqueta": nueva_etiqueta.strip()})
        st.rerun()
    else:
        st.warning("Escribe una etiqueta.")

# ── Lista de anotaciones ───────────────────────────────────────────────────────
with st.expander(f"📌 Anotaciones guardadas ({len(st.session_state.anotaciones)})", expanded=True):
    if not st.session_state.anotaciones:
        st.write("Aún no hay anotaciones.")
    else:
        for i, a in enumerate(st.session_state.anotaciones):
            ca, cb = st.columns([5, 1])
            ca.write(f"**{a['fecha']}** — {a['etiqueta']}")
            if cb.button("🗑️", key=f"del_{i}"):
                st.session_state.anotaciones.pop(i)
                st.rerun()

        if st.button("Eliminar todas"):
            st.session_state.anotaciones = []
            st.rerun()