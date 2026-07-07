import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go

# ── Configuración ──────────────────────────────────────────────────────────────
st.set_page_config(page_title="Limpieza de Datos", page_icon="🧹", layout="wide")

st.title("🧹 Pipeline de Limpieza de Datos")
st.caption("Carga un CSV, trata los valores faltantes y descarga el resultado limpio.")

# ── Cargar archivo ─────────────────────────────────────────────────────────────
archivo = st.file_uploader("Sube un CSV con valores faltantes", type=["csv"])

def leer_csv(file):
    contenido = file.read().decode("utf-8")
    file.seek(0)
    sep = ";" if ";" in contenido[:500] else ","
    return pd.read_csv(file, sep=sep)

if not archivo:
    st.info("Sube un archivo CSV para comenzar.")
    st.stop()

df = leer_csv(archivo)

# ── Resumen de NaN ─────────────────────────────────────────────────────────────
st.markdown("---")
st.subheader("1. Resumen de valores faltantes")

nan_counts = df.isnull().sum()
nan_pct    = (nan_counts / len(df) * 100).round(2)
resumen    = pd.DataFrame({"Faltantes": nan_counts, "Porcentaje (%)": nan_pct})
resumen    = resumen[resumen["Faltantes"] > 0]

if resumen.empty:
    st.success("¡El dataset no tiene valores faltantes!")
    st.stop()

# Color condicional con pandas Styler
def colorear(val):
    if val == 0:
        return "background-color: #d4edda; color: #155724"
    elif val < 10:
        return "background-color: #fff3cd; color: #856404"
    else:
        return "background-color: #f8d7da; color: #721c24"

styled = resumen.style.map(colorear, subset=["Porcentaje (%)"])
st.dataframe(styled, use_container_width=True)

# ── Elegir tratamiento por columna ─────────────────────────────────────────────
st.markdown("---")
st.subheader("2. Elegir tratamiento por columna")

cols_nan = resumen.index.tolist()
opciones_num = ["Eliminar filas", "Rellenar con media", "Rellenar con mediana", "Rellenar con moda", "Valor constante"]
opciones_cat = ["Eliminar filas", "Rellenar con moda", "Valor constante"]

decisiones = {}
constantes = {}

for col in cols_nan:
    es_num = pd.api.types.is_numeric_dtype(df[col])
    opciones = opciones_num if es_num else opciones_cat
    c1, c2 = st.columns([2, 3])
    c1.markdown(f"**{col}** — {resumen.loc[col, 'Faltantes']} NaN ({resumen.loc[col, 'Porcentaje (%)']}%)")
    decision = c2.selectbox("Tratamiento", opciones, key=f"sel_{col}")
    decisiones[col] = decision

    if decision == "Valor constante":
        constantes[col] = st.text_input(f"Valor constante para '{col}'", key=f"const_{col}")

# ── Aplicar limpieza ───────────────────────────────────────────────────────────
st.markdown("---")
st.subheader("3. Resultado limpio")

df_limpio = df.copy()

for col, decision in decisiones.items():
    if decision == "Eliminar filas":
        df_limpio = df_limpio.dropna(subset=[col])
    elif decision == "Rellenar con media":
        df_limpio[col] = df_limpio[col].fillna(df_limpio[col].mean())
    elif decision == "Rellenar con mediana":
        df_limpio[col] = df_limpio[col].fillna(df_limpio[col].median())
    elif decision == "Rellenar con moda":
        df_limpio[col] = df_limpio[col].fillna(df_limpio[col].mode()[0])
    elif decision == "Valor constante":
        val = constantes.get(col, "")
        if val != "":
            try:
                val = float(val) if pd.api.types.is_numeric_dtype(df[col]) else val
            except ValueError:
                pass
            df_limpio[col] = df_limpio[col].fillna(val)

st.dataframe(df_limpio, use_container_width=True)
st.caption(f"Filas originales: {len(df)} → Filas limpias: {len(df_limpio)}")

# ── Botón de descarga ──────────────────────────────────────────────────────────
csv_limpio = df_limpio.to_csv(index=False).encode("utf-8")
st.download_button(
    label="⬇️ Descargar CSV limpio",
    data=csv_limpio,
    file_name="dataset_limpio.csv",
    mime="text/csv",
    use_container_width=True
)

# ── Gráfico comparativo ────────────────────────────────────────────────────────
st.markdown("---")
st.subheader("4. NaN antes vs después")

nan_antes   = (df.isnull().sum() / len(df) * 100).round(2)
nan_despues = (df_limpio.isnull().sum() / len(df_limpio) * 100).round(2) if len(df_limpio) > 0 else nan_antes * 0
cols_plot   = nan_antes[nan_antes > 0].index.tolist()

fig = go.Figure()
fig.add_trace(go.Bar(name="Antes", x=cols_plot, y=nan_antes[cols_plot], marker_color="#f78166"))
fig.add_trace(go.Bar(name="Después", x=cols_plot, y=nan_despues[cols_plot], marker_color="#3fb950"))

fig.update_layout(
    barmode="group",
    yaxis_title="% NaN",
    height=380,
    margin=dict(l=10, r=10, t=30, b=10),
    legend=dict(orientation="h", y=1.1)
)
st.plotly_chart(fig, use_container_width=True)