# ============================================================
# Minería de Datos - Taller Clase 10
# Medidas estadísticas y comunicación de resultados
# Versión unificada en Streamlit
# ============================================================

import io

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import scipy.stats as stats
import streamlit as st


# ------------------------------------------------------------
# Configuración general
# ------------------------------------------------------------

st.set_page_config(
    page_title="Taller 10 - Streamlit",
    layout="wide"
)

st.title("Minería de Datos - Taller Clase 10")
st.caption("Versión unificada de los cinco ejercicios en una sola aplicación Streamlit.")


# ------------------------------------------------------------
# Funciones auxiliares
# ------------------------------------------------------------

def leer_csv(archivo):
    """Lee un archivo CSV cargado desde Streamlit."""
    return pd.read_csv(archivo)


def columnas_numericas(df):
    """Devuelve nombres de columnas numéricas."""
    return df.select_dtypes(include="number").columns.tolist()


def columnas_no_numericas(df):
    """Devuelve nombres de columnas no numéricas."""
    return df.select_dtypes(exclude="number").columns.tolist()


def convertir_csv_descarga(df):
    """Convierte un DataFrame a bytes CSV para descargar."""
    return df.to_csv(index=False).encode("utf-8")


def tabla_resumen_numerica(serie):
    """Calcula media, mediana y desviación estándar de una serie numérica."""
    return {
        "media": serie.mean(),
        "mediana": serie.median(),
        "desviacion_estandar": serie.std()
    }


def calcular_n_proporcion(p, e, confianza):
    """Calcula tamaño de muestra para estimar una proporción."""
    z = stats.norm.ppf(1 - (1 - confianza) / 2)
    n = np.ceil((z ** 2) * p * (1 - p) / (e ** 2))
    return int(n), z


# ------------------------------------------------------------
# Menú de ejercicios
# ------------------------------------------------------------

ejercicio = st.sidebar.selectbox(
    "Seleccione el ejercicio",
    [
        "Ejercicio 1. Comparador de distribuciones",
        "Ejercicio 2. Series de tiempo con anotaciones",
        "Ejercicio 3. Tamaño muestral",
        "Ejercicio 4. Pipeline de limpieza",
        "Ejercicio 5. Dashboard con roles"
    ]
)


# ============================================================
# Ejercicio 1. Comparador de distribuciones con múltiples datasets
# ============================================================

if ejercicio == "Ejercicio 1. Comparador de distribuciones":

    st.header("Ejercicio 1. Comparador de distribuciones con múltiples datasets")

    st.write(
        "Cargue dos archivos CSV, seleccione una columna numérica común "
        "y compare sus distribuciones mediante histogramas superpuestos."
    )

    archivos = st.file_uploader(
        "Cargue dos archivos CSV",
        type="csv",
        accept_multiple_files=True
    )

    if len(archivos) < 2:
        st.info("Cargue dos archivos CSV para realizar la comparación.")
        st.stop()

    if len(archivos) > 2:
        st.warning("Se usarán únicamente los dos primeros archivos cargados.")

    archivo_1, archivo_2 = archivos[0], archivos[1]
    datos_1 = leer_csv(archivo_1)
    datos_2 = leer_csv(archivo_2)

    numericas_1 = columnas_numericas(datos_1)
    numericas_2 = columnas_numericas(datos_2)

    columnas_comunes = sorted(set(numericas_1).intersection(set(numericas_2)))

    if len(columnas_comunes) == 0:
        st.error("Los dos archivos no tienen columnas numéricas comunes.")
        st.stop()

    columna = st.selectbox(
        "Seleccione una columna numérica común",
        columnas_comunes
    )

    serie_1 = datos_1[columna].dropna()
    serie_2 = datos_2[columna].dropna()

    fig = go.Figure()

    fig.add_trace(
        go.Histogram(
            x=serie_1,
            name=archivo_1.name,
            opacity=0.6
        )
    )

    fig.add_trace(
        go.Histogram(
            x=serie_2,
            name=archivo_2.name,
            opacity=0.6
        )
    )

    fig.update_layout(
        title=f"Distribución comparada de {columna}",
        xaxis_title=columna,
        yaxis_title="Frecuencia",
        barmode="overlay"
    )

    st.plotly_chart(fig, use_container_width=True)

    resumen = pd.DataFrame({
        "dataset": [archivo_1.name, archivo_2.name],
        "media": [serie_1.mean(), serie_2.mean()],
        "mediana": [serie_1.median(), serie_2.median()],
        "desviacion_estandar": [serie_1.std(), serie_2.std()]
    })

    st.subheader("Tabla comparativa")
    st.dataframe(resumen, use_container_width=True)


# ============================================================
# Ejercicio 2. Explorador de series de tiempo con anotaciones
# ============================================================

elif ejercicio == "Ejercicio 2. Series de tiempo con anotaciones":

    st.header("Ejercicio 2. Explorador de series de tiempo con anotaciones")

    if "anotaciones" not in st.session_state:
        st.session_state.anotaciones = []

    archivo = st.file_uploader(
        "Cargue un archivo CSV con una columna de fechas",
        type="csv"
    )

    if archivo is None:
        st.info("Cargue un CSV para iniciar el análisis.")
        st.stop()

    datos = leer_csv(archivo)

    col_fecha = st.selectbox(
        "Columna de fecha",
        datos.columns.tolist()
    )

    datos[col_fecha] = pd.to_datetime(datos[col_fecha], errors="coerce")
    datos = datos.dropna(subset=[col_fecha])

    if datos.empty:
        st.error("No fue posible interpretar fechas válidas en la columna seleccionada.")
        st.stop()

    numericas = columnas_numericas(datos)

    if len(numericas) == 0:
        st.error("El archivo no contiene columnas numéricas para graficar.")
        st.stop()

    col_valor = st.selectbox(
        "Columna numérica",
        numericas
    )

    datos = datos.sort_values(col_fecha)

    fecha_min = datos[col_fecha].min().date()
    fecha_max = datos[col_fecha].max().date()

    rango = st.date_input(
        "Rango de fechas",
        value=(fecha_min, fecha_max),
        min_value=fecha_min,
        max_value=fecha_max
    )

    if len(rango) != 2:
        st.info("Seleccione fecha inicial y fecha final.")
        st.stop()

    inicio, fin = rango

    datos_filtrados = datos[
        (datos[col_fecha].dt.date >= inicio) &
        (datos[col_fecha].dt.date <= fin)
    ]

    st.subheader("Agregar anotación")

    col1, col2 = st.columns(2)

    with col1:
        fecha_anotacion = st.date_input(
            "Fecha de anotación",
            value=inicio,
            min_value=fecha_min,
            max_value=fecha_max
        )

    with col2:
        texto_anotacion = st.text_input("Etiqueta")

    if st.button("Agregar anotación"):
        if texto_anotacion.strip() != "":
            st.session_state.anotaciones.append({
                "fecha": fecha_anotacion,
                "texto": texto_anotacion.strip()
            })
            st.rerun()
        else:
            st.warning("Escriba una etiqueta antes de agregar la anotación.")

    fig = go.Figure()

    fig.add_trace(
        go.Scatter(
            x=datos_filtrados[col_fecha],
            y=datos_filtrados[col_valor],
            mode="lines+markers",
            name=col_valor
        )
    )

    y_ref = datos_filtrados[col_valor].max() if not datos_filtrados.empty else 0

    for anotacion in st.session_state.anotaciones:
        fig.add_vline(
            x=anotacion["fecha"],
            line_dash="dash"
        )
        fig.add_annotation(
            x=anotacion["fecha"],
            y=y_ref,
            text=anotacion["texto"],
            showarrow=True
        )

    fig.update_layout(
        title=f"Serie de tiempo: {col_valor}",
        xaxis_title="Fecha",
        yaxis_title=col_valor
    )

    st.plotly_chart(fig, use_container_width=True)

    with st.expander("Anotaciones guardadas"):
        if len(st.session_state.anotaciones) == 0:
            st.write("No hay anotaciones guardadas.")
        else:
            st.dataframe(
                pd.DataFrame(st.session_state.anotaciones),
                use_container_width=True
            )

            eliminar = st.selectbox(
                "Seleccione una anotación para eliminar",
                range(len(st.session_state.anotaciones)),
                format_func=lambda i: (
                    f"{st.session_state.anotaciones[i]['fecha']} - "
                    f"{st.session_state.anotaciones[i]['texto']}"
                )
            )

            if st.button("Eliminar anotación"):
                st.session_state.anotaciones.pop(eliminar)
                st.rerun()


# ============================================================
# Ejercicio 3. Calculadora de tamaño muestral interactiva
# ============================================================

elif ejercicio == "Ejercicio 3. Tamaño muestral":

    st.header("Ejercicio 3. Calculadora interactiva de tamaño muestral")

    st.sidebar.header("Parámetros")

    p = st.sidebar.slider(
        "Proporción esperada",
        min_value=0.01,
        max_value=0.99,
        value=0.50,
        step=0.01
    )

    e = st.sidebar.slider(
        "Margen de error",
        min_value=0.01,
        max_value=0.20,
        value=0.05,
        step=0.01
    )

    confianza = st.sidebar.selectbox(
        "Nivel de confianza",
        [0.90, 0.95, 0.99],
        index=1
    )

    n_muestra, z = calcular_n_proporcion(p, e, confianza)

    st.metric(
        label="Tamaño de muestra requerido",
        value=n_muestra
    )

    tab1, tab2, tab3 = st.tabs([
        "Sensibilidad al margen de error",
        "Sensibilidad a la proporción",
        "Fórmula"
    ])

    with tab1:
        errores = np.arange(0.01, 0.201, 0.01)

        curva_error = pd.DataFrame({
            "margen_error": errores,
            "n": [calcular_n_proporcion(p, err, confianza)[0] for err in errores]
        })

        fig1 = px.line(
            curva_error,
            x="margen_error",
            y="n",
            markers=True,
            title="Tamaño muestral vs. margen de error"
        )

        st.plotly_chart(fig1, use_container_width=True)

    with tab2:
        proporciones = np.arange(0.01, 0.991, 0.01)

        curva_p = pd.DataFrame({
            "proporcion_esperada": proporciones,
            "n": [calcular_n_proporcion(prop, e, confianza)[0] for prop in proporciones]
        })

        fig2 = px.line(
            curva_p,
            x="proporcion_esperada",
            y="n",
            markers=True,
            title="Tamaño muestral vs. proporción esperada"
        )

        st.plotly_chart(fig2, use_container_width=True)

    with tab3:
        st.latex(r"n = \frac{z_{1-\alpha/2}^2 p(1-p)}{e^2}")
        st.write(
            "Donde p es la proporción esperada, e es el margen de error "
            "y z es el cuantil normal asociado al nivel de confianza."
        )


# ============================================================
# Ejercicio 4. Pipeline de limpieza de datos con descarga
# ============================================================

elif ejercicio == "Ejercicio 4. Pipeline de limpieza":

    st.header("Ejercicio 4. Pipeline de limpieza de datos con descarga")

    archivo = st.file_uploader(
        "Cargue un archivo CSV con valores faltantes",
        type="csv"
    )

    if archivo is None:
        st.info("Cargue un CSV para iniciar la limpieza.")
        st.stop()

    datos = leer_csv(archivo)

    st.subheader("Datos originales")
    st.dataframe(datos.head(20), use_container_width=True)

    na_antes = datos.isna().mean().mul(100).reset_index()
    na_antes.columns = ["columna", "pct_na_antes"]

    st.subheader("Porcentaje de NaN por columna")
    st.dataframe(
        na_antes.style.background_gradient(subset=["pct_na_antes"]),
        use_container_width=True
    )

    datos_limpios = datos.copy()
    columnas_con_na = datos.columns[datos.isna().any()].tolist()

    decisiones = {}

    st.subheader("Tratamiento por columna")

    if len(columnas_con_na) == 0:
        st.success("El dataset no contiene valores faltantes.")

    for columna in columnas_con_na:
        decisiones[columna] = st.selectbox(
            f"Tratamiento para {columna}",
            [
                "eliminar filas",
                "rellenar con media",
                "rellenar con mediana",
                "rellenar con moda",
                "rellenar con valor constante"
            ],
            key=f"decision_{columna}"
        )

        if decisiones[columna] == "rellenar con valor constante":
            decisiones[f"valor_{columna}"] = st.text_input(
                f"Valor constante para {columna}",
                key=f"valor_{columna}"
            )

    for columna in columnas_con_na:
        decision = decisiones[columna]

        if decision == "eliminar filas":
            datos_limpios = datos_limpios.dropna(subset=[columna])

        elif decision == "rellenar con media":
            if pd.api.types.is_numeric_dtype(datos_limpios[columna]):
                datos_limpios[columna] = datos_limpios[columna].fillna(
                    datos_limpios[columna].mean()
                )
            else:
                st.warning(f"{columna} no es numérica; no se aplicó media.")

        elif decision == "rellenar con mediana":
            if pd.api.types.is_numeric_dtype(datos_limpios[columna]):
                datos_limpios[columna] = datos_limpios[columna].fillna(
                    datos_limpios[columna].median()
                )
            else:
                st.warning(f"{columna} no es numérica; no se aplicó mediana.")

        elif decision == "rellenar con moda":
            moda = datos_limpios[columna].mode(dropna=True)
            if len(moda) > 0:
                datos_limpios[columna] = datos_limpios[columna].fillna(moda.iloc[0])

        elif decision == "rellenar con valor constante":
            valor_constante = decisiones.get(f"valor_{columna}", "")
            datos_limpios[columna] = datos_limpios[columna].fillna(valor_constante)

    st.subheader("Resultado limpio")
    st.dataframe(datos_limpios, use_container_width=True)

    na_despues = datos_limpios.isna().mean().mul(100).reset_index()
    na_despues.columns = ["columna", "pct_na_despues"]

    comparacion = na_antes.merge(
        na_despues,
        on="columna",
        how="left"
    ).fillna(0)

    comparacion_larga = comparacion.melt(
        id_vars="columna",
        value_vars=["pct_na_antes", "pct_na_despues"],
        var_name="momento",
        value_name="pct_na"
    )

    fig = px.bar(
        comparacion_larga,
        x="columna",
        y="pct_na",
        color="momento",
        barmode="group",
        title="Porcentaje de NaN antes y después"
    )

    st.plotly_chart(fig, use_container_width=True)

    csv = convertir_csv_descarga(datos_limpios)

    st.download_button(
        label="Descargar CSV limpio",
        data=csv,
        file_name="datos_limpios.csv",
        mime="text/csv"
    )


# ============================================================
# Ejercicio 5. Dashboard con autenticación básica y roles
# ============================================================

elif ejercicio == "Ejercicio 5. Dashboard con roles":

    st.header("Ejercicio 5. Dashboard con autenticación básica y roles")

    if "rol" not in st.session_state:
        st.session_state.rol = "visitante"

    if "datos_app" not in st.session_state:
        st.session_state.datos_app = None

    password_ingresado = st.text_input(
        "Password",
        type="password"
    )

    try:
        password_correcto = st.secrets["password"]
    except Exception:
        password_correcto = None

    if password_correcto is None:
        st.warning(
            "Configure el password en .streamlit/secrets.toml "
            "con la clave password."
        )
    elif password_ingresado == password_correcto:
        st.session_state.rol = "analista"

    archivo = st.file_uploader(
        "Cargue un archivo CSV",
        type="csv"
    )

    if archivo is None and st.session_state.datos_app is None:
        st.info("Cargue un CSV para visualizar el dashboard.")
        st.stop()

    if archivo is not None:
        st.session_state.datos_app = leer_csv(archivo)

    datos = st.session_state.datos_app.copy()

    numericas = columnas_numericas(datos)

    st.subheader(f"Rol activo: {st.session_state.rol}")

    if len(numericas) == 0:
        st.error("El dataset no contiene columnas numéricas para construir gráficos.")
        st.stop()

    variable_num = st.selectbox(
        "Variable numérica",
        numericas
    )

    # Gráfico 1: distribución
    fig_dist = px.histogram(
        datos,
        x=variable_num,
        title="Distribución"
    )

    st.plotly_chart(fig_dist, use_container_width=True)

    # Gráfico 2: correlación
    if len(numericas) >= 2:
        corr = datos[numericas].corr(numeric_only=True)

        fig_corr = px.imshow(
            corr,
            text_auto=True,
            title="Correlación"
        )

        st.plotly_chart(fig_corr, use_container_width=True)

    # Gráfico 3: tendencia temporal
    col_fecha = st.selectbox(
        "Columna para tendencia temporal",
        datos.columns.tolist()
    )

    datos[col_fecha] = pd.to_datetime(datos[col_fecha], errors="coerce")
    datos_tiempo = datos.dropna(subset=[col_fecha]).sort_values(col_fecha)

    if not datos_tiempo.empty:
        fig_tendencia = px.line(
            datos_tiempo,
            x=col_fecha,
            y=variable_num,
            title="Tendencia temporal"
        )

        st.plotly_chart(fig_tendencia, use_container_width=True)
    else:
        st.warning("La columna seleccionada no contiene fechas válidas.")

    if st.session_state.rol == "analista":

        st.subheader("Tabla completa")
        st.dataframe(datos, use_container_width=True)

        csv = convertir_csv_descarga(datos)

        st.download_button(
            label="Descargar datos",
            data=csv,
            file_name="datos_dashboard.csv",
            mime="text/csv"
        )

        st.subheader("Agregar fila en memoria")

        with st.form("form_agregar_fila"):
            nueva_fila = {}

            for columna in datos.columns:
                nueva_fila[columna] = st.text_input(columna)

            enviar = st.form_submit_button("Agregar fila")

        if enviar:
            nueva_fila_df = pd.DataFrame([nueva_fila])
            st.session_state.datos_app = pd.concat(
                [st.session_state.datos_app, nueva_fila_df],
                ignore_index=True
            )
            st.rerun()

    else:
        st.info("El rol visitante solo puede ver gráficos de resumen.")

