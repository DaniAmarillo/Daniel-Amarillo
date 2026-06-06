# Dashboard ACM TOG — Taller 2 (Minería de Datos, 2016325)

Aplicación **Streamlit** que consulta, visualiza y actualiza la base de datos SQLite construida en el Taller 1 (artículos de *ACM Transactions on Graphics*). Funciona como tablero del proceso **KDD**: adquisición, almacenamiento, consulta y visualización.

Diseño minimalista en blanco y negro, estilo artículo LaTeX; las gráficas interactivas (Plotly) son la única parte con color.

🔗 **App desplegada:** https://acm-tog.streamlit.app

---

## Qué incluye

* **Conexión a SQLite** y consultas dinámicas (panel de consulta SQL de solo lectura).
* **Sidebar** con filtros: rango de fechas, tema/categoría, autor, DOI y palabra clave/título.
* **5 widgets** interactivos (slider de fechas, multiselect, campos de texto, botones, tabla).
* **7 indicadores** calculados sobre los datos filtrados (artículos, autores/art., citas/art., referencias/art., descargas totales, más citado, más descargado).
* **4 gráficas** Plotly: evolución temporal, artículos por categoría, top autores y distribución de citas.
* **Tabla interactiva** con enlaces a los DOI.
* **Actualización por scraping** mediante botón (diseñado para ejecución local).

---

## Arquitectura

| Archivo | Rol |
| :--- | :--- |
| `app.py` | Interfaz Streamlit principal. Dependencias mínimas (`streamlit`, `pandas`, `plotly`). |
| `db.py` | Acceso y normalización de la base SQLite (parseo de fechas, dedup de autores, clasificación temática, inserción normalizada, consultas de solo lectura). |
| `scraper.py` | Actualización por scraping (Selenium + `undetected-chromedriver`), **separada de la app** y con importaciones diferidas. |

### Sobre el Scraper y la Nube

El scraper hereda la lógica del Taller 1 usando `undetected-chromedriver`, el cual necesita un navegador Chrome real y, a veces, intervención humana inicial para resolver captchas. Por la naturaleza de **Streamlit Cloud** (servidores Linux sin interfaz gráfica y bloqueo estricto de IPs de datacenter por parte de Cloudflare), **el scraper solo puede ejecutarse con éxito en un entorno local**. En la nube, la app funciona perfectamente como dashboard de consulta y visualización interactiva sobre los datos ya almacenados.

---

## Ejecutar en local (Recomendado para evaluar scraping)

Para evaluar el 100% de la funcionalidad (incluyendo la actualización de artículos y la evasión de Cloudflare en ACM), la app debe ejecutarse localmente usando tu instalación real de Google Chrome.

```bash
pip install -r requirements.txt -r requirements-scraper.txt
python -m streamlit run app.py