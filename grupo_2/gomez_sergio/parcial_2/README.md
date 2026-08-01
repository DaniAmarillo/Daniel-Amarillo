# 📊 Análisis Bibliométrico — Journal of Hematology & Oncology

> Dashboard interactivo para el análisis bibliométrico de artículos publicados en el **Journal of Hematology & Oncology (JHO)**, uno de los journals Q1 más relevantes en oncología a nivel mundial.

🔗 **[Ver app en producción](https://sgernag.shinyapps.io/parcial2-gomez-sergio/)**

---

## Descripción

Esta aplicación Shiny permite explorar, filtrar y analizar la producción científica indexada del JHO. Los datos son recolectados automáticamente desde [Springer Link](https://link.springer.com/journal/13045) mediante un pipeline de scraping construido en Python, y almacenados en una base de datos SQLite que la app consulta en tiempo real.

El objetivo es proporcionar una vista integral de métricas bibliométricas: tendencias temporales de publicación, distribución temática, autores más prolíficos, métricas de impacto (citas y descargas) y análisis de redes de referencias.

---

## Funcionalidades

- **Filtros dinámicos** por rango de años, tema, journal y número mínimo de citas
- **Tendencia temporal** de publicaciones con escala ajustable (año / mes-año / día-mes-año)
- **Clasificación temática** automática de abstracts en categorías: Machine Learning, IA Generativa, Estadística y Otros
- **Top autores** por volumen de publicaciones
- **Métricas de impacto**: distribución de citas y descargas por artículo
- **Scraping bajo demanda** directamente desde la UI, con logs en tiempo real
- **Exportación** de datos filtrados

## Autor

**Sergio Gómez** — [@sgernag](https://www.shinyapps.io/admin/#/dashboard)  
Curso: Minería de Datos · 2025
