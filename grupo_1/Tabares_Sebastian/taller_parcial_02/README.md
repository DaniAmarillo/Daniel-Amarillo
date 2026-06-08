# Taller 2 — Minería de Datos (2016325)
**Dashboard KDD: Nature Ecology & Evolution**  
Sebastián Tabares-Segovia · `setabaress@unal.edu.co`  
Universidad Nacional de Colombia — Facultad de Ciencias
Readme creado con gpt(v5.0)

---

## 🚀 Aplicación desplegada

🔗 **[https://tabares-segovia.shinyapps.io/nature-eco-evo-dashboard/](https://tabares-segovia.shinyapps.io/nature-eco-evo-dashboard/)**

---

## 📋 Descripción

Aplicación interactiva desarrollada en **Shiny (R)** que permite consultar, visualizar y actualizar la base de datos SQLite construida en el Taller 1, implementando el ciclo completo del proceso **KDD** visto en clase.

| Etapa KDD       | Implementación                                         |
|-----------------|--------------------------------------------------------|
| Recolección     | API OpenAlex + scraping `rvest` sobre nature.com       |
| Almacenamiento  | Base de datos SQLite (`nature_eco_evo_2025.sqlite`)    |
| Consulta        | Filtros dinámicos construidos en SQL desde R           |
| Visualización   | 4 gráficos interactivos con `highcharter`              |
| Actualización   | Botón de scraping para artículos nuevos (2026)         |

---

## 📁 Estructura de archivos

```
taller_parcial_02/
├── app.R                          # Aplicación Shiny (1121 líneas)
├── nature_eco_evo_2025.sqlite     # Base de datos (373 artículos, 14.981 referencias)
├── taller_2_Tabares-Segovia_3.qmd # Documento Quarto
├── taller_2_Tabares-Segovia_3.pdf # Informe PDF
├── www/
│   ├── logo_nature.png
│   └── logo_unal.png
└── README.md
```

---

## ⚙️ Requerimientos cumplidos

### Widgets interactivos (11 — mínimo requerido: 3)
- `dateRangeInput()` — rango de fechas
- `checkboxGroupInput()` — categorías temáticas
- `selectInput()` — lista desplegable
- `textInput()` × 3 — título, autor, DOI
- `actionButton()` × 2 — aplicar filtros, scraping
- `downloadButton()` × 2 — descarga CSV
- `DT::datatable()` × 2 — tabla principal y modal

### Indicadores descriptivos (7 — mínimo requerido: 5)
| Indicador | Valor |
|-----------|-------|
| Total artículos | 373 |
| Promedio autores/paper | 8.9 |
| Promedio citas/paper | 3.7 |
| Promedio referencias/paper | 48.7 |
| Papers con Accesses | 324 |
| Artículo más citado | *Restoration cannot be scaled up globally…* (41 citas) |
| Artículo con más Accesses | *Shotgun sequencing of airborne eDNA…* (48,000 Accesses) |

### Visualizaciones interactivas (4 — mínimo requerido: 2)
- 🍩 Donut chart — artículos por categoría temática
- 📈 Serie temporal — evolución mensual de publicaciones
- 📊 Histograma — distribución de citas
- 👤 Barras horizontales — Top 10 autores

> Cada gráfico responde al clic mostrando un **modal** con los artículos del subconjunto seleccionado.

---

## 🗄️ Base de datos

- **Tabla `papers`**: 373 registros, 14 columnas
- **Tabla `paper_references`**: 14.981 pares de citas
- **Fuentes**: API OpenAlex + scraping nature.com (Accesses)
- **Revista**: Nature Ecology & Evolution (ISSN 2397-334X, Q1)

---

## 📦 Paquetes utilizados

`shiny` · `bslib` · `DT` · `dplyr` · `stringr` · `tibble` · `DBI` · `RSQLite` · `httr2` · `purrr` · `highcharter`
