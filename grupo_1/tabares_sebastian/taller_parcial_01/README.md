# Taller parcial 1 — Tabares Segovia, Sebastián
**Asignatura:** Minería de Datos (2016325)  
**Entrega:** Taller parcial 1 · 25 % de la nota final  

---

## Descripción

Este taller construye dos bases de datos -una finalizada Nature Ecology and Evolution, y una sin finalizar, PNAS - en SQLite a partir de artículos científicos publicados en 2025, recolectados mediante técnicas de web scraping y consulta a la API de OpenAlex. Se realizan 11 consultas SQL sobre cada base de datos para responder preguntas analíticas sobre patrones de autoría, temáticas, citas y métricas de uso.

---

## Revistas seleccionadas

### 1. Nature Ecology & Evolution *(entrega principal)*
- **ISSN:** 2397-334X · Clasificación Q1
- **Artículos recolectados:** 325 artículos publicados en 2025
- **Fuente de metadatos:** API de OpenAlex (título, autores, DOI, abstract, citas, referencias)
- **Métrica de descargas:** *Accesses* (vistas totales), extraída directamente del HTML público de `nature.com` mediante `rvest`. Se verificó que el sitio responde HTTP 200 sin bloqueo y que el dato está disponible en el HTML estático.
- **Archivo R Markdown:** Taller-Parcial_1_Tabares-Segovia.Rmd`
- **Base de datos:** `nature_eco_evo_2025.sqlite`
- **CSV de Accesses:** `nature_accesses.csv`

### 2. PNAS *(entrega complementaria)*
- **ISSN:** 0027-8424 · Clasificación Q1
- **Artículos recolectados:** ~3.500 artículos publicados en 2025
- **Fuente de metadatos:** API de OpenAlex
- **Métrica de descargas:** no disponible. Se intentaron cuatro estrategias sin éxito: scraping directo a `pnas.org` (HTTP 403 Cloudflare), API de Altmetric (HTTP 403), API de iCite del NIH (no indexa artículos recientes de PNAS), y campo `counts_by_year` de OpenAlex (solo reporta citas, no vistas). La variable `downloads` se dejó como NULL con justificación en el documento.
- **Base de datos:** `pnas_2025.sqlite`

---

## Archivos entregados

| Archivo | Descripción |
|---|---|
| `Taller-Parcial_1_Tabares-Segovia.Rmd` | Documento reproducible principal (Nature Eco & Evo) |
| `nature_eco_evo_2025.sqlite` | Base de datos SQLite con 325 papers y 14.981 referencias |
| `nature_accesses.csv` | CSV con los Accesses de cada artículo (checkpoint del scraping) |
| `pnas_2025.sqlite` | Base de datos SQLite de PNAS 2025 (downloads = NULL, entrega complementaria) |

---
