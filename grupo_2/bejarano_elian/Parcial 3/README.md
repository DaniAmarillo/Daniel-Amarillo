# Taller 3 - Cardiopatia isquemica

Este directorio contiene el analisis del Taller 3 de Mineria de Datos para la enfermedad:

**Cardiopatia isquemica - CID I20-I25**

## Estructura

- `scripts/01_prepare_features.py`: prepara la base, construye la variable objetivo y agrega variables por beneficiario.
- `scripts/02_modelos.py`: entrena regresion logistica, arbol, Random Forest y Gradient Boosting.
- `scripts/03_graficos_descriptivos.py`: genera graficos descriptivos adicionales.
- `data/processed/features_cardio_beneficiario.csv.gz`: tabla final comprimida a nivel de beneficiario.
- `output/tablas/`: tablas de metricas, costos, calidad de datos e interpretabilidad.
- `output/figuras/`: graficos del informe.
- `taller_3.Rmd`: informe principal renderizable con tablas `kableExtra`.
- `taller_3.html`: informe renderizado en HTML.
- `taller_3.md`: version estatica de respaldo.

## Reproduccion

La base original no se versiona por su tamano. Por defecto los scripts buscan:

```powershell
C:\Users\stive\Downloads\db_2026.csv
```

Para usar otra ruta:

```powershell
$env:DB_2026_PATH = "C:\ruta\a\db_2026.csv"
```

Instalar dependencias:

```powershell
python -m pip install -r requirements.txt
```

Para renderizar el informe con tablas bonitas en R:

```r
install.packages(c("rmarkdown", "knitr", "kableExtra"))
rmarkdown::render("taller_3.Rmd")
```

Si `rmarkdown` no encuentra Pandoc en Windows, usar el Pandoc incluido en RStudio antes de renderizar:

```powershell
$env:RSTUDIO_PANDOC = "C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools"
& "C:\Program Files\R\R-4.4.0\bin\Rscript.exe" -e "rmarkdown::render('taller_3.Rmd')"
```

Ejecutar:

```powershell
python scripts\01_prepare_features.py --input "C:\Users\stive\Downloads\db_2026.csv"
python scripts\02_modelos.py
python scripts\03_graficos_descriptivos.py
```

Nota metodologica: la etiqueta se construye con registros CID I20-I25, pero las variables predictoras agregadas excluyen esas mismas filas para reducir fuga de informacion.
La evaluacion usa particion estratificada train/test y el entrenamiento conserva todos los positivos con un submuestreo reproducible de negativos en razon 50:1.
