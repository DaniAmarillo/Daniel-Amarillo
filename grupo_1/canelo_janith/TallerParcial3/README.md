# Cómo usar esto (flujo de 2 pasos)

## Paso 1 — Entrenar (una sola vez, o cuando cambies datos/modelos)

Desde una terminal, en la carpeta donde tengas `entrenar.py`:

```bash
python entrenar.py
```

- Ajusta primero la ruta del CSV en la línea `PATH = r"C:\Users\johan\Downloads\db_2026.csv"` si cambia.
- **El tuning de Random Forest (`RandomizedSearchCV`, 200 fits, ~4 horas) NO se vuelve a correr por defecto.** El script ya trae hardcodeados los hiperparámetros óptimos que esa búsqueda encontró (`MEJORES_PARAMS_RF`, en la sección 0 del script), así que por defecto solo entrena el modelo final con esos parámetros — toma segundos.
  - El código completo de la búsqueda sigue ahí (sección 9, dentro de `if RUN_TUNING: ...`), documentado y sin borrar, por si el profesor quiere ver exactamente cómo se hizo el tuning.
  - Si cambias las variables/features o el CSV y quieres verificar/refrescar el tuning, pon `RUN_TUNING = True` en la sección 0 y vuelve a correr — ahí sí tomará varias horas, y al final te dirá los nuevos mejores parámetros para que actualices `MEJORES_PARAMS_RF`.
- El resto del script (limpieza, features, comparación de 7 modelos, CV repetida, importancia de variables, GLM de costo) sí corre siempre — es rápido en comparación con el tuning.
- Al terminar, vas a tener una carpeta `artifacts/` al lado del script, con todas las tablas (`.csv`), métricas (`.json`), figuras (`.png` en `artifacts/figuras/`) y el modelo entrenado (`modelo_final.pkl`).

## Paso 2 — Renderizar el reporte (rápido, cuantas veces quieras)

Copia `taller3_cancer_mama.qmd` **a la misma carpeta** donde quedó `artifacts/` (o mueve `artifacts/` junto al `.qmd`), y corre:

```bash
quarto render taller3_cancer_mama.qmd
```

Esto debería tomar **segundos**, porque el `.qmd` no entrena nada — solo lee lo que ya está en `artifacts/`.