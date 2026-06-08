# Entrega 2: Springer Visual Miner | Plataforma de Minería de Datos

**Estudiante:** Winston Obeymar Lucano Villota 
**Materia:** Mineria De Datos
**Profesor:** Andres Felipe Florez Rivera  

---

## Enlace de la Aplicación en Vivo
La aplicación ha sido desplegada exitosamente en ShinyApps.io y está lista para su evaluación:
**[ CLIC AQUÍ PARA ABRIR LA APLICACIÓN ](https://qwbdau-wlucano.shinyapps.io/entrega_2/)**

---

## Guía de Evaluación para el Profesor

Para revisar todas las funcionalidades técnicas implementadas en esta plataforma, sugiero seguir este flujo de revisión:

### 1. Exploración del Dashboard Principal (Pestaña 1)
Al abrir la aplicación, observará un panel de control con indicadores clave de rendimiento (KPIs) y gráficos interactivos (`highcharter`). 
* **Detalle técnico:** Estos gráficos están conectados reactivamente a una base de datos local `SQLite`. Su renderizado está optimizado para no bloquear la interfaz principal.

### 2. Prueba del "Literature Explorer" (Pestaña 3)
Esta es una de las secciones más robustas de la aplicación. Para evaluarla:
1. Diríjase a la pestaña **Literature Explorer**.
2. **Haga clic en cualquier fila** de la tabla de artículos.
3. Observe cómo en la parte inferior se despliega de forma dinámica e instantánea una tarjeta de detalles del artículo.
* **Detalles técnicos a observar:**
  * **Persistencia de UI:** Se implementó una solución avanzada en el servidor (`suspendWhenHidden = FALSE`) para evitar que el motor interno de Shiny suspenda los elementos del DOM al cambiar de pestañas, garantizando una experiencia de usuario sin interrupciones ni pantallas en blanco.
  * **Extracción limpia del DOI:** Se programó un validador que extrae y muestra el DOI (Digital Object Identifier) con una tipografía monoespaciada para facilitar su copia, manejando excepciones (NAs) en caso de que el artículo no cuente con uno.
  * **Consulta relacional:** Los autores se extraen mediante un `JOIN` dinámico a la base de datos `SQLite` (`paper_authors` y `authors`) basado en la fila seleccionada.

### 3. Motor de Sincronización / Scraping (Panel de Administración)
En esta sección se encuentra el motor de actualización de datos.
* **Detalle técnico:** La función de extracción (Scraping) está encapsulada en bloques `tryCatch` con barras de progreso interactivas (`withProgress`).
* **Trigger Reactivo Nativo:** Cuando el scraping termina y actualiza la base de datos SQLite, se dispara un evento reactivo (`datos_reactivos()`). Esto avisa automáticamente al **Dashboard** y al **Literature Explorer** para que redibujen sus gráficas y tablas en tiempo real con los nuevos datos, todo esto en segundo plano (asíncrono) sin requerir que el usuario refresque la página del navegador.

---

## Arquitectura y Tecnologías Utilizadas

* **Framework Web:** Shiny (con `shinyjs` para inyección de JavaScript)
* **Base de Datos:** Motor `RSQLite` integrado.
* **Manejo de Datos:** `dplyr`, `tidyr` (Tidyverse)
* **Visualización:** * `highcharter` (Gráficos interactivos de alto rendimiento).
  * `DT` (DataTables interactivas con auto-ajuste responsivo).
* **UI/UX:** CSS personalizado con diseño *Glassmorphism* (efecto cristal) y variables de paleta oscura (Dark Mode).
---



