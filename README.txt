================================================================================
TALLER 3 - MINERIA DE DATOS (2016325)
Prediccion y estimacion de costos del Trastorno del Espectro Autista (TEA)
Autor: Sebastian Tabares Segovia
================================================================================

--------------------------------------------------------------------------------
1. DESCRIPCION DEL PROYECTO
--------------------------------------------------------------------------------

Este proyecto desarrolla un proceso completo de mineria de datos sobre una base
real de utilizacion de servicios de salud suplementaria en Brasil (~9.3 millones
de registros, ~653.000 beneficiarios). La enfermedad seleccionada es el
Trastorno del Espectro Autista (TEA), identificado mediante los codigos CIE-10
F84.x.

El documento principal (taller_3_Tabares-Segovia.Rmd) construye una variable
objetivo binaria a nivel de beneficiario, entrena y compara cinco modelos de
clasificacion supervisada (Arbol de decision, Random Forest, XGBoost, SVM,
KNN), aplica dos tecnicas de analisis no supervisado (K-means, Isolation
Forest), estima el costo asociado a la atencion de beneficiarios con TEA y
ajusta un modelo de costo esperado (GLM Gamma) que combina simultaneamente
TEA, edad, hospitalizaciones e ingresos a UTI para distintos perfiles de
beneficiario.

--------------------------------------------------------------------------------
2. ESTRUCTURA DE CARPETAS ESPERADA
--------------------------------------------------------------------------------

    carpeta_personal/
    |-- taller_3_Tabares-Segovia.Rmd   <- documento principal (este analisis)
    |-- README.txt                     <- este archivo
    |-- data/
    |   `-- db_2026.csv                <- base de datos original (no versionada)
    `-- (los .rds generados por el propio script: modelos y bases intermedias)

El script guarda varios objetos intermedios (bases curadas, modelos entrenados)
como archivos .rds en el directorio de trabajo mediante saveRDS(), para evitar
reentrenar modelos costosos (Random Forest, XGBoost, SVM, KNN) cada vez que se
vuelve a compilar el documento. Si se ejecuta el analisis desde cero, estos
archivos se generaran automaticamente; no es necesario crearlos a mano.

NOTA: el archivo db_2026.csv NO esta incluido en el repositorio por su tamano
(~1.6 GB). Debe copiarse manualmente dentro de la carpeta data/ antes de
ejecutar el documento.

--------------------------------------------------------------------------------
3. REQUISITOS PARA REPRODUCIR EL ANALISIS
--------------------------------------------------------------------------------

  - R (version 4.2 o superior recomendada) y RStudio.
  - Paquetes de R utilizados a lo largo del documento:

      data.table, tidyverse, lubridate, caret, ggplot2, rpart, rpart.plot,
      randomForest, xgboost, kernlab, e1071, kknn, factoextra, cluster,
      solitude, pROC, PRROC, tidyr, knitr, scales

    Se pueden instalar todos de una vez con:

      install.packages(c(
        "data.table", "tidyverse", "lubridate", "caret", "ggplot2",
        "rpart", "rpart.plot", "randomForest", "xgboost", "kernlab",
        "e1071", "kknn", "factoextra", "cluster", "solitude",
        "pROC", "PRROC", "scales"
      ))

--------------------------------------------------------------------------------
4. PASOS PARA REPRODUCIR EL ANALISIS
--------------------------------------------------------------------------------

  1. Clonar/descargar esta carpeta personal del repositorio de la clase.
  2. Crear la subcarpeta data/ (si no existe) y copiar alli el archivo
     db_2026.csv proporcionado por el curso.
  3. Abrir taller_3_Tabares-Segovia.Rmd en RStudio.
  4. Verificar que el working directory sea la carpeta personal (donde esta
     el .Rmd), de forma que la ruta relativa "data/db_2026.csv" funcione. En
     RStudio esto ocurre automaticamente si se abre el proyecto/archivo desde
     esa carpeta.
  5. Ejecutar "Knit" (o Run All). El documento vuelve a leer la base completa,
     recalcula la limpieza y las variables, reentrena los cinco modelos
     supervisados y las dos tecnicas no supervisadas, y genera el reporte en
     HTML.

  Nota de tiempo de ejecucion: por el tamano de la base (~9.3 millones de
  registros) y el entrenamiento de varios modelos (en particular Random
  Forest, XGBoost y la validacion cruzada de KNN), la compilacion completa
  puede tardar varios minutos.

--------------------------------------------------------------------------------
5. SOBRE LA NUMERACION DE SECCIONES
--------------------------------------------------------------------------------

Los encabezados del .Rmd tienen numeracion manual (1, 2, 2.1, 2.1.1, etc.) y
la opcion number_sections del YAML se dejo en false a proposito, para que
Pandoc no agregue una numeracion automatica adicional que chocaria con la
numeracion manual. La jerarquia de secciones de la seccion 6 de este README
debe coincidir exactamente con los encabezados del documento renderizado.

--------------------------------------------------------------------------------
6. JERARQUIA COMPLETA DE SECCIONES DEL DOCUMENTO
--------------------------------------------------------------------------------

1. Introduccion
2. Metodologia
3. Comprension y carga inicial de los datos
    3.1 Variables disponibles
    3.2 Verificacion de los codigos asociados al TEA
4. Limpieza y preparacion de los datos
    4.1 Construccion de la variable objetivo
    4.2 Construccion de la base analitica
        4.2.1 Revision de la consistencia de los datos
        4.2.2 Agrupacion por beneficiario: variable objetivo y variables demograficas
        4.2.3 Variables de utilizacion y costos
            4.2.3.1 Construccion de la variable edad
5. Analisis exploratorio de la base analitica
    5.1 Balance de clases
    5.2 Variables demograficas
    5.3 Distribucion por estado y especialidad
        5.3.1 Distribucion por estado (UF_CNES_PREST_HOSPITALAR)
        5.3.2 Distribucion por especialidad medica
    5.4 Tipo de beneficiario
    5.5 Comparacion de las caracteristicas segun la presencia de TEA
        5.5.1 Edad segun la presencia de TEA
        5.5.2 Sexo segun la presencia de TEA
        5.5.3 Tipo de beneficiario segun la presencia de TEA
    5.6 Comparacion de la utilizacion de servicios de salud
        5.6.1 Numero de utilizaciones
        5.6.2 Numero de procedimientos distintos
        5.6.3 Numero de especialidades distintas
        5.6.4 Hospitalizaciones
        5.6.5 Ingresos a UTI
    5.7 Analisis de valores extremos en los costos
        5.7.1 Valores extremos en VALOR_UTILIZACAO a nivel de registro
        5.7.2 Costo total
            5.7.2.1 Datos atipicos
            5.7.2.2 Guardado de la base analitica curada
6. Modelamiento predictivo
    6.1 Cargar la base analitica curada
    6.2 Preparacion para el modelamiento
    6.3 Division de los datos
    6.4 Validacion cruzada
7. Analisis supervisado
    7.1 Arbol de decision
        7.1.1 Predicciones del arbol
        7.1.2 Curva ROC
        7.1.3 Area bajo la curva ROC
        7.1.4 Curva Precision-Recall
        7.1.5 Funcion para evaluar los modelos
        7.1.6 Evaluacion automatica del arbol de decision
        7.1.7 Guardado del arbol de decision
    7.2 Random Forest
        7.2.1 Predicciones de Random Forest
        7.2.2 Evaluacion de Random Forest
    7.3 XGBoost
        7.3.1 Preparacion de las matrices para XGBoost
        7.3.2 Evaluacion de XGBoost
        7.3.3 Importancia de las variables en XGBoost
        7.3.4 Guardado del modelo XGBoost
        7.3.5 Interpretacion del analisis supervisado
8. Analisis no supervisado
    8.1 Escalamiento de las variables
        8.1.1 Guardado de la base escalada
    8.2 Support Vector Machine (SVM)
        8.2.1 Ajuste computacional para SVM
    8.3 K-Nearest Neighbors (KNN)
    8.4 Agrupamiento mediante K-means
        8.4.1 Determinacion de numero optimo de grupos
        8.4.2 Ajuste del algoritmo K-means
            8.4.2.1 Interpretacion de los clusteres
            8.4.2.2 Relacion entre los clusteres y la presencia de TEA
            8.4.2.3 Interpretacion de la relacion entre los clusteres y la presencia de TEA
    8.5 Deteccion de anomalias mediante Isolation Forest
        8.5.1 Interpretacion de los resultados
9. Comparacion de los modelos supervisados
    9.1 Interpretacion
    9.2 Importancia de las variables
        9.2.1 Interpretacion
10. Estimacion del costo asociado al TEA
    10.1 Distribucion general del costo
        10.1.1 Interpretacion
    10.2 Costo segun presencia de TEA
    10.3 Relacion entre edad y costo
    10.4 Hospitalizaciones y costo
    10.5 Ingresos a UTI y costo
    10.6 Modelo de costo esperado segun perfil del beneficiario
11. Conclusiones
    11.1 Limitaciones
    11.2 Trabajos futuros

--------------------------------------------------------------------------------
7. ACLARACION: DE 6 ETAPAS A 11 SECCIONES
--------------------------------------------------------------------------------

La seccion 2 (Metodologia) del documento presenta el trabajo como el
desarrollo de 6 etapas conceptuales del proceso KDD (Knowledge Discovery in
Databases):

    1. Comprension del problema
    2. Seleccion y preparacion de los datos
    3. Analisis exploratorio
    4. Modelamiento predictivo
    5. Evaluacion e interpretacion
    6. Generacion de conocimiento

Sin embargo, como se ve en la jerarquia de la seccion 6 de este README, el
documento final termina organizado en 11 secciones principales (numeradas
de la 1 a la 11), no en 6. Esto NO es una inconsistencia entre lo prometido
y lo entregado, sino el resultado esperable de operacionalizar esas 6 etapas
sobre una base de 9.3 millones de registros: varias etapas conceptuales se
dividieron en secciones independientes para que cada una fuera mas facil de
seguir y de evaluar por separado. En particular:

    - "Seleccion y preparacion de los datos" (etapa 2) se dividio en:
        3. Comprension y carga inicial de los datos
        4. Limpieza y preparacion de los datos

    - "Analisis exploratorio" (etapa 3) corresponde a:
        5. Analisis exploratorio de la base analitica

    - "Modelamiento predictivo" (etapa 4) se dividio en:
        6. Modelamiento predictivo (particion y validacion cruzada)
        7. Analisis supervisado (los 5 algoritmos de clasificacion)
        8. Analisis no supervisado (K-means e Isolation Forest, que el
           enunciado no exige pero se incluyeron como analisis adicional)

    - "Evaluacion e interpretacion" (etapa 5) corresponde a:
        9. Comparacion de los modelos supervisados

    - "Generacion de conocimiento" (etapa 6) se dividio en:
        10. Estimacion del costo asociado al TEA
        11. Conclusiones

Se deja esta aclaracion explicita en el README (y una nota equivalente dentro
del propio .Rmd, al final de la seccion de Metodologia) para que la
numeracion de las 11 secciones no se lea como una desviacion no explicada
del plan de trabajo presentado al inicio del documento.

--------------------------------------------------------------------------------
8. LIMITACIONES CONOCIDAS
--------------------------------------------------------------------------------

  - La estimacion de costos (seccion 10.6) usa un modelo Gamma con enlace
    logaritmico (GLM), que es una estrategia asociativa y no causal. No se
    evaluo formalmente su bondad de ajuste (por ejemplo, con validacion
    cruzada del error de prediccion) ni se comparo con especificaciones
    alternativas (regresion log-lineal, modelos de dos partes, etc.).
  - Variables como el numero de especialidades consultadas y los costos
    acumulados, usadas como predictoras del TEA (seccion 7) y tambien
    incluidas en el modelo de costo esperado (seccion 10.6), reflejan en
    parte la atencion multidisciplinaria que reciben los beneficiarios YA
    diagnosticados con TEA. Esto se discute como limitacion en la seccion
    11.1 del documento.
  - La distribucion por estado (seccion 5.3.1) se calcula a nivel de
    utilizacion (UF_CNES_PREST_HOSPITALAR), no a nivel de beneficiario, y
    no se incorporo como variable predictora en los modelos de la seccion 7.

--------------------------------------------------------------------------------
9. PAQUETES ADICIONALES REQUERIDOS POR LAS SECCIONES 5.3, 5.7.1 Y 10.6
--------------------------------------------------------------------------------

Las secciones agregadas para cubrir la distribucion por estado/especialidad,
los valores extremos de VALOR_UTILIZACAO a nivel de registro y el modelo de
costo esperado NO requieren paquetes adicionales a los ya listados en la
seccion 3 de este README: usan unicamente dplyr/ggplot2 (ya cargados via
tidyverse) y la funcion glm() con family = Gamma(link = "log"), que forma
parte del paquete stats incluido en la instalacion base de R.

================================================================================
