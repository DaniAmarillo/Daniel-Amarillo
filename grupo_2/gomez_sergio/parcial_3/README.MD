# Desarrollo parcial 3

## 1. Comprension del problema

### 1.1 Contexto

La base de datos corresponde a registros reales de utilización de servicios de salud (formato tipo TISS/ANS, Brasil), donde cada fila representa una atención o procedimiento asociado a un beneficiario de un plan de salud. Este tipo de bases permite estudiar patrones de uso de servicios médicos asociados a enfermedades específicas, lo cual es relevante tanto desde una perspectiva clínica (identificación temprana de pacientes en riesgo) como desde una perspectiva actuarial/financiera (estimación de costos esperados para la aseguradora).\\

La enfermedad seleccionada para este trabajo es la infección por coronavirus, una condición de alta relevancia en salud pública dado su impacto epidemiológico reciente (pandemia COVID-19) y su efecto sobre la utilización de servicios hospitalarios, UCI e internación.\\

### 1.2 Justificación de la enfermedad seleccionada

Se eligió esta condición por tres razones:

- _Relevancia epidemiológica y de negocio:_ la infección por coronavirus generó picos significativos de demanda de servicios de salud (consultas, exámenes, internaciones, UCI), lo que la hace un caso de estudio natural para minería de datos en salud.
- _Disponibilidad de codificación estandarizada:_ existen códigos CID-10 específicos y bien documentados para esta condición, lo que facilita la construcción reproducible de la variable objetivo.
- _Riqueza de variables predictoras potencialmente asociadas:_ variables como INTERNADO, UTI, PORTE_ANESTESICO, DESC_ESPECIALIDADE y edad tienen una relación clínica plausible con la severidad y manejo de esta infección.

### 1.3 Codigos objetivo

Para este trabajo, interesaron especialmente los codigos B34.2 Infección por coronavirus, de sitio no especificado y U07.1 COVID-19, virus identificado (confirmado por laboratorio), ya que son los codigos presentes en la base de datos que refieren a la patologia de interes expuesta anteriormente

## 2. Preparacion de los datos

La base de datos original contaba con **9.345.278 registros**. A continuación se describen
las decisiones de limpieza tomadas sobre cada variable, junto con la justificación de cada
una. Al finalizar el proceso se conservaron **8.510.292 registros (91 % del total)**.

---

### 2.1 Variable `CID`

La exploración inicial reveló tres categorías de valores nulos con notaciones distintas:
**7.667.147** registros marcados como `NA`, **613.585** como cadena vacía y **2.101** como
guion (`-`). Todos fueron unificados bajo la etiqueta `no_clasificado` para mantener
trazabilidad sin eliminar registros que aún contienen información útil en otras variables.

Adicionalmente se detectaron dos variantes del código objetivo sin puntuación (`B342`,
`U071`), que fueron normalizadas a su forma canónica `B34.2` y `U07.1` respectivamente.
Todos los valores restantes fueron llevados a mayúsculas y sin espacios laterales.

---

### 2.2 Variables `UTI`, `INTERNADO` y `PORTE_ANESTESICO`

`UTI` e `INTERNADO` se revisaron sin requerir transformaciones adicionales. Para
`PORTE_ANESTESICO` se imputaron los valores nulos con `0`, interpretando su ausencia como
indicador de que no se aplicó anestesia en dicha utilización, lo cual es consistente con
el contexto clínico de una consulta o examen ambulatorio.

---

### 2.3 Variable `DT_UTILIZACAO`

Se eliminaron los registros cuya fecha de utilización era **posterior a la fecha de
ejecución del análisis**, ya que una atención futura no puede ser un registro real y
compromete la integridad del modelo. Este filtro afecta únicamente registros con fechas
claramente erróneas.

---

### 2.4 Variable `DESC_ESPECIALIDADE`

Se aplicó una normalización en tres pasos:

1. **Estandarización de formato**: eliminación de acentos y caracteres no ASCII,
   conversión a mayúsculas y eliminación de espacios laterales.
2. **Unificación de nulos**: los valores `"-"`, `"NAO INFORMADA"`, `"NAO INFORMADO"` y
   cadena vacía fueron reemplazados por `no_clasificado`.
3. **Corrección de inconsistencias léxicas**: se unificaron variantes de una misma
   especialidad que diferían por errores tipográficos o notación alternativa, por ejemplo
   `CARDIO VASCULAR` → `CARDIOVASCULAR`, `TORAXICA` → `TORACICA`,
   `ANESTESIOLOGISTA` → `ANESTESIOLOGIA`, entre otras.

Se identificó además que algunos registros contenían el título profesional en lugar del
nombre de la especialidad. Esta limitación se documenta como restricción de la base y no
fue posible resolverla sin una tabla de equivalencias externa.

---

### 2.5 Variables `TIPO_UNIDADE_PREST_HOSPITALAR` y `UF_CNES_PREST_HOSPITALAR`

En ambas variables los valores nulos estaban representados como `"-"`. Se unificaron bajo
`no_clasificado` y se estandarizaron a mayúsculas. No se encontraron otras
inconsistencias relevantes.

---

### 2.6 Variable `DT_NASCIMENTO_BENEFICIARIO`

Esta variable requirió una estrategia de imputación en dos etapas, dado que algunos
beneficiarios tenían fechas de nacimiento inválidas (nulas o posteriores a la fecha
actual) en ciertos registros, pero contaban con fechas válidas en otros registros de la
misma base:

1. **Imputación desde registros válidos del mismo beneficiario**: se extrajeron las fechas
   válidas por `CHAVE_FUNCIONAL` y se usaron para reemplazar las fechas inválidas de ese
   mismo usuario.
2. **Eliminación de registros irrecuperables**: aquellos registros cuya fecha permaneció
   inválida tras la imputación (sin fecha válida de referencia disponible) fueron
   descartados.

Posteriormente se aplicó un filtro adicional de coherencia clínica: se eliminaron los
registros en los que la **fecha de nacimiento era posterior a la fecha de utilización**,
ya que un beneficiario no puede recibir atención antes de nacer. Estos registros
presentan contradicción interna y no son recuperables.

---

### 2.7 Variables `TIPO_BENEFICIARIO` y `SEXO_BENEFICIARIO`

En `TIPO_BENEFICIARIO` la categoría `"Não Informado"` fue reclasificada como `OUTROS`,
unificándola con los demás casos sin clasificación.

En `SEXO_BENEFICIARIO` se estandarizó `"MASCULINO"` a `"M"` para homogeneizar la
notación con los demás valores de la variable, y `"Não Informado"` fue igualmente
reclasificado como `OUTROS`.

---

### 2.8 Consistencia demográfica por beneficiario (`SEXO` , `DT_NASCIMENTO` y `TIPO_BENEFICIARIO`)

Dado que la base opera a nivel de transacción, un mismo beneficiario puede aparecer en
múltiples registros. Se verificó que `SEXO_BENEFICIARIO` , `DT_NASCIMENTO_BENEFICIARIO` y
`TIPO_BENEFICIARIO` fueran constantes dentro de cada `CHAVE_FUNCIONAL`.

La estrategia adoptada fue:

- Calcular la **moda** de cada variable por beneficiario.
- Si existe una **moda clara** (un valor con frecuencia estrictamente mayor a los demás),
  todos los registros del beneficiario se actualizan con ese valor.
- Si existe un **empate** (dos o más valores con la misma frecuencia máxima), no es
  posible determinar el valor correcto sin información externa, por lo que **todos los
  registros de ese beneficiario son descartados**.

Esta decisión es conservadora pero garantiza que las variables demográficas utilizadas en
el modelo sean confiables y no introduzcan ruido por inconsistencias entre transacciones.

---

### 2.9 Variable `CETIPO`

Se estandarizó a mayúsculas y se eliminaron caracteres no ASCII. No se requirió ninguna
otra transformación.

---

### 2.10 Variables `CD_PROCEDIMENTO` y `DESCRICAO_PROCEDIMENTO`

Se estandarizaron las descripciones a mayúsculas y caracteres ASCII. Luego se verificó la
consistencia entre código y descripción mediante la evaluación cruzada de pares únicos:

- **Descripciones con más de un código asociado**: detectadas. Indican que una misma
  descripción fue registrada con distintos códigos en diferentes transacciones.
- **Códigos con más de una descripción asociada**: no detectados. Cada código corresponde
  a una única descripción.

Dado que el problema es unidireccional (descripción → múltiples códigos, pero no al
revés), la estandarización se realizó conservando el **primer código observado** para cada
descripción, eliminando así la ambigüedad sin descartar registros.

---

### 2.11 Variable `VALOR_UTILIZACAO`

Se eliminaron los registros con valor negativo, ya que corresponden a devoluciones o
ajustes contables que no representan una utilización real de servicios de salud y podrían
distorsionar tanto el modelo predictivo como la estimación de costos.

---

### 2.12 Construcción de la variable objetivo

Se identificaron los beneficiarios con al menos un registro asociado a los códigos CID
`U07.1` (COVID-19 confirmado) o `B34.2` (infección por coronavirus no especificada).
A estos se les asignó `OBJETIVO = 1`; al resto, `OBJETIVO = 0`.

## 3. Analisis descriptivo

### 3.1 Resumen global de la base

Tras el proceso de preparación y limpieza, la base de datos quedó conformada por:

| Métrica                          | Valor     |
| -------------------------------- | --------- |
| Beneficiarios únicos             | 648.990   |
| Utilizaciones o consultas        | 2.235.408 |
| Tipos de procedimiento distintos | 3.220     |

Cada beneficiario generó en promedio **3,44 utilizaciones** durante el periodo analizado
(`2.235.408 / 648.990`), lo que refleja una base con alta densidad de registros por
persona y variedad moderada en el catálogo de procedimientos.

---

### 3.2 Distribución de la variable objetivo

```r
  OBJETIVO       n  Etiqueta    Porcentaje
         0  648976  Sin COVID-19      ~100
         1      14  COVID-19          0.00
```

La variable objetivo presenta un **desbalance extremo**: solo **14 beneficiarios
(0,002 %)** cuentan con al menos un registro asociado a los códigos CID `U07.1` o
`B34.2`, mientras que los **648.976 restantes (99,998 %)** no presentan diagnóstico de
infección por coronavirus en la base.

Este nivel de desbalance tiene implicaciones directas sobre la estrategia de modelamiento:
métricas como _accuracy_ son poco informativas en este contexto (un modelo que prediga
siempre 0 alcanzaría ~100 %), por lo que la evaluación deberá basarse en **sensibilidad,
especificidad, F1-score y PR-AUC**. Adicionalmente, será necesario considerar técnicas de
balanceo de clases (SMOTE, submuestreo, pesos por clase) durante el entrenamiento.

---

### 3.3 Distribución por sexo

```r
  SEXO       n    Porcentaje
  F     357002          55.0
  M     261774          40.3
  OUTROS  30214           4.7
```

La mayoría de los beneficiarios son de sexo femenino (**55 %**), seguidos por masculino
(**40,3 %**). Un **4,7 %** quedó clasificado como `OUTROS` al no contar con información
de sexo confiable, lo cual fue documentado en la etapa de preparación.

---

### 3.4 Distribución por grupo de edad

```r
  grupo_edad       n    pct
  0-17        137892   21.2
  18-29        88419   13.6
  30-44       173749   26.8
  45-59       129064   19.9
  60-74        78605   12.1
  75+          41261    6.4
```

El grupo de edad más representado es el de **30 a 44 años (26,8 %)**, seguido por los
grupos de **0 a 17 años (21,2 %)** y **45 a 59 años (19,9 %)**. Los adultos mayores de
60 años representan en conjunto el **18,5 %** de la población analizada. Esta distribución
es relevante para el análisis de coronavirus, dado que la literatura clínica señala a los
grupos de mayor edad como los de mayor riesgo de severidad.

---

### 3.5 Distribución por tipo de beneficiario

```r
  TIPO_BENEFICIARIO       n    pct
  TITULAR            335978   51.8
  DEPENDENTE         269492   41.5
  OUTROS              35249    5.4
  AGREGADO             5874    0.9
  FILHO                1107    0.2
  CONJUGE              1104    0.2
  IGNORADO              184    0.0
  MAE                     1    0.0
  TUTELADO                1    0.0
```

Más de la mitad de los beneficiarios son **titulares (51,8 %)** y el **41,5 %** son
dependientes, lo que es consistente con la estructura típica de un plan de salud
corporativo o familiar. Las categorías `FILHO`, `CONJUGE`, `MAE` y `TUTELADO` tienen
representación marginal y podrían ser agrupadas bajo `OUTROS` en la etapa de
construcción de variables para el modelo.

---

### 3.6 Distribución por estado de atención (top 5)

```r
  UF_CNES_PREST_HOSPITALAR        n    pct
  SP                         4391852   51.6
  no_clasificado             2381444   28.0
  PE                          521911    6.1
  MT                          420373    4.9
  RJ                          314716    3.7
```

El estado de **São Paulo (SP)** concentra más de la mitad de los registros (**51,6 %**),
lo que sugiere que la población asegurada se encuentra mayoritariamente en esa región.
Preocupa el alto porcentaje de registros **sin estado identificado (28 %)**, que
constituye la segunda categoría más frecuente y representa una limitación importante para
cualquier análisis geográfico.

---

### 3.7 Distribución por especialidad médica (top 5)

```r
  DESC_ESPECIALIDADE                         n    pct
  no_clasificado                       6214506   84.7
  MEDICO CLINICO                        595887    8.1
  PATOLOGIA CLINICA MEDICINA LABORATORIAL 192846  2.6
  ANALISES CLINICAS DIVERSAS            169532    2.3
  CLINICA MEDICA                        163924    2.2
```

El **84,7 %** de los registros no cuenta con especialidad identificada, lo que constituye
la limitación más severa de esta variable. Esto se debe en parte a que muchos registros
contenían el título profesional en lugar del nombre de la especialidad, como se documentó
en la etapa de preparación. Entre los registros clasificados, **Médico Clínico (8,1 %)**
es la especialidad más frecuente, seguida de especialidades de laboratorio y análisis
clínicas. Esta variable tendrá utilidad limitada como predictora en el modelo dado su alto
porcentaje de valores no clasificados.

---

### 3.8 Análisis de valores extremos en `VALOR_UTILIZACAO`

```r
   min   p25  mediana    media    p75      p95       p99       max
  0.01  4.78     18.2  107.81  74.54   273.12   1202.51  628388.7
```

La distribución de costos presenta una **asimetría positiva marcada**: la mediana es de
**R$ 18,20**, pero la media asciende a **R$ 107,81**, evidenciando el efecto de valores
extremos en la parte superior. El percentil 99 alcanza **R$ 1.202,51**, mientras que el
valor máximo registrado es de **R$ 628.388,70**, correspondiente probablemente a
internaciones prolongadas o procedimientos de alta complejidad.

Usando el criterio de Tukey (límite superior = Q3 + 1,5 × IQR = **R$ 179,18**), se
identificaron **630.400 registros con valores extremos (7,41 %)** del total de
procedimientos. Estos registros no serán eliminados de la base, ya que representan
eventos clínicos reales de alta complejidad que son especialmente relevantes para la
estimación de costos asociados a coronavirus. Sin embargo, se tendrán en cuenta en la
etapa de modelamiento de costos, donde se evaluará si requieren un tratamiento especial
(transformación logarítmica, modelo separado para cola de distribución, etc.).

## 4. Modelamiento predictivo

### 4.1 Configuración del experimento

#### 4.1.1 Partición de los datos

El dataset de modelamiento se construyó a nivel de beneficiario (`CHAVE_FUNCIONAL`),
resultando en **648.990 observaciones y 21 variables predictoras**. La variable objetivo
presenta un desbalance extremo: **14 positivos (0,002 %) frente a 648.976 negativos
(99,998 %)**.

Se realizó una partición estratificada 80/20 para garantizar presencia de la clase
positiva en ambos conjuntos:

| Conjunto             | Positivos | Negativos | Total   |
| -------------------- | --------- | --------- | ------- |
| Entrenamiento (80 %) | 7         | 519.185   | 519.192 |
| Test (20 %)          | 7         | 129.791   | 129.798 |

La estratificación es obligatoria en este contexto: sin ella, una partición aleatoria
podría dejar el conjunto de test sin ningún positivo, haciendo la evaluación imposible.

#### 4.1.2 Validación cruzada

Se empleó validación cruzada k-fold estratificada con **k = 5** para la búsqueda de
hiperparámetros. Dado que el conjunto de entrenamiento cuenta con solo 7 positivos,
cada fold contiene aproximadamente 1-2 casos positivos, lo que introduce varianza
considerable en las métricas por fold. Este es un límite estructural del problema, no
del procedimiento.

#### 4.1.3 Tratamiento del desbalance

El ratio de desbalance es de **74.169 negativos por cada positivo**. Se evaluaron dos
estrategias complementarias:

- **Pesos por clase** (`class.weights` / `scale_pos_weight`): asigna un peso de 74.169
  al positivo durante el entrenamiento, penalizando los errores sobre esa clase.
  Aplicado a Regresión Logística, Random Forest y XGBoost.
- **Submuestreo de negativos** (`step_downsample`, `under_ratio = 10`): conserva todos
  los positivos y reduce los negativos a 10 veces su cantidad, generando un conjunto
  balanceado de aproximadamente 77 observaciones para entrenamiento. Aplicado a RF
  submuestreo.

No se utilizó SMOTE dado que con 7 positivos reales los sintéticos generados serían
casi copias exactas de los originales, sin aportar variabilidad real.

---

### 4.2 Modelos entrenados

Se entrenaron y compararon **cinco configuraciones** de modelos, cubriendo tres familias
algorítmicas distintas más un enfoque de detección de anomalías:

#### Modelo 1 — Regresión Logística Lasso

Modelo lineal con regularización L1, que realiza selección automática de variables al
llevar a cero los coeficientes de predictores irrelevantes. Sirve como **baseline
interpretable**. Se ajustó el hiperparámetro `penalty` sobre una grilla de 8 valores en
escala logarítmica (`10⁻⁴` a `10⁰`). El desbalance se compensó mediante pesos inversos
a la frecuencia.

#### Modelo 2 — Random Forest con pesos por clase

Ensamble de 500 árboles de decisión con `mtry` y `min_n` como hiperparámetros a
optimizar. Captura interacciones no lineales y es robusto a outliers en variables de
costo. El peso de 74.169 al positivo busca compensar el desbalance extremo.

#### Modelo 3 — XGBoost

Gradient boosting con cinco hiperparámetros optimizados simultáneamente. El parámetro
`scale_pos_weight = 74.169` compensa el desbalance nativamente, y `eval_metric = "aucpr"`
hace que cada árbol optimice PR-AUC directamente.

#### Modelo 4 — Random Forest con submuestreo

Misma arquitectura que el Modelo 2 pero entrenado sobre el dataset reducido por
submuestreo (approximately 77 observaciones balanceadas). Al no usar pesos extremos, el
modelo ve clases más balanceadas y tiene mayor posibilidad de aprender la frontera de
decisión.

#### Modelo 5 — Isolation Forest (detección de anomalías)

A diferencia de los modelos anteriores, el Isolation Forest **no es un clasificador
binario**: aprende la distribución de los beneficiarios _sin_ coronavirus y asigna un
score de anomalía a cada observación nueva. Los beneficiarios que se alejan de ese
patrón normal reciben scores altos y son flaggeados como probables positivos.

Este enfoque es metodológicamente el más apropiado para este problema porque:

1. No requiere ver positivos durante el entrenamiento. Evita el problema de los 7
   ejemplos en train.
2. No depende de la definición de un peso o ratio de desbalance.
3. Reformula el problema correctamente: con 0,002 % de positivos, detectar coronavirus
   es detectar una anomalía en el espacio de features, no clasificar entre dos clases
   equiparables.

El modelo se entrenó exclusivamente sobre los **519.185 negativos** del conjunto de
entrenamiento con 200 árboles de aislamiento y `sample_size = 256`.

---

### 4.3 Métrica principal y justificación

La métrica principal seleccionada es la **ROC-AUC**, complementada con sensibilidad y
especificidad.

Con 99,998 % de negativos, el _accuracy_ es trivialmente alto, un modelo que prediga
siempre negativo alcanzaría ~100 %, lo que lo hace completamente no informativo. La
**PR-AUC** es en teoría la métrica más robusta al desbalance, pero con solo 7 positivos
en test su estimación es altamente inestable: un único error cambia la curva
drásticamente.

La **ROC-AUC** mide la capacidad del modelo para ordenar correctamente los casos por
score, independientemente del umbral de decisión. Es más estable que la PR-AUC con tan
pocos positivos y permite comparar modelos de distinta naturaleza (supervisados vs.
detección de anomalías) en una escala común.

Para el contexto clínico del problema, la **sensibilidad** es la métrica más crítica:
en screening de enfermedades infecciosas, un falso negativo (caso de coronavirus no
detectado) tiene consecuencias epidemiológicas mayores que un falso positivo (beneficiario
sano marcado para revisión). Por tanto, ante modelos con ROC-AUC comparable, se
priorizará aquel con mayor sensibilidad.

---

### 4.4 Resultados

#### 4.4.1 Modelos supervisados

| Modelo          | PR-AUC    | ROC-AUC   | F1      | Sensibilidad | Especificidad | Accuracy |
| --------------- | --------- | --------- | ------- | ------------ | ------------- | -------- |
| RF submuestreo  | 0,00168   | **0,917** | 0,00355 | 0,143        | 0,996         | 0,996    |
| Logística Lasso | 0,0000270 | 0,499     | 0,000   | 0,000        | 0,997         | 0,997    |
| RF pesos        | 0,0000270 | 0,499     | —       | 0,000        | 1,000         | 1,000    |
| XGBoost         | 0,0000270 | 0,500     | —       | 0,000        | 1,000         | 1,000    |

Tres de los cuatro modelos supervisados presentan **sensibilidad = 0**, lo que indica
que no detectan ningún caso positivo en el conjunto de test. Este colapso tiene una
causa estructural: con solo 7 positivos en entrenamiento y pesos de 74.169, la función
de pérdida ponderada queda dominada por los negativos en términos absolutos
(519.185 × 1 ≈ 7 × 74.169), pero la variabilidad estadística de 7 ejemplos positivos
es insuficiente para que el modelo aprenda una frontera de decisión generalizable.
La mayoría de los árboles en bootstrap nunca ven un positivo, y el modelo converge a
predecir siempre negativo.

**RF submuestreo** es el único supervisado que logra sensibilidad mayor a cero (0,143)
y el mejor ROC-AUC entre los supervisados (0,917). Al entrenar con solo ~77
observaciones balanceadas, los árboles sí ven positivos en cada muestra y aprenden
alguna señal discriminativa, aunque detecta apenas 1 de 7 positivos en test.

Los valores `NA` en F1 para RF pesos y XGBoost indican que el denominador de la
precisión es cero, estos modelos nunca predicen positivo, haciendo la métrica
indefinida.

#### 4.4.2 Tabla final incluyendo Isolation Forest

| Modelo               | PR-AUC    | ROC-AUC   | F1       | Sensibilidad | Especificidad | Accuracy | Enfoque     |
| -------------------- | --------- | --------- | -------- | ------------ | ------------- | -------- | ----------- |
| RF submuestreo       | 0,00168   | **0,917** | 0,00355  | 0,143        | **0,996**     | 0,996    | Supervisado |
| **Isolation Forest** | 0,000779  | 0,869     | 0,000792 | **0,714**    | 0,903         | 0,903    | Anomalía    |
| XGBoost              | 0,0000270 | 0,500     | —        | 0,000        | 1,000         | 1,000    | Supervisado |
| Logística Lasso      | 0,0000270 | 0,499     | 0,000    | 0,000        | 0,997         | 0,997    | Supervisado |
| RF pesos             | 0,0000270 | 0,499     | —        | 0,000        | 1,000         | 1,000    | Supervisado |

Los dos modelos con capacidad discriminativa real son **RF submuestreo** e **Isolation
Forest**, con ROC-AUC de 0,917 y 0,869 respectivamente, ambos superan el umbral de
referencia del 85 % indicado en el enunciado.

#### 4.4.3 Selección del modelo final

La elección entre RF submuestreo e Isolation Forest depende del criterio de evaluación
prioritario:

| Criterio             | RF submuestreo | Isolation Forest | Ganador |
| -------------------- | -------------- | ---------------- | ------- |
| ROC-AUC              | **0,917**      | 0,869            | RF sub  |
| Sensibilidad         | 0,143          | **0,714**        | IF      |
| Especificidad        | **0,996**      | 0,903            | RF sub  |
| Positivos detectados | 1 de 7         | **5 de 7**       | IF      |
| Falsos negativos     | 6              | **2**            | IF      |

En el contexto de detección de una enfermedad infecciosa como el coronavirus, el **falso
negativo es el error más costoso**: un caso no detectado continúa generando contagios y
no recibe atención oportuna. Por este motivo, **se selecciona el Isolation Forest como
modelo principal**, al detectar 5 de 7 casos positivos frente al único caso detectado
por RF submuestreo.

El mayor número de falsos positivos del IF (12.608 vs. 555) representa el costo
aceptable de esa sensibilidad: beneficiarios sanos marcados para revisión adicional,
lo cual es preferible en un contexto de screening a dejar casos reales sin detectar.
El RF submuestreo se reporta como mejor modelo supervisado de referencia.

#### 4.4.4 Matrices de confusión

**Isolation Forest (modelo seleccionado)**

|                   | Predicho negativo | Predicho positivo |
| ----------------- | ----------------- | ----------------- |
| **Real negativo** | 117.183 (VN)      | 12.608 (FP)       |
| **Real positivo** | 2 (FN)            | 5 (VP)            |

- Sensibilidad = 5/7 = **0,714** detecta 5 de 7 casos reales de coronavirus.
- Especificidad = 117.183/129.791 = **0,903** clasifica correctamente el 90,3 % de
  los negativos.
- VPP = 5/12.613 = **0,04 %** de cada 100 alertas del modelo, 0,04 son casos reales.
  Bajo pero esperable dado el desbalance extremo.
- VPN = 117.183/117.185 = **99,998 %** prácticamente todos los clasificados como
  negativos son realmente negativos.

**RF submuestreo (mejor supervisado)**

|                   | Predicho negativo | Predicho positivo |
| ----------------- | ----------------- | ----------------- |
| **Real negativo** | 129.236 (VN)      | 555 (FP)          |
| **Real positivo** | 6 (FN)            | 1 (VP)            |

- Sensibilidad = 1/7 = **0,143** detecta solo 1 de 7 casos reales.
- Especificidad = 129.236/129.791 = **0,996**.
- VPP = 1/556 = **0,18 %**.
- VPN = 129.236/129.242 = **99,995 %**.

---

### 4.5 Discusión del desempeño

El Isolation Forest supera el umbral de referencia del 85 % en ROC-AUC (0,869) y en
sensibilidad (0,714), cumpliendo el criterio del enunciado con la métrica más relevante
para el problema clínico.

Sin embargo, los resultados deben interpretarse con cautela estructural: con **7
positivos en test**, cada error individual representa un cambio de ~14 puntos
porcentuales en sensibilidad. Las métricas no son estadísticamente estables una
semilla diferente en la partición podría redistribuir los 7 positivos y producir
resultados distintos. Esto no invalida el análisis, pero sí limita la confianza en los
valores exactos reportados.

La conclusión más robusta no es el número exacto de ROC-AUC, sino la **conclusión
metodológica**: los clasificadores supervisados con pesos extremos (74.169) colapsan
porque 7 ejemplos positivos son insuficientes para aprender una frontera de decisión
generalizable, independientemente del algoritmo usado. El enfoque de detección de
anomalías resuelve este problema estructural al no requerir positivos en entrenamiento,
y es el más apropiado cuando la clase positiva representa menos del 0,01 % de la
población.

### 4.6 Importancia de variables — Isolation Forest

Dado que el Isolation Forest no produce coeficientes ni impureza de Gini directamente
interpretables, la importancia de variables se estimó mediante **permutación**: para
cada variable predictora se permutan aleatoriamente sus valores en el conjunto de test
y se mide la caída en ROC-AUC respecto al score base. Una caída grande indica que esa
variable es relevante para la detección de anomalías; una caída cercana a cero indica
que el modelo no la utiliza de forma significativa.

El ROC-AUC base del Isolation Forest sobre el conjunto de test es **0,869**.

#### 4.6.1 Resultados

A partir del perfil de caídas se identifican tres grupos:

**Variables importantes** (caída > 0,019):

- `prop_internacion` (caída ≈ 0,044): la proporción de utilizaciones asociadas a
  internación es la variable más determinante. Tiene sentido clínico directo: el
  coronavirus en sus formas severas requiere hospitalización, por lo que los
  beneficiarios COVID presentan una proporción de internaciones inusualmente alta
  respecto al resto de la población.

- `especialidades` (caída ≈ 0,036): el número de especialidades médicas distintas
  consultadas por el beneficiario. Los casos de coronavirus severo suelen requerir
  atención multidisciplinaria (infectología, neumología, medicina intensiva), lo que
  genera un perfil de especialidades más amplio que el promedio.

**Variables moderadas** (caída entre 0,007 y 0,019):

- `n_utilizaciones` (≈ 0,019), `costo_total` (≈ 0,010), `prop_urgencias` (≈ 0,010),
  `n_estados` (≈ 0,010): contribuyen a la detección pero no son determinantes de forma
  individual. El volumen de utilizaciones y el costo total elevados son consistentes
  con una enfermedad que genera múltiples consultas y procedimientos. La proporción de
  urgencias refleja la presentación aguda típica del coronavirus.

**Variables marginales** (caída < 0,007):

- `edad`, `n_procedimientos`, `prop_examenes`, `costo_promedio`, `ventana_dias`,
  `n_internaciones`, `n_uci`, `densidad_uso`, `SEXO_BENEFICIARIO`: su permutación
  no degrada significativamente el modelo. Esto no implica que estas variables sean
  clínicamente irrelevantes para la enfermedad (la edad es un factor de riesgo
  documentado para COVID-19 ) sino que en este dataset con 14 positivos, la señal
  estadística de estas variables no es suficiente para que el Isolation Forest las
  incorpore de forma robusta.

## 5. Conclusiones, limitaciones y trabajos futuros

### 5.1 ¿Qué tan bien se logró identificar a los beneficiarios con coronavirus?

La identificación de beneficiarios con infección por coronavirus resultó parcialmente
exitosa, con resultados que deben interpretarse en el contexto del desbalance extremo
de la variable objetivo: **14 positivos frente a 648.976 negativos (0,002 %)**.

El modelo seleccionado, **Isolation Forest**, detectó **5 de los 7 positivos presentes
en el conjunto de test** (sensibilidad = 0,714), clasificando correctamente el 90,3 %
de los negativos (especificidad = 0,903) y obteniendo un ROC-AUC de **0,869**,
superando el umbral de referencia del 85 % establecido en el enunciado.

Sin embargo, esta capacidad de identificación tiene limitaciones estructurales
importantes. Con solo 7 positivos en test, cada error individual representa un cambio
de ~14 puntos porcentuales en sensibilidad, lo que hace que las métricas sean
estadísticamente inestables. Los **2 falsos negativos** corresponden probablemente a
casos leves cuyo perfil de utilización no se diferencia del de la población general,
y los **12.608 falsos positivos** reflejan el costo inevitable de operar con alta
sensibilidad ante un desbalance tan extremo.

En términos prácticos, el modelo es útil como identificación de una lista reducida de
beneficiarios candidatos a revisión pero no como clasificador definitivo.

---

### 5.2 ¿Qué variables parecen estar más relacionadas con la enfermedad?

El análisis de importancia por permutación sobre el Isolation Forest identificó dos
variables con capacidad discriminativa sustancial:

**`prop_internacion`** es la variable más importante (caída en ROC-AUC ≈ 0,044).
La proporción de utilizaciones asociadas a internación captura la severidad clínica
característica del coronavirus: a diferencia de la mayoría de condiciones que generan
consultas ambulatorias, el COVID-19 en sus formas moderadas y severas requiere
hospitalización. Que esta variable emerja como la más discriminativa es una validación
indirecta de su relevancia clínica.

**`especialidades`** es la segunda variable en importancia (caída ≈ 0,036). El número
de especialidades médicas distintas consultadas por el beneficiario refleja la atención
multidisciplinaria típica del coronavirus severo: infectología, neumología, medicina
intensiva y otras especialidades suelen intervenir en el mismo episodio de enfermedad,
generando un perfil de consultas más amplio que el promedio de la población.

Con importancia moderada aparecen `n_utilizaciones`, `costo_total`, `prop_urgencias`
y `n_estados`, que en conjunto reflejan un patrón de uso intensivo y geográficamente
distribuido del sistema de salud, consistente con la presentación aguda del coronavirus.

Variables como `edad`, `n_uci` y `SEXO_BENEFICIARIO` resultaron marginales en el
modelo, lo que no contradice su relevancia clínica documentada en la literatura, sino
que refleja la insuficiencia estadística de 14 casos para detectar esas señales de
forma robusta.

---

### 5.3 ¿Qué tan confiable es el modelo construido?

La confiabilidad del modelo debe evaluarse en dos dimensiones:

**Desde la perspectiva del desempeño:** el Isolation Forest alcanza ROC-AUC = 0,869
y sensibilidad = 0,714, lo que representa un desempeño sólido en términos de ranking
y detección. Sin embargo, con solo 7 positivos en test, los intervalos de confianza
implícitos en estas métricas son muy amplios, una partición diferente podría producir
resultados notablemente distintos.

**Desde la perspectiva metodológica:** el modelo es conceptualmente el más apropiado
para este problema. Al entrenarse exclusivamente sobre los 519.185 negativos, evita
el principal obstáculo estadístico (la escasez de positivos) y no depende de supuestos
sobre la distribución de la clase minoritaria. Su capacidad de generalización es más
robusta que la de cualquier clasificador supervisado con 7 ejemplos positivos.

Los modelos supervisados entrenados (Regresión Logística Lasso, Random Forest con
pesos, XGBoost) demostraron ser no confiables en este contexto: el peso extremo de
74.169 aplicado al positivo generó colapso de la especificidad o la sensibilidad,
confirmando que 7 ejemplos son insuficientes para que el gradiente de aprendizaje
aprenda una frontera de decisión generalizable. El Random Forest con submuestreo,
aunque alcanzó ROC-AUC = 0,917, detectó únicamente 1 de 7 positivos, lo que lo hace
poco útil en un contexto clínico donde el falso negativo es el error más costoso.

---

### 5.4 ¿Qué limitaciones tiene la base de datos?

**Desbalance extremo irreducible:** con 14 positivos en 648.990 beneficiarios. La
prevalencia de 0,002 % sugiere que la base no cubre el período de mayor incidencia
de COVID-19 o que los códigos CID utilizados capturan solo una fracción de los casos reales.

**Alta proporción de especialidad no clasificada:** el 84,7 % de los registros presenta
especialidad médica no identificada (`no_clasificado`), lo que limita significativamente
el potencial de variables derivadas de `DESC_ESPECIALIDADE` como predictores. Esta
limitación reduce la capacidad del modelo para capturar señales clínicas importantes.

**Inconsistencias demográficas:** se identificaron beneficiarios con múltiples valores
de sexo o fecha de nacimiento en diferentes transacciones. Aunque se aplicó una
estrategia de imputación por moda con descarte de casos ambiguos, la existencia de
estas inconsistencias evidencia problemas de calidad en el registro administrativo que
podrían afectar la integridad de otras variables no verificadas.

**28 % de registros sin estado geográfico identificado:** la variable
`UF_CNES_PREST_HOSPITALAR` tiene una proporción muy alta de valores no clasificados,
lo que limita cualquier análisis geográfico y reduce el poder predictivo de variables
derivadas del estado de atención.

---

### 5.5 ¿Qué limitaciones tiene la estrategia de modelamiento utilizada?

**Validación estadísticamente inestable:** con 7 positivos en test, las métricas
reportadas tienen alta varianza. No es posible construir intervalos de confianza
confiables ni garantizar que los resultados se repliquen con una muestra diferente.

**Isolation Forest no produce probabilidades calibradas:** el score de anomalía no
es directamente interpretable como probabilidad de pertenecer a la clase positiva.
Esto limita su uso en aplicaciones que requieran umbrales de decisión con
interpretación probabilística (por ejemplo, para análisis de costo-beneficio de
intervenciones clínicas).

**Imposibilidad de usar SMOTE:** con 7 positivos en entrenamiento, la generación de
ejemplos sintéticos mediante SMOTE produce casi-duplicados sin variabilidad real,
lo que descartó esta técnica estándar de balanceo. El espacio de features de los
positivos es demasiado pequeño para sintetizar nuevos ejemplos informativos.

**Features construidas a nivel beneficiario pierden información temporal:** la
agregación de 9 millones de registros a 648.990 observaciones colapsa la dimensión
temporal del historial clínico. Variables como la evolución del costo a lo largo del
tiempo, la secuencia de especialidades consultadas o el intervalo entre utilizaciones
podrían ser señales discriminativas que se pierden en la agregación.

**Imposibilidad de interpretar el Isolation Forest con SHAP o PDP:** las herramientas
estándar de interpretabilidad para modelos de clasificación no están directamente
disponibles para modelos de detección de anomalías. La importancia por permutación
empleada es una aproximación válida pero menos precisa que SHAP en términos de
cuantificación de efectos individuales.

---

### 5.6 ¿Qué mejoras podrían realizarse en un trabajo futuro?

**Modelamiento longitudinal:** en lugar de agregar el historial a nivel beneficiario,
construir features que capturen la evolución temporal de las utilizaciones: cambios
en el costo entre períodos, secuencias de especialidades, aceleración en la frecuencia
de consultas. Modelos como LSTM o features de series temporales podrían capturar
señales que la agregación estática pierde.

**Enriquecer la variable `DESC_ESPECIALIDADE`:** desarrollar una tabla de equivalencias
entre títulos profesionales y especialidades médicas para recuperar información del
84,7 % de registros actualmente no clasificados. Esto mejoraría significativamente la
calidad de las variables derivadas de especialidad.

**Validación externa:** contrastar los beneficiarios identificados por el modelo contra
fuentes externas de notificación de COVID-19 (registros epidemiológicos, resultados de
laboratorio) para estimar la tasa real de falsos positivos y negativos más allá de lo
que permite la propia base de datos.

**Explorar modelos de One-Class SVM y autoencoders** como alternativas al Isolation
Forest dentro del paradigma de detección de anomalías, para comparar si arquitecturas
más complejas mejoran la discriminación manteniendo la ventaja de no requerir positivos
en entrenamiento.
