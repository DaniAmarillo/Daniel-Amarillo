######################################
# librerias
#####################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(stringr)
library(stringi)
source("./utils/functions.R")
library(lubridate)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(themis)
library(ranger)
library(xgboost)
library(isotree)
library(vip)
library(patchwork)

set.seed(2026)

#####################################
# lectura de datos
####################################
datos <- read.csv("C:/Users/sergi/Downloads/db_2026.csv")

####################################
# preparacion de los datos
####################################

# manejo variable CID
datos_agrupados <- datos %>% group_by(CID) %>% count()
# encontramos de 9345278 datos, 7667147 son marcados como N/A, 613585 marcados 
# como vacios y 2101 como -, unificamos todos los nulos

datos <- datos %>% mutate(
                          CID = case_when(
                                  is.na(str_trim(toupper(CID))) ~ "no_clasificado",
                                  str_trim(toupper(CID)) == "N/A" ~ "no_clasificado",
                                  str_trim(toupper(CID)) == "-" ~ "no_clasificado",
                                  str_trim(toupper(CID)) == "" ~ "no_clasificado",
                                  str_trim(toupper(CID)) == "B342" ~ "B34.2",
                                  str_trim(toupper(CID)) == "U071" ~ "U07.1",
                                  TRUE ~ str_trim(toupper(CID)),
                                  )
                        )

# exploracion UTI
datos_agrupados <- datos %>% group_by(UTI) %>% count()

# exploracion internado
datos_agrupados <- datos %>% group_by(INTERNADO) %>% count()

# exploracion PORTE_ANESTESICO
datos_agrupados <- datos %>% group_by(PORTE_ANESTESICO) %>% count()
# implementar todos los niveles de anestecia nulos a 0 ya que no se aplicaron
datos <- datos %>% mutate(
                    PORTE_ANESTESICO = case_when(
                      is.na(PORTE_ANESTESICO) ~ 0,
                      TRUE ~ PORTE_ANESTESICO
                    )
                  )

# exploracion DT_UTILIZACAO
datos_agrupados <- datos %>% group_by(DT_UTILIZACAO) %>% count()
# filtramos los registros  con fecha de utilizacion mayor a la de hoy, 
# ya que no se puede confiar en la integridad de dichos registros
datos <- datos %>%
  filter(as.Date(DT_UTILIZACAO) <= Sys.Date())

# exploracion DESC_ESPECIALIDADE
datos_agrupados <- datos %>% group_by(DESC_ESPECIALIDADE) %>% count()

# quitamos acentos, caracteres no ascii y unificamos nulos para guardar 
# integridad de registros y casos especificos
datos <- datos %>%
  mutate(
    DESC_ESPECIALIDADE = stri_trans_general(str_trim(toupper(DESC_ESPECIALIDADE)), "Latin-ASCII"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "-", "no_clasificado"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "NAO INFORMADA", "no_clasificado"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "NAO INFORMADO", "no_clasificado"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "/", " "),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE,"   ", " "),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, " E ", " "),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "CARDIO VASCULAR", "CARDIOVASCULAR"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "ANESTESIOLOGISTA", "ANESTESIOLOGIA"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "CIRURGIA DA CABECA PESCOCO", "CIRURGIA DE CABECA PESCOCO"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "CIRURGIA DA MAO", "CIRURGIA DE MAO"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "TORAXICA", "TORACICA"),
    DESC_ESPECIALIDADE = str_replace_all(DESC_ESPECIALIDADE, "FONOAUDIOLOGO", "FONOAUDIOLOGIA"),
    DESC_ESPECIALIDADE = if_else(DESC_ESPECIALIDADE == "", "no_clasificado", DESC_ESPECIALIDADE)
  )
# hay mas valores que en lugar de escribir la especializacion, escribieron el
# titulo profesional, se requiere una base con claridad que indique que 
# especializacion corresponde a cada titulo

# exploracion TIPO_UNIDADE_PREST_HOSPITALAR
datos_agrupados <- datos %>% group_by(TIPO_UNIDADE_PREST_HOSPITALAR) %>% count()

# unificamos los nulos con la demas notacion 
datos <- datos %>%
  mutate(
    TIPO_UNIDADE_PREST_HOSPITALAR = case_when(
      TIPO_UNIDADE_PREST_HOSPITALAR == "-" ~ "no_clasificado",
      TRUE ~ str_trim(toupper(TIPO_UNIDADE_PREST_HOSPITALAR))
    )
  )

# exploracion UF_CNES_PREST_HOSPITALAR
datos_agrupados <- datos %>% group_by(UF_CNES_PREST_HOSPITALAR) %>% count()

# unificamos los nulos con la demas notacion 
datos <- datos %>%
  mutate(
    UF_CNES_PREST_HOSPITALAR = case_when(
      UF_CNES_PREST_HOSPITALAR == "-" ~ "no_clasificado",
      TRUE ~ str_trim(toupper(UF_CNES_PREST_HOSPITALAR))
    )
  )

# exploracion DT_NASCIMENTO_BENEFICIARIO
datos_agrupados <- datos %>% group_by(DT_NASCIMENTO_BENEFICIARIO) %>% count()

# filtramos los nacimientos que no tienen sentido (los mayores al dia de hoy o nulos)
# y extraemos los id del usuario, buscamos y si ese usuario tiene una fecha 
# valida en ese registro, lo rescribimos, caso contrario, omitimos los registros
# con fechas no validas, si es nula y no se logra imputar se elimina el registro

datos <- datos %>%
  mutate(DT_NASCIMENTO_BENEFICIARIO = as.Date(DT_NASCIMENTO_BENEFICIARIO))

fechas_validas <- datos %>%
  filter(!is.na(DT_NASCIMENTO_BENEFICIARIO), DT_NASCIMENTO_BENEFICIARIO <= Sys.Date()) %>%
  distinct(CHAVE_FUNCIONAL, DT_NASCIMENTO_BENEFICIARIO) %>%
  group_by(CHAVE_FUNCIONAL) %>%
  slice(1) %>%
  ungroup() %>%
  rename(DT_NASCIMENTO_VALIDA = DT_NASCIMENTO_BENEFICIARIO)

datos <- datos %>%
  left_join(fechas_validas, by = "CHAVE_FUNCIONAL") %>%
  mutate(
    fecha_invalida = is.na(DT_NASCIMENTO_BENEFICIARIO) | DT_NASCIMENTO_BENEFICIARIO > Sys.Date(),
    DT_NASCIMENTO_BENEFICIARIO = if_else(
      fecha_invalida & !is.na(DT_NASCIMENTO_VALIDA),
      DT_NASCIMENTO_VALIDA,
      DT_NASCIMENTO_BENEFICIARIO
    )
  )

datos <- datos %>%
  filter(DT_NASCIMENTO_BENEFICIARIO <= Sys.Date()) %>%
  select(-DT_NASCIMENTO_VALIDA, -fecha_invalida)

# explorar TIPO_BENEFICIARIO
datos_agrupados <- datos %>% group_by(TIPO_BENEFICIARIO) %>% count()

# unificar la categoria no informada y otros
datos <- datos %>%
  mutate(TIPO_BENEFICIARIO = case_when(
    TIPO_BENEFICIARIO == "Não Informado" ~ "OUTROS",
    TRUE ~ str_trim(TIPO_BENEFICIARIO)
  ))

# explorar SEXO_BENEFICIARIO 
datos_agrupados <- datos %>% group_by(SEXO_BENEFICIARIO) %>% count()

# estandarizar los nombres y a los no informados clasificarlos como OTROS
datos <- datos %>%
  mutate(SEXO_BENEFICIARIO = case_when(
    SEXO_BENEFICIARIO == "Não Informado" ~ "OUTROS",
    SEXO_BENEFICIARIO == "MASCULINO" ~ "M",
    TRUE ~ str_trim(SEXO_BENEFICIARIO)
  ))

# explorar CETIPO
datos_agrupados <- datos %>% group_by(CETIPO) %>% count()

# modificar los valores todos en mayuscula y con ascii
datos <- datos %>% mutate(
  CETIPO = stri_trans_general(str_trim(toupper(CETIPO)), "Latin-ASCII"),
)

# explorar CD_PROCEDIMENTO
datos_agrupados <- datos %>% group_by(CD_PROCEDIMENTO) %>% count()

# explorar DESCRICAO_PROCEDIMENTO
datos_agrupados <- datos %>% group_by(DESCRICAO_PROCEDIMENTO) %>% count()

# estandarizamos todos los valores en mayusculas y caracteres ascii
datos <- datos %>% mutate(
  DESCRICAO_PROCEDIMENTO = stri_trans_general(str_trim(toupper(DESCRICAO_PROCEDIMENTO)), "Latin-ASCII"),
)

# explorar VALOR_UTILIZACAO
datos_agrupados <- datos %>% group_by(VALOR_UTILIZACAO) %>% count()

# eliminamos precios negativos dado que no nos interesan sobrecostos o devoluciones
# para este modelo
datos <- datos %>% filter(VALOR_UTILIZACAO > 0)

# explorar CHAVE_FUNCIONAL
datos_agrupados <- datos %>% group_by(CHAVE_FUNCIONAL) %>% count()

# validacion fechas de nacimiento posterior a la fecha de atencion por poca confiabilidad de los registros
datos <- datos %>% filter(!(DT_NASCIMENTO_BENEFICIARIO > DT_UTILIZACAO))

# validamos que la informacion sea estable para cada usuario
modas_por_beneficiario <- datos %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    moda_sexo  = moda_unica(SEXO_BENEFICIARIO),
    moda_fecha = moda_unica(DT_NASCIMENTO_BENEFICIARIO),
    moda_tipo = moda_unica(TIPO_BENEFICIARIO),
    .groups = "drop"
  )

chaves_descartar <- modas_por_beneficiario %>%
  filter(is.na(moda_sexo) | is.na(moda_fecha) | is.na(moda_tipo)) %>%
  pull(CHAVE_FUNCIONAL)

datos <- datos %>%
  filter(!CHAVE_FUNCIONAL %in% chaves_descartar) %>%
  left_join(modas_por_beneficiario, by = "CHAVE_FUNCIONAL") %>%
  mutate(
    SEXO_BENEFICIARIO          = moda_sexo,
    DT_NASCIMENTO_BENEFICIARIO = moda_fecha,
    TIPO_BENEFICIARIO          = moda_tipo,
  ) %>%
  select(-moda_sexo, -moda_fecha, -moda_tipo)

# verificamos empalmes entre codigos de tratamiento y su descripcion
pares_unicos <- distinct(datos, CD_PROCEDIMENTO, DESCRICAO_PROCEDIMENTO)

list(
  desc_multi_cod = pares_unicos %>% count(DESCRICAO_PROCEDIMENTO) %>% filter(n > 1),
  cod_multi_desc = pares_unicos %>% count(CD_PROCEDIMENTO)        %>% filter(n > 1)
) -> prueba

# como hay varias descripciones que tienen varios codigos pero no hay codigos que
# tengan varias descripciones, estandarisamos al primer codigo nomas para las
# descripciones con mas de un codigo

par_procedimiento <- datos %>%
                      distinct(CD_PROCEDIMENTO, DESCRICAO_PROCEDIMENTO) %>%
                      group_by(DESCRICAO_PROCEDIMENTO) %>%
                      slice(1) %>%
                      ungroup() %>%
                      rename(CD_PROCEDIMENTO_VALIDA = CD_PROCEDIMENTO)
datos <- datos %>%
          left_join(par_procedimiento, by="DESCRICAO_PROCEDIMENTO") %>%
          mutate(CD_PROCEDIMENTO = CD_PROCEDIMENTO_VALIDA) %>%
          select(-CD_PROCEDIMENTO_VALIDA)
          
# empezamos con 9345278 y terminamos con 8510292, lo que corresponde a 
# 91% de los datos iniciales

# creacion variable objetivo
usuarios_covid <- datos %>%
                  filter(CID == "U07.1" | CID == "B34.2") %>%
                  distinct(CHAVE_FUNCIONAL) %>%
                  mutate(OBJETIVO = 1)
datos <- datos %>%
          left_join(usuarios_covid, by="CHAVE_FUNCIONAL") %>%
          mutate(
            OBJETIVO = case_when(
              is.na(OBJETIVO) ~ 0,
              TRUE ~ OBJETIVO
            )
          )
# eliminamos duplicados
datos <- distinct(datos)


#################################
# descripcion de datos
#################################

# resumen nivel beneficiario
beneficiarios <- datos %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    SEXO_BENEFICIARIO          = first(SEXO_BENEFICIARIO),
    DT_NASCIMENTO_BENEFICIARIO = first(DT_NASCIMENTO_BENEFICIARIO),
    OBJETIVO                   = max(OBJETIVO),
    TIPO_BENEFICIARIO          = first(TIPO_BENEFICIARIO),
    n_utilizaciones            = n_distinct(DT_UTILIZACAO),
    n_procedimientos           = n_distinct(CD_PROCEDIMENTO),
    n_internaciones            = sum(INTERNADO, na.rm = TRUE),
    n_uci                      = sum(UTI,       na.rm = TRUE),
    costo_total                = sum(VALOR_UTILIZACAO,  na.rm = TRUE),
    costo_promedio             = mean(VALOR_UTILIZACAO, na.rm = TRUE),
    prop_consultas             = mean(CETIPO == "CONSULTA", na.rm = TRUE),
    prop_examenes              = mean(CETIPO == "EXAME", na.rm = TRUE),
    prop_urgencias             = mean(CETIPO == "PRONTO SOCORRO", na.rm = TRUE),
    prop_internacion           = mean(CETIPO == "INTERNACAO", na.rm = TRUE),
    prop_terapia               = mean(CETIPO == "TERAPIA", na.rm = TRUE),
    max_anestesia              = max(PORTE_ANESTESICO, na.rm = TRUE),
    tuvo_anestesia             = as.integer(any(PORTE_ANESTESICO > 0, na.rm = TRUE)),
    ventana_dias               = as.integer(
                                 max(as.Date(DT_UTILIZACAO)) - 
                                 min(as.Date(DT_UTILIZACAO))
                                 ),
    densidad_uso               = n_distinct(DT_UTILIZACAO) /
                                 pmax(as.integer(
                                   max(as.Date(DT_UTILIZACAO)) -
                                   min(as.Date(DT_UTILIZACAO))
                                 ), 1),
    n_estados                  = n_distinct(UF_CNES_PREST_HOSPITALAR),
    especialidades             = n_distinct(DESC_ESPECIALIDADE),
    .groups = "drop"
  ) %>%
  mutate(
    edad = as.integer(interval(DT_NASCIMENTO_BENEFICIARIO, Sys.Date()) / years(1))
  )

# resumen global
n_beneficiarios  <- n_distinct(datos$CHAVE_FUNCIONAL)
n_utilizaciones  <- n_distinct(paste(datos$CHAVE_FUNCIONAL, datos$DT_UTILIZACAO))
n_procedimientos <- n_distinct(datos$CD_PROCEDIMENTO)

# analisis variable objetivo
dist_objetivo <- beneficiarios %>%
  count(OBJETIVO) %>%
  mutate(
    Etiqueta   = if_else(OBJETIVO == 1, "COVID-19", "Sin COVID-19"),
    Porcentaje = round(100 * n / sum(n), 2)
  )

print(dist_objetivo)

ggplot(dist_objetivo, aes(x = Etiqueta, y = n, fill = Etiqueta)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución de la variable objetivo",
       x = NULL, y = "N° beneficiarios") +
  theme_minimal() +
  theme(legend.position = "none")

# analisis objetivos beneficiarios por sexo
dist_sexo <- beneficiarios %>%
  count(SEXO_BENEFICIARIO) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 1))

print(dist_sexo)

ggplot(dist_sexo, aes(x = SEXO_BENEFICIARIO, y = Porcentaje)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución de la variable sexo",
       x = NULL, y = "% beneficiarios") +
  theme_minimal()

# distribucion por brackets de edad
beneficiarios <- beneficiarios %>%
  mutate(grupo_edad = cut(edad,
                          breaks = c(0, 18, 30, 45, 60, 75, Inf),
                          labels = c("0-17", "18-29", "30-44", "45-59", "60-74", "75+"),
                          right  = FALSE))
dist_edad <- beneficiarios %>%
  count(grupo_edad) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(dist_edad)

ggplot(dist_edad, aes(x = grupo_edad, y = pct)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución por grupo de edad",
       x = "Grupo de edad", y = "% beneficiarios", fill = NULL) +
  theme_minimal()

# distribucion por tipo de beneficiario
dist_tipo <- beneficiarios %>%
  count(TIPO_BENEFICIARIO) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

print(dist_tipo)

ggplot(dist_tipo, aes(x = TIPO_BENEFICIARIO, y = n)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(pct, "")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución por tipo de beneficiario",
       x = "Tipo de beneficiario", y = "N beneficiarios", fill = NULL) +
  theme_minimal()

# distribucion por estado (top 5)
dist_estado <- datos %>%
  count(UF_CNES_PREST_HOSPITALAR) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(pct)) %>%
  top_n(5)

print(dist_estado)

ggplot(dist_estado, aes(x = UF_CNES_PREST_HOSPITALAR, y = n)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(pct, "")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución por estado (top 5)",
       x = "estado del hospital", y = "N registros", fill = NULL) +
  theme_minimal()

# distribucion registros por especialidad
dist_especialidad <- datos %>%
  count(DESC_ESPECIALIDADE) %>%
  slice_max(order_by = n, n = 5) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(dist_especialidad)

ggplot(dist_especialidad, aes(x = DESC_ESPECIALIDADE, y = n)) +
  geom_col(width = 0.5, fill = "darkseagreen3") +
  geom_text(aes(label = paste0(pct, "")), vjust = -0.5, size = 3.5) +
  labs(title = "Top 5 especialidades por volumen de registros",
       x = NULL, y = "N° registros", fill = NULL) +
  theme_minimal()

# analisis valores extremos en VALOR_UTILIZACAO
stats_valor <- datos %>%
  summarise(
    min    = min(VALOR_UTILIZACAO,                  na.rm = TRUE),
    p25    = quantile(VALOR_UTILIZACAO, 0.25,       na.rm = TRUE),
    mediana= median(VALOR_UTILIZACAO,               na.rm = TRUE),
    media  = mean(VALOR_UTILIZACAO,                 na.rm = TRUE),
    p75    = quantile(VALOR_UTILIZACAO, 0.75,       na.rm = TRUE),
    p95    = quantile(VALOR_UTILIZACAO, 0.95,       na.rm = TRUE),
    p99    = quantile(VALOR_UTILIZACAO, 0.99,       na.rm = TRUE),
    max    = max(VALOR_UTILIZACAO,                  na.rm = TRUE)
  )

print(stats_valor)
# Límite IQR para identificar outliers
iqr_val <- IQR(datos$VALOR_UTILIZACAO, na.rm = TRUE)
lim_sup  <- quantile(datos$VALOR_UTILIZACAO, 0.75, na.rm = TRUE) + 1.5 * iqr_val

cat(sprintf("\nRegistros por encima del límite IQR (%.2f): %d (%.2f%%)\n",
            lim_sup,
            sum(datos$VALOR_UTILIZACAO > lim_sup, na.rm = TRUE),
            100 * mean(datos$VALOR_UTILIZACAO > lim_sup, na.rm = TRUE)))

# ajustes finales sobre la tabla de datos
beneficiarios <- beneficiarios %>%
          mutate(TIPO_BENEFICIARIO = case_when(
            TIPO_BENEFICIARIO != "TITULAR" & TIPO_BENEFICIARIO != "DEPENDENTE" ~ "OUTRO",
            TRUE ~ TIPO_BENEFICIARIO
          )) %>%
          select(-CHAVE_FUNCIONAL, -DT_NASCIMENTO_BENEFICIARIO, -grupo_edad) %>%
          mutate(
            OBJETIVO          = factor(OBJETIVO, levels = c(0, 1),
                                       labels = c("negativo", "positivo")),
            SEXO_BENEFICIARIO = factor(SEXO_BENEFICIARIO),
            TIPO_BENEFICIARIO = factor(TIPO_BENEFICIARIO),
          )
cat(sprintf("Dataset modelo: %d filas · %d columnas\n",
            nrow(beneficiarios), ncol(beneficiarios)))
cat(sprintf("Positivos: %d · Negativos: %d\n",
            sum(beneficiarios$OBJETIVO == "positivo"),
            sum(beneficiarios$OBJETIVO == "negativo")))
###########################################
# modelamiento predictivo
###########################################

# split 80/20 estratificados para asegurar presencia de objetivos positivos
split <- initial_split(beneficiarios, prop = 0.80, strata = OBJETIVO)
train <- training(split)
test  <- testing(split)

cat(sprintf("\nTrain — positivos: %d · negativos: %d\n",
            sum(train$OBJETIVO == "positivo"),
            sum(train$OBJETIVO == "negativo")))
cat(sprintf("Test  — positivos: %d · negativos: %d\n",
            sum(test$OBJETIVO == "positivo"),
            sum(test$OBJETIVO == "negativo")))

# validacion cruzada
cv_folds <- vfold_cv(train, v = 5, repeats = 1, strata = OBJETIVO)

# lista de metricas a emplear
metricas <- metric_set(pr_auc, roc_auc, f_meas, sens, yardstick::spec, accuracy)

# pesos para compensar el desbalance de los datos
n_neg         <- sum(train$OBJETIVO == "negativo")
n_pos         <- sum(train$OBJETIVO == "positivo")
peso_positivo <- n_neg / n_pos

cat(sprintf("\nPeso asignado al positivo: %.0f\n", peso_positivo))

# modelos base para los modelos tanto completos como con submuestreos
receta_base <- recipe(OBJETIVO ~ ., data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())
receta_sub <- receta_base %>%
  step_downsample(OBJETIVO, under_ratio = 10, seed = 2026)

# especificaciones modelos predictivos
# regresion logistica
log_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# random Forest con pesos
rf_spec <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger",
             class.weights = c(negativo = 1, positivo = peso_positivo),
             importance    = "impurity",
             num.threads   = parallel::detectCores() - 1) %>%
  set_mode("classification")

# XGBoost
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune(),
  min_n          = tune()
) %>%
  set_engine("xgboost",
             scale_pos_weight = peso_positivo,
             eval_metric      = "aucpr",
             nthread          = parallel::detectCores() - 1) %>%
  set_mode("classification")

# random forest con submuestreo
rf_sub_spec <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger",
             importance  = "impurity",
             num.threads = parallel::detectCores() - 1) %>%
  set_mode("classification")

# pipelines
wf_log     <- workflow() %>% add_recipe(receta_base) %>% add_model(log_spec)
wf_rf      <- workflow() %>% add_recipe(receta_base) %>% add_model(rf_spec)
wf_xgb     <- workflow() %>% add_recipe(receta_base) %>% add_model(xgb_spec)
wf_rf_sub  <- workflow() %>% add_recipe(receta_sub)  %>% add_model(rf_sub_spec)

# intervalos para los hiperparametros
grid_log <- tibble(penalty = 10^seq(-4, 0, length.out = 8))

grid_rf <- grid_random(
  mtry(range  = c(2, 5)),
  min_n(range = c(2, 10)),
  size = 6
)

grid_xgb <- grid_random(
  trees(range          = c(100, 500)),
  tree_depth(range     = c(2, 4)),
  learn_rate(range     = c(-3, -1)),
  loss_reduction(range = c(-4, 0)),
  min_n(range          = c(2, 10)),
  size = 8
)
# tuning de hiperparametros
cat("\n Tuning regresión logística Lasso...\n")
tune_log <- wf_log %>%
  tune_grid(resamples = cv_folds, grid = grid_log,
            metrics   = metricas,
            control   = control_grid(save_pred = TRUE, 
                                     event_level = "second", 
                                     verbose = FALSE))

cat(" Tuning Random Forest (pesos por clase)...\n")
tune_rf <- wf_rf %>%
  tune_grid(resamples = cv_folds, grid = grid_rf,
            metrics   = metricas,
            control   = control_grid(save_pred = TRUE, 
                                     event_level = "second", 
                                     verbose = FALSE))

cat(" Tuning XGBoost...\n")
tune_xgb <- wf_xgb %>%
  tune_grid(resamples = cv_folds, grid = grid_xgb,
            metrics   = metricas,
            control   = control_grid(save_pred = TRUE, 
                                     event_level = "second", 
                                     verbose = FALSE))

cat(" Tuning Random Forest (submuestreo)...\n")
tune_rf_sub <- wf_rf_sub %>%
  tune_grid(resamples = cv_folds, grid = grid_rf,
            metrics   = metricas,
            control   = control_grid(save_pred = TRUE, 
                                     event_level = "second", 
                                     verbose = FALSE))

# seleccion hiperparametros
best_log    <- select_best(tune_log,    metric = "pr_auc")
best_rf     <- select_best(tune_rf,     metric = "pr_auc")
best_xgb    <- select_best(tune_xgb,    metric = "pr_auc")
best_rf_sub <- select_best(tune_rf_sub, metric = "pr_auc")

# pipeline completo y ajuste
wf_final_log    <- finalize_workflow(wf_log,    best_log)    %>% fit(train)
wf_final_rf     <- finalize_workflow(wf_rf,     best_rf)     %>% fit(train)
wf_final_xgb    <- finalize_workflow(wf_xgb,    best_xgb)    %>% fit(train)
wf_final_rf_sub <- finalize_workflow(wf_rf_sub, best_rf_sub) %>% fit(train)

# predicciones
preds_log    <- predecir(wf_final_log,    "Logística Lasso")
preds_rf     <- predecir(wf_final_rf,     "RF pesos")
preds_xgb    <- predecir(wf_final_xgb,   "XGBoost")
preds_rf_sub <- predecir(wf_final_rf_sub, "RF submuestreo")

# calculo de metricas

tabla_comparativa <- bind_rows(
  calcular_metricas(preds_log,    "Logística Lasso"),
  calcular_metricas(preds_rf,     "RF pesos"),
  calcular_metricas(preds_xgb,   "XGBoost"),
  calcular_metricas(preds_rf_sub, "RF submuestreo")
) %>%
  select(modelo, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(desc(pr_auc))

cat("\n=== Comparativa supervisados (test) ===\n")
print(tabla_comparativa, n = Inf)


# uso isolation forest para deteccion de anomalias
prep_iso <- recipe(OBJETIVO ~ ., data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep()

train_iso_neg <- bake(prep_iso,
                      new_data = train %>% filter(OBJETIVO == "negativo")) %>%
  select(-OBJETIVO)

test_iso_baked <- bake(prep_iso, new_data = test) %>%
  select(-OBJETIVO)

# entrenamiento modelo
iso_model <- isolation.forest(
  train_iso_neg,
  ntrees      = 200,
  sample_size = 256,
  nthreads    = parallel::detectCores() - 1
)

# prediccion
iso_scores <- predict(iso_model, test_iso_baked)

preds_iso <- tibble(
  OBJETIVO       = test$OBJETIVO,
  .pred_positivo = iso_scores,
  .pred_negativo = 1 - iso_scores,  
  .pred_class    = factor(
    if_else(iso_scores > 0.5, "positivo", "negativo"),
    levels = c("negativo", "positivo")
  ),
  modelo = "Isolation Forest"
)


# calculo metricas isolation tree
metricas_iso <- bind_rows(
  preds_iso %>% pr_auc(truth  = OBJETIVO, .pred_positivo,
                       event_level = "second"),
  preds_iso %>% roc_auc(truth = OBJETIVO, .pred_positivo,
                        event_level = "second"),
  preds_iso %>% f_meas(truth  = OBJETIVO, estimate = .pred_class,
                       event_level = "second"),
  preds_iso %>% sens(truth    = OBJETIVO, estimate = .pred_class,
                     event_level = "second"),
  preds_iso %>% yardstick::spec(truth = OBJETIVO, estimate = .pred_class,
                                event_level = "second"),
  preds_iso %>% accuracy(truth = OBJETIVO, estimate = .pred_class)
) %>% mutate(modelo = "Isolation Forest")

# unificacion metricas de todos los modelos
tabla_final <- bind_rows(
  tabla_comparativa %>% mutate(tipo = "Supervisado"),
  metricas_iso %>%
    select(modelo, .metric, .estimate) %>%
    pivot_wider(names_from = .metric, values_from = .estimate) %>%
    mutate(tipo = "Anomalía")
) %>%
  arrange(desc(roc_auc))

cat("\n=== Tabla final (supervisados + Isolation Forest) ===\n")
print(tabla_final, n = Inf)

# tabla de confucion mejor modelo
mejor_nombre <- tabla_final$modelo[1]
cat(sprintf("\nMejor modelo: %s\n", mejor_nombre))

mejor_preds <- list(
  "Logística Lasso"  = preds_log,
  "RF pesos"         = preds_rf,
  "XGBoost"          = preds_xgb,
  "RF submuestreo"   = preds_rf_sub,
  "Isolation Forest" = preds_iso
)[[mejor_nombre]]

conf_mat(mejor_preds, truth = OBJETIVO, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(title = sprintf("Matriz de confusión — %s", mejor_nombre)) +
  theme_minimal()

conf_mat(preds_iso, truth = OBJETIVO, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(title = sprintf("Matriz de confusión — %s", "Isolation Forest")) +
  theme_minimal()

# importancia de variables
# score base (sin permutar)
score_base <- predict(iso_model, test_iso_baked)

roc_base <- roc_auc_vec(
  truth    = test$OBJETIVO,
  estimate = score_base,
  event_level = "second"
)

# nombre de las columnas predictoras
vars <- names(test_iso_baked)

# permutación variable por variable
importancia_iso <- map_dfr(vars, function(v) {
  test_perm <- test_iso_baked %>%
    mutate(across(all_of(v), ~ sample(.)))
  
  score_perm <- predict(iso_model, test_perm)
  
  roc_perm <- roc_auc_vec(
    truth       = test$OBJETIVO,
    estimate    = score_perm,
    event_level = "second"
  )
  
  tibble(
    variable   = v,
    roc_base   = roc_base,
    roc_perm   = roc_perm,
    caida      = roc_base - roc_perm   # caída = importancia
  )
}) %>%
  arrange(desc(caida))

print(importancia_iso)

# gráfico
ggplot(importancia_iso %>% slice_max(caida, n = 15),
       aes(x = reorder(variable, caida), y = caida)) +
  geom_col(fill = "darkseagreen3") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    title    = "Importancia de variables — Isolation Forest",
    subtitle = "Caída en ROC-AUC al permutar cada variable",
    x        = NULL,
    y        = "Caída en ROC-AUC"
  ) +
  theme_minimal()
