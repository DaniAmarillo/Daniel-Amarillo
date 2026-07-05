paquetes <- c(
  "tidyverse","lubridate","hrbrthemes","forcats","scales","car","PRROC","pROC","caret","xgboost","Matrix",
  "SHAPforxgboost","ranger","pdp","vip","MASS","broom","car","moments","splines","janitor","patchwork"
  
)

 
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}
# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)


db_2026 <- read.csv("C:/Users/User/Downloads/db_2026.csv")
db_2026$DT_NASCIMENTO_BENEFICIARIO <- ymd(db_2026$DT_NASCIMENTO_BENEFICIARIO) 
db_2026$DT_UTILIZACAO <- ymd(db_2026$DT_UTILIZACAO)
db_2026$CID <- str_squish(db_2026$CID)
#EXPLORACION INICIAL
qry <- db_2026[c("CID","CHAVE_FUNCIONAL")]
qry <- qry %>% 
  mutate(CID = str_remove_all(CID, fixed(".")))
x <- table(unique(qry)$CID)
view(x)
# BUSQUEDA DE PACIENTES CON CONSULTAS RELACIONADAS A LA ANSIEDAD
anx <- db_2026[c("CHAVE_FUNCIONAL","CID")] |> filter(str_detect(CID,"F41\\.*"))
pc_anx <- unique(anx$CHAVE_FUNCIONAL)
# CREACIÓN variable objetivo
db_2026$ANSIEDADE <- as.integer(db_2026$CHAVE_FUNCIONAL %in% pc_anx)
table(db_2026$ANSIEDADE)


df_benef <- unique(db_2026[c("CHAVE_FUNCIONAL","DT_NASCIMENTO_BENEFICIARIO","ANSIEDADE")])
df_benef$EDADE <- floor(time_length(interval(df_benef$DT_NASCIMENTO_BENEFICIARIO, Sys.Date()), "years"))
df_benef$DT_NASCIMENTO_BENEFICIARIO <- NULL


#función robusta de moda
find_mode <- function(x) {
  x <- x[!is.na(x)]                 # Remove NA values
  if (length(x) == 0) return(NA)     # Handle empty groups
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
#funcion agrupar
agrupar_categorias_raras <- function(x, min_freq = 30) {
  frecuencias <- table(x)
  categorias_raras <- names(frecuencias[frecuencias < min_freq])
  x <- as.character(x)
  x[x %in% categorias_raras] <- "Outro"
  factor(x)
}
#####MODA DE CADA VARIABLE A NIVEL BENEFICIARIO QUE SE REPITE#######


df_benef <-  db_2026 %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    TIPO_BENEFICIARIO = find_mode(TIPO_BENEFICIARIO),
    .groups = "drop"                 # Drop grouping metadata
  ) |> left_join(df_benef)
df_benef <- df_benef %>%
  mutate(TIPO_BENEFICIARIO = case_match(TIPO_BENEFICIARIO,
                                        "MAE" ~ "TITULAR",
                                        "FILHO" ~ "DEPENDENTE",
                                        "IGNORADO" ~ "Não Informado",
                                        .default = TIPO_BENEFICIARIO # Mantiene igual todo lo demás
  ))
 # Mantiene igual todo lo demás
df_benef <-  db_2026 %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    SEXO_BENEFICIARIO = find_mode(SEXO_BENEFICIARIO),
    .groups = "drop"                 # Drop grouping metadata
  ) |> left_join(df_benef)
df_benef <- df_benef %>%
  mutate(SEXO_BENEFICIARIO = case_match(SEXO_BENEFICIARIO,
                                        "MASCULINO" ~ "M",
                                        .default = SEXO_BENEFICIARIO # Mantiene igual todo lo demás
  ))

df_benef <-  db_2026 %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    UF_CNES_PREST_HOSPITALAR = find_mode(UF_CNES_PREST_HOSPITALAR),
    .groups = "drop"                 # Drop grouping metadata
  ) |> left_join(df_benef)

df_benef <-  db_2026 %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    TIPO_UNIDADE_PREST_HOSPITALAR = find_mode(TIPO_UNIDADE_PREST_HOSPITALAR),
    .groups = "drop"                 # Drop grouping metadata
  ) |> left_join(df_benef)
df_benef <- df_benef %>%
  mutate(across(c(TIPO_UNIDADE_PREST_HOSPITALAR),
                ~ agrupar_categorias_raras(.x, min_freq = 30)))
###ANÁLISIS DESCRIPTIVO####
df_anx <- db_2026 |> filter(ANSIEDADE == 1)
#Número de beneficiarios 
length(unique(db_2026$CHAVE_FUNCIONAL)) # 653.631 beneficiarios
#Número de beneficiarios con Ansiedad diagnosticada
length(unique(df_anx$CHAVE_FUNCIONAL)) # 419 beneficiarios con ansiedad diagnosticada
#Número de utilizaciones o consultas.
length(db_2026$VALOR_UTILIZACAO) # 9.345.278 utilizaciones
#Número de utilizaciones o consultas de pacientes con ansiedad diagnosticada
length(df_anx$VALOR_UTILIZACAO) # 6.608 utilizaciones
#Número de procedimientos.
nrow(unique(db_2026[c("DT_UTILIZACAO","CHAVE_FUNCIONAL","CD_PROCEDIMENTO")])) #6.627.032
#Número de procedimientos de beneficiarios diagnosticados con ansiedad
nrow(unique(df_anx[c("DT_UTILIZACAO","CHAVE_FUNCIONAL","CD_PROCEDIMENTO")])) # 5.134
#Distribución de la variable objetivo.
table(unique(db_2026[c("ANSIEDADE","CHAVE_FUNCIONAL")])$ANSIEDADE) #     0:653212      1:419
#Porcentaje de beneficiarios con la enfermedad seleccionada.
  419/653631 * 100                                              #0.06410345
#Distribución por sexo:
  ## GENERAL =           F 360143 ; M 263627 ; Não Informado 30386 
  table(unique(db_2026[c("ANSIEDADE","CHAVE_FUNCIONAL","SEXO_BENEFICIARIO")])$SEXO_BENEFICIARIO) 
  ## CON ANSIEDAD =     F 227 ; M 185 ; Não Informado 7
  table(unique(df_anx[c("ANSIEDADE","CHAVE_FUNCIONAL","SEXO_BENEFICIARIO")])$SEXO_BENEFICIARIO) 
  # EDAD:
 ## GENERAL
  table(df_benef$EDADE) 
  mean_edad_gen <- floor(mean(df_benef$EDADE,na.rm = T)) # 38 años
  p <- df_benef %>% 
    filter(EDADE > 0) |> 
    ggplot( aes(x=EDADE)) +
    geom_histogram( fill="#449dd1", color="#f1f0ea", alpha=0.9) +
    ggtitle("Distribución etaria de los beneficiados") +
    geom_vline(aes(xintercept = mean_edad_gen), color = "#97041d", linetype = "dashed", linewidth = .5) +
    theme_ipsum_gs() +
    scale_x_continuous(breaks = seq(0, 150, 10))+
    theme(
      plot.title = element_text(size=15)
    )
  p
  ## CON ANSIEDAD 
  table(df_benef[df_benef$ANSIEDADE == 1,]$EDADE)  
  mean_edad_anx <- floor(mean(df_benef[df_benef$ANSIEDADE == 1,]$EDADE,na.rm = T))
  p <- df_benef[df_benef$ANSIEDADE == 1,] %>% 
    filter(EDADE > 0) |> 
    ggplot( aes(x=EDADE)) +
    geom_histogram(fill="#97041d", color="#000000", alpha=0.9) +
    ggtitle("Distribución etaria de los beneficiados con ansiedad") +
    geom_vline(aes(xintercept = mean_edad_anx), color = "#054a91", linetype = "dashed", linewidth = .5) +
    theme_ipsum_ps() +
    scale_x_continuous(breaks = seq(0, 150, 10))+
    theme(
      plot.title = element_text(size=15)
    )
  p 
  # tipo de beneficiario:
  #General 
  table(df_benef$TIPO_BENEFICIARIO) 
  p <- df_benef %>%   
    ggplot(aes(x=fct_infreq(TIPO_BENEFICIARIO))) +
    geom_bar( fill="#449dd1", color="#f1f0ea", alpha=0.9) +
    scale_y_continuous(
      breaks = seq(0, 421841, 25000), # Ajusta el límite superior según tus datos
      labels = function(x) format(x, scientific = FALSE, big.mark = ",")          # Transforma 1e+05 en 100,000
    ) + 
    labs(
      title = "Distribución de los beneficiados por tipo",
      x = "Tipo de beneficiario",
      y = "Cantidad de beneficiarios"  
    ) +
    theme_ipsum_gs()+
    theme(
      plot.title = element_text(size=15)
    )
  p
  #Con ansiedad = 
  table(df_benef[df_benef$ANSIEDADE == 1,]$TIPO_BENEFICIARIO)  
  p <- df_benef[df_benef$ANSIEDADE == 1,] %>% 
    filter(EDADE > 0) |> 
    ggplot(aes(x=fct_infreq(TIPO_BENEFICIARIO))) +
    geom_bar(fill="#97041d", color="#000000", alpha=0.9) +
    labs(
      title = "Distribución de los beneficiados con ansiedad por tipo",
      x = "Tipo de beneficiario",
      y = "Cantidad de beneficiarios"  
    ) +
    theme_ipsum_gs()+
    theme(
      plot.title = element_text(size=15)
    )
  p
  # especialidad:
  table(db_2026$CETIPO) 
  p <- db_2026 %>%   
    ggplot(aes(x=fct_infreq(CETIPO))) +
    geom_bar( fill="#449dd1", color="#f1f0ea", alpha=0.9) +
    scale_y_continuous(
      breaks = seq(0, 4218417, 800000), # Ajusta el límite superior según tus datos
      labels = function(x) format(x, scientific = FALSE, big.mark = ",")          # Transforma 1e+05 en 100,000
    ) + 
    scale_x_discrete() + 
    labs(
      title = "Distribución de las consultas por especialidad",
      x = "Especialidad",
      y = "Cantidad de consultas"  
    ) +
    theme_ipsum_gs()+
    theme(
      plot.title = element_text(size=15)
    )
  p  
  
  #CON ANSIEDAD:
  table(df_anx$CETIPO) 
  p <- df_anx %>%   
    ggplot(aes(x=fct_infreq(CETIPO))) +
    geom_bar(fill="#97041d", color="#000000", alpha=0.9) +
    labs(
      title = "Distribución de las consultas por especialidad",
      x = "Especialidad",
      y = "Cantidad de consultas"  
    ) +
    theme_ipsum_gs()+
    theme(
      plot.title = element_text(size=15)
    )
  p
  #Análisis de valores faltantes.
  db_2026 |> filter(CID == regex("N/A") )
  
  db_2026 %>%
    summarise(across(everything(), ~ sum(is.na(.))))  
  
##### preparación variables adicionales #####
  sum(duplicated(df_benef$CHAVE_FUNCIONAL))
  
##### VALOR DE PROCEDIMIENTO TOTAL #####
  df_benef <-  db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      VALOR_PROCEDIMENTO_TOTAL = sum(VALOR_UTILIZACAO),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef)
##### NÚMERO DE PROCEDIMIENTO TOTAL #####
  df_benef <- db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      N_PROCEDIMENTOS = n(),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef)
##### VALOR MÁXIMO DE PROCEDIMIENTO #####
  df_benef <-  db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      MAX_VALOR_PROCEDIMENTO = max(VALOR_UTILIZACAO),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef)  

##### VALOR PROMEDIO DE PROCEDIMIENTO  #####
   df_benef <-  db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      MEDIA_VALOR_PROCEDIMENTO = round(mean(VALOR_UTILIZACAO),2),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef) 

##### NUMERO DE UTILIZACIONES#####
    df_benef <- db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      N_UTILIZACAO = length(unique(DT_UTILIZACAO)),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef) 
##### TIPOS DE PROCEDIMIENTO DIFERENTES #####
  df_benef <- db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      N_PROCEDIMENTOS_DIFERENTES = length(unique(CD_PROCEDIMENTO)),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef) 
##### ESPECIALIDADES  ACCEDIDAS#####
  frecuencias_anchas <- db_2026 %>%
    count(CHAVE_FUNCIONAL, CETIPO) %>% 
    pivot_wider(
      names_from = CETIPO,    # Cada ítem único se convierte en una columna
      values_from = n,              # La celda tendrá la frecuencia
      values_fill = 0               # Si el ID no tiene ese ítem, rellena con 0
    ) 
  df_benef <- frecuencias_anchas |> left_join(df_benef)
  
##### TIPOS DE DIAGNOSTICOS DIFERENTES #####
  df_benef <- db_2026 %>%
    group_by(CHAVE_FUNCIONAL) %>%
    summarise(
      N_DIAGNOSTICOS_DIFERENTES = length(unique(CID)),
      .groups = "drop"                 # Drop grouping metadata
    ) |> left_join(df_benef)

colnames(df_benef)[colnames(df_benef) == "Internação"] <- "Internacao"

##### FASE DE PREDICCION #####
#####Regresión Logística #####

set.seed(12345) # semilla fija 

## ---- REGRESIÓN LOGÍSTICA----

# 2.1 Verificar que ANSIEDADE es binaria y no tiene NA
stopifnot(all(df_benef$ANSIEDADE %in% c(0, 1)))
stopifnot(sum(is.na(df_benef$ANSIEDADE)) == 0)

df_benef <- df_benef %>%
  mutate(ANSIEDADE = factor(ANSIEDADE, levels = c(0, 1), labels = c("No", "Si")))

# 2.2 Revisar el balance de clases (justifica la metrica principal)
tabla_balance <- table(df_benef$ANSIEDADE)
prop_balance  <- prop.table(tabla_balance)
print(tabla_balance)
print(round(prop_balance, 4))

# Si la clase positiva es < ~15-20%, se considera desbalance relevante
# y se debe priorizar F1 / Sensibilidad / PR-AUC sobre Accuracy.

# 2.3 Excluir identificadores y variables que generan fuga de informacion
# (ajustar nombres segun tus variables reales)
vars_excluir <- c("CHAVE_FUNCIONAL", "CID", "DT_UTILIZACAO",
                  "DT_NASCIMENTO_BENEFICIARIO","N_PROCEDIMENTOS","TIPO_UNIDADE_PREST_HOSPITALAR")


df_modelo <- df_benef %>%
  select(-any_of(vars_excluir))

# 2.4 Revisar valores faltantes remanentes en predictoras
na_por_variable <- colSums(is.na(df_modelo))
print(na_por_variable[na_por_variable > 0])
# Se encuentran 302 edades faltantes debido a la falta de información :
# Se decide excluir las 302 debido a que representan solo el 0.047% de la información y 
# ninguno posee ansiedad diagnosticada 

df_modelo <- df_modelo %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

## ---- 3. Particion train/test estratificada ----

idx_train <- createDataPartition(df_modelo$ANSIEDADE, p = 0.75, list = FALSE)

train_data <- df_modelo[idx_train, ]
test_data  <- df_modelo[-idx_train, ]
names(train_data) <- make.names(names(train_data), unique = TRUE)
names(test_data)  <- make.names(names(test_data),  unique = TRUE)
# Verificar que la proporcion de clases se mantiene en ambos conjuntos
prop.table(table(train_data$ANSIEDADE))
prop.table(table(test_data$ANSIEDADE))

## ---- 4. Manejo del desbalance de clases (solo en TRAIN) ----

# Opcion B (alternativa): sobremuestreo con caret::upSample
 train_bal <- upSample(x = train_data %>% select(-ANSIEDADE),
                        y = train_data$ANSIEDADE, yname = "ANSIEDADE")


## ---- 5. Multicolinealidad (opcional pero recomendado) ----

modelo_vif <- glm(ANSIEDADE ~ ., data = train_bal, family = binomial)
print(vif(modelo_vif))
# Variables con VIF > 5-10 son candidatas a remover o combinar.

## ---- 6. Entrenamiento del modelo logistico ----

modelo_logit <- glm(
  ANSIEDADE ~ .,
  data    = train_bal,
  family  = binomial,
)

summary(modelo_logit)

## ---- 7. Seleccion de umbral optimo (no usar 0.5 por defecto) ----

pred_prob_train <- predict(modelo_logit, newdata = train_bal, type = "response")

roc_train <- roc(response = train_bal$ANSIEDADE, predictor = pred_prob_train,
                 levels = c("No", "Si"), direction = "<")

# Umbral que maximiza Youden's J (sensibilidad + especificidad - 1)
umbral_optimo <- coords(roc_train, "best", best.method = "youden")$threshold
cat("Umbral optimo seleccionado en TRAIN:", umbral_optimo, "\n")

## ---- 8. Evaluacion en TEST ----

pred_prob_test  <- predict(modelo_logit, newdata = test_data, type = "response")
pred_clase_test <- factor(ifelse(pred_prob_test >= umbral_optimo, "Si", "No"),
                          levels = c("No", "Si"))


matriz_conf <- confusionMatrix(pred_clase_test, test_data$ANSIEDADE, positive = "Si")
print(matriz_conf)

# Metricas individuales
accuracy    <- matriz_conf$overall["Accuracy"]
sensibilidad<- matriz_conf$byClass["Sensitivity"]
especificidad<- matriz_conf$byClass["Specificity"]
f1_score    <- matriz_conf$byClass["F1"]

roc_test <- roc(response = test_data$ANSIEDADE, predictor = pred_prob_test,
                levels = c("No", "Si"), direction = "<")
roc_auc  <- auc(roc_test)

pr_test <- pr.curve(
  scores.class0 = pred_prob_test[test_data$ANSIEDADE == "Si"],
  scores.class1 = pred_prob_test[test_data$ANSIEDADE == "No"],
  curve = TRUE
)
pr_auc <- pr_test$auc.integral

cat("\n---- Resumen de metricas en TEST ----\n")
cat("Accuracy:      ", round(accuracy, 4), "\n")
cat("Sensibilidad:  ", round(sensibilidad, 4), "\n")
cat("Especificidad: ", round(especificidad, 4), "\n")
cat("F1-score:      ", round(f1_score, 4), "\n")
cat("ROC-AUC:       ", round(roc_auc, 4), "\n")
cat("PR-AUC:        ", round(pr_auc, 4), "\n")

## ---- 9. Curvas ROC y PR ----

plot(roc_test, main = "Curva ROC - Regresion Logistica (TEST)")
plot(pr_test, main = "Curva Precision-Recall - Regresion Logistica (TEST)")

## ---- 10. Interpretacion: variables mas influyentes ----

coeficientes <- broom::tidy(modelo_logit) %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate)) %>%
  arrange(p.value)

print(coeficientes, n = Inf)
# odds_ratio > 1  -> aumenta la probabilidad de la enfermedad
# odds_ratio < 1  -> disminuye la probabilidad
# Documentar aqui la relacion clinica de las variables mas significativas
# (p.value < 0.05), apoyandose en CIE-10/ICD-10 y literatura del dominio.

## ---- 11. Comparacion train vs test (chequeo de sobreajuste) ----

pred_clase_train <- factor(ifelse(pred_prob_train >= umbral_optimo, "Si", "No"),
                           levels = c("No", "Si"))
matriz_conf_train <- confusionMatrix(pred_clase_train, train_bal$ANSIEDADE, positive = "Si")

cat("\nAccuracy TRAIN:", round(matriz_conf_train$overall["Accuracy"], 4),
    " | Accuracy TEST:", round(accuracy, 4), "\n")
# Diferencias grandes entre train y test sugieren sobreajuste.

## ---- 12. Guardar resultados ----

saveRDS(modelo_logit, "modelo_logistico.rds")

resultados_logit <- tibble(
  modelo        = "Regresion Logistica",
  accuracy      = accuracy,
  sensibilidad  = sensibilidad,
  especificidad = especificidad,
  f1_score      = f1_score,
  roc_auc       = as.numeric(roc_auc),
  pr_auc        = pr_auc,
  umbral        = umbral_optimo
)
##### XGBOOST ####

## ---- XGBoost ----
# Se reutilizan train_data y test_data del script 03 (ANTES del upSample,
# ya que XGBoost maneja el desbalance internamente via scale_pos_weight,
# sin necesidad de duplicar observaciones).
#
# train_data / test_data deben tener:
#   - ANSIEDADE: factor "No"/"Si"
#   - variables predictoras (sin CHAVE_FUNCIONAL, CID, fechas crudas)

stopifnot(exists("train_data"), exists("test_data"))

## ---- 2. Preparar matrices para xgboost ----

# xgboost requiere matriz numerica -> convertir factores a dummies
# model.matrix() genera automaticamente el encoding, quitando el intercepto
formula_modelo <- ANSIEDADE ~ . - 1

train_matrix <- sparse.model.matrix(ANSIEDADE ~ . - 1, data = train_data)
test_matrix  <- sparse.model.matrix(ANSIEDADE ~ . - 1, data = test_data)

label_train <- ifelse(train_data$ANSIEDADE == "Si", 1, 0)
label_test  <- ifelse(test_data$ANSIEDADE  == "Si", 1, 0)
# xgb.DMatrix acepta matrices dispersas directamente
dtrain <- xgb.DMatrix(data = train_matrix, label = label_train)
dtest  <- xgb.DMatrix(data = test_matrix,  label = label_test)

# Alinear columnas de test con las de train (por si alguna categoria
# no aparece en uno de los dos conjuntos tras el encoding)
columnas_train <- colnames(train_matrix)
faltantes_en_test <- setdiff(columnas_train, colnames(test_matrix))
for (col in faltantes_en_test) {
  test_matrix <- cbind(test_matrix, matrix(0, nrow = nrow(test_matrix), ncol = 1,
                                           dimnames = list(NULL, col)))
}
test_matrix <- test_matrix[, columnas_train, drop = FALSE]



dtrain <- xgb.DMatrix(data = train_matrix, label = label_train)
dtest  <- xgb.DMatrix(data = test_matrix,  label = label_test)

## ---- 3. Manejo del desbalance de clases ----

# scale_pos_weight ~ (# negativos) / (# positivos) en TRAIN
# Alternativa a sobremuestreo: no duplica filas, ajusta el gradiente.
n_pos <- sum(label_train == 1)
n_neg <- sum(label_train == 0)
scale_pos_weight <- n_neg / n_pos
cat("scale_pos_weight utilizado:", round(scale_pos_weight, 2), "\n")

## ---- 4. Validacion cruzada para tuning de hiperparametros ----

# Grid reducido y razonable; ajustar segun tiempo de computo disponible
grid_params <- expand.grid(
  max_depth        = c(3, 5, 7),
  eta              = c(0.01, 0.05, 0.1),
  min_child_weight = c(1, 5),
  subsample        = 0.8,
  colsample_bytree = 0.8
)

resultados_cv <- list()

for (i in seq_len(nrow(grid_params))) {
  params <- list(
    objective         = "binary:logistic",
    eval_metric       = "aucpr",
    max_depth         = grid_params$max_depth[i],
    eta               = grid_params$eta[i],
    min_child_weight  = grid_params$min_child_weight[i],
    subsample         = grid_params$subsample[i],
    colsample_bytree  = grid_params$colsample_bytree[i],
    scale_pos_weight  = scale_pos_weight
  )
  
  cv <- xgb.cv(
    params                = params,
    data                  = dtrain,
    nrounds               = 500,
    nfold                 = 5,
    stratified            = TRUE,
    early_stopping_rounds = 20,
    verbose               = FALSE
  )
  
  # Busca la columna de test-aucpr sin importar guion o guion bajo
  log_cv <- as.data.frame(cv$evaluation_log)
  col_metric <- grep("test.*aucpr.*mean", colnames(log_cv), value = TRUE)
  
  if (length(col_metric) == 0) {
    stop("No se encontro la columna de metrica esperada. Columnas disponibles: ",
         paste(colnames(log_cv), collapse = ", "))
  }
  
  mejor_iter  <- which.max(log_cv[[col_metric]])
  mejor_score <- log_cv[[col_metric]][mejor_iter]
  
  resultados_cv[[i]] <- tibble(
    max_depth        = params$max_depth,
    eta              = params$eta,
    min_child_weight = params$min_child_weight,
    nrounds_optimo   = mejor_iter,
    pr_auc_cv        = as.numeric(mejor_score)
  )
}

tabla_cv <- bind_rows(resultados_cv) %>% arrange(desc(pr_auc_cv))
print(tabla_cv)

mejor_config <- tabla_cv %>% slice(1) ###  max_depth   eta min_child_weight nrounds_optimo pr_auc_cv
                                      ###    1         7   0.1                1            200     0.694
print(mejor_config)

## ---- 5. Entrenamiento del modelo final con los mejores hiperparametros ----

params_finales <- list(
  objective         = "binary:logistic",
  eval_metric       = "aucpr",
  max_depth         = mejor_config$max_depth,
  eta               = mejor_config$eta,
  min_child_weight  = mejor_config$min_child_weight,
  subsample         = 0.8,
  colsample_bytree  = 0.8,
  scale_pos_weight  = scale_pos_weight
)

modelo_xgb <- xgb.train(
  params  = params_finales,
  data    = dtrain,
  nrounds = mejor_config$nrounds_optimo,
  evals = list(train = dtrain, test = dtest),
  verbose = 0
)

## ---- 6. Seleccion de umbral optimo (igual criterio que en logistica) ----

pred_prob_train_xgb <- predict(modelo_xgb, dtrain)

roc_train_xgb <- roc(response = label_train, predictor = pred_prob_train_xgb,
                     levels = c(0, 1), direction = "<")

umbral_optimo_xgb <- coords(roc_train_xgb, "best", best.method = "youden")$threshold
cat("Umbral optimo XGBoost (train):", umbral_optimo_xgb, "\n")

## ---- 7. Evaluacion en TEST ----

pred_prob_test_xgb  <- predict(modelo_xgb, dtest)
pred_clase_test_xgb <- factor(ifelse(pred_prob_test_xgb >= umbral_optimo_xgb, "Si", "No"),
                              levels = c("No", "Si"))

matriz_conf_xgb <- confusionMatrix(
  pred_clase_test_xgb,
  factor(ifelse(label_test == 1, "Si", "No"), levels = c("No", "Si")),
  positive = "Si"
)
print(matriz_conf_xgb)

accuracy_xgb      <- matriz_conf_xgb$overall["Accuracy"]
sensibilidad_xgb  <- matriz_conf_xgb$byClass["Sensitivity"]
especificidad_xgb <- matriz_conf_xgb$byClass["Specificity"]
f1_xgb            <- matriz_conf_xgb$byClass["F1"]

roc_test_xgb <- roc(response = label_test, predictor = pred_prob_test_xgb,
                    levels = c(0, 1), direction = "<")
roc_auc_xgb  <- auc(roc_test_xgb)

pr_test_xgb <- pr.curve(
  scores.class0 = pred_prob_test_xgb[label_test == 1],
  scores.class1 = pred_prob_test_xgb[label_test == 0],
  curve = TRUE
)
pr_auc_xgb <- pr_test_xgb$auc.integral

cat("\n---- Resumen de metricas en TEST (XGBoost) ----\n")
cat("Accuracy:      ", round(accuracy_xgb, 4), "\n")
cat("Sensibilidad:  ", round(sensibilidad_xgb, 4), "\n")
cat("Especificidad: ", round(especificidad_xgb, 4), "\n")
cat("F1-score:      ", round(f1_xgb, 4), "\n")
cat("ROC-AUC:       ", round(roc_auc_xgb, 4), "\n")
cat("PR-AUC:        ", round(pr_auc_xgb, 4), "\n")

# Referencia: PR-AUC esperado bajo azar = prevalencia de la clase positiva
prevalencia_test <- mean(label_test == 1)
cat("PR-AUC baseline (azar):", round(prevalencia_test, 4), "\n")

## ---- 8. Curvas ROC y PR ----

plot(roc_test_xgb, main = "Curva ROC - XGBoost (TEST)")
plot(pr_test_xgb, main = "Curva Precision-Recall - XGBoost (TEST)")

## ---- 9. Interpretacion: importancia de variables ----

# 9.1 Importancia por ganancia (gain) - rapida, nativa de xgboost
importancia <- xgb.importance(feature_names = colnames(train_matrix), model = modelo_xgb)
print(importancia)
xgb.plot.importance(importancia, top_n = 15, main = "Importancia de variables - XGBoost")

# 9.2 SHAP values - interpretacion mas robusta (direccion + magnitud del efecto)
shap_values <- shap.values(xgb_model = modelo_xgb, X_train = train_matrix)
shap_long   <- shap.prep(xgb_model = modelo_xgb, X_train = train_matrix)

# Grafico resumen: variables mas influyentes y direccion del efecto
shap.plot.summary(shap_long)

# Dependencia parcial (PDP) para la variable mas importante
variable_top <- importancia$Feature[1]
shap.plot.dependence(shap_long, x = variable_top)

## ---- 10. Comparacion train vs test (chequeo de sobreajuste) ----

pred_clase_train_xgb <- factor(ifelse(pred_prob_train_xgb >= umbral_optimo_xgb, "Si", "No"),
                               levels = c("No", "Si"))
matriz_conf_train_xgb <- confusionMatrix(
  pred_clase_train_xgb,
  factor(ifelse(label_train == 1, "Si", "No"), levels = c("No", "Si")),
  positive = "Si"
)

cat("\nAccuracy TRAIN:", round(matriz_conf_train_xgb$overall["Accuracy"], 4),
    " | Accuracy TEST:", round(accuracy_xgb, 4), "\n")
# Diferencias grandes entre train y test sugieren sobreajuste
# (comun en XGBoost si nrounds es muy alto o el arbol es muy profundo).

## ---- 11. Comparacion final: Regresion Logistica vs XGBoost ----

# Requiere que 'resultados_logit' exista (generado en 03_modelamiento_logistico.R)
if (exists("resultados_logit")) {
  resultados_xgb <- tibble(
    modelo        = "XGBoost",
    accuracy      = accuracy_xgb,
    sensibilidad  = sensibilidad_xgb,
    especificidad = especificidad_xgb,
    f1_score      = f1_xgb,
    roc_auc       = as.numeric(roc_auc_xgb),
    pr_auc        = pr_auc_xgb,
    umbral        = umbral_optimo_xgb
  )
  
  tabla_comparacion <- bind_rows(resultados_logit, resultados_xgb)
  print(tabla_comparacion)
  
  write_csv(tabla_comparacion, "comparacion_modelos.csv")
}

## ---- 12. Guardar resultados ----

xgb.save(modelo_xgb, "modelo_xgboost.model")

resultados_xgb_solo <- tibble(
  modelo        = "XGBoost",
  accuracy      = accuracy_xgb,
  sensibilidad  = sensibilidad_xgb,
  especificidad = especificidad_xgb,
  f1_score      = f1_xgb,
  roc_auc       = as.numeric(roc_auc_xgb),
  pr_auc        = pr_auc_xgb,
  umbral        = umbral_optimo_xgb
)
## ---- Random Forest ----
str(train_data$ANSIEDADE)  # debe ser factor "No"/"Si"

## ---- 2. Manejo del desbalance de clases ----

# class.weights: se asigna mas peso a la clase minoritaria, en el mismo
# espiritu que 'pesos' en la logistica y 'scale_pos_weight' en XGBoost
prop_clases <- prop.table(table(train_data$ANSIEDADE))
pesos_clase <- c("No" = 1 / prop_clases[["No"]], "Si" = 1 / prop_clases[["Si"]])
pesos_clase <- pesos_clase / sum(pesos_clase)  # normalizar
print(pesos_clase)

## ---- 3. Validacion cruzada para tuning de hiperparametros ----

# Grid reducido y razonable; ajustar segun tiempo de computo disponible
grid_params <- expand.grid(
  mtry          = c(floor(sqrt(ncol(train_data) - 1)),
                    floor((ncol(train_data) - 1) / 3),
                    floor((ncol(train_data) - 1) / 2)),
  min.node.size = c(1, 5, 10),
  num.trees     = c(500)
)
grid_params <- distinct(grid_params)

k <- 5
folds <- createFolds(train_data$ANSIEDADE, k = k, list = TRUE, returnTrain = FALSE)

resultados_cv <- list()

for (i in seq_len(nrow(grid_params))) {
  
  pr_auc_folds <- numeric(k)
  
  for (f in seq_len(k)) {
    idx_val   <- folds[[f]]
    fold_train <- train_data[-idx_val, ]
    fold_val   <- train_data[idx_val, ]
    
    modelo_fold <- ranger(
      formula        = ANSIEDADE ~ .,
      data           = fold_train,
      num.trees      = grid_params$num.trees[i],
      mtry           = grid_params$mtry[i],
      min.node.size  = grid_params$min.node.size[i],
      class.weights  = pesos_clase,
      probability    = TRUE,     # necesario para obtener probabilidades
      importance     = "none",   # se calcula solo en el modelo final
      seed           = 2016325
    )
    
    pred_val <- predict(modelo_fold, data = fold_val)$predictions[, "Si"]
    
    pr_fold <- pr.curve(
      scores.class0 = pred_val[fold_val$ANSIEDADE == "Si"],
      scores.class1 = pred_val[fold_val$ANSIEDADE == "No"],
      curve = FALSE
    )
    pr_auc_folds[f] <- pr_fold$auc.integral
  }
  
  resultados_cv[[i]] <- tibble(
    mtry          = grid_params$mtry[i],
    min.node.size = grid_params$min.node.size[i],
    num.trees     = grid_params$num.trees[i],
    pr_auc_cv     = mean(pr_auc_folds)
  )
  
  cat("Config", i, "de", nrow(grid_params), "- PR-AUC promedio:",
      round(mean(pr_auc_folds), 4), "\n")
}

tabla_cv <- bind_rows(resultados_cv) %>% arrange(desc(pr_auc_cv))
print(tabla_cv)

mejor_config <- tabla_cv %>% slice(1)
print(mejor_config)   #  mtry min.node.size num.trees pr_auc_cv
                       #  1     5             1       500     0.706

## ---- 4. Entrenamiento del modelo final con los mejores hiperparametros ----

modelo_rf <- ranger(
  formula        = ANSIEDADE ~ .,
  data           = train_data,
  num.trees      = mejor_config$num.trees,
  mtry           = mejor_config$mtry,
  min.node.size  = mejor_config$min.node.size,
  class.weights  = pesos_clase,
  probability    = TRUE,
  importance     = "permutation",  # importancia mas confiable que Gini
  seed           = 2016325
)

print(modelo_rf)

## ---- 5. Seleccion de umbral optimo (mismo criterio que los otros modelos) ----

pred_prob_train_rf <- predict(modelo_rf, data = train_data)$predictions[, "Si"]

roc_train_rf <- roc(response = train_data$ANSIEDADE, predictor = pred_prob_train_rf,
                    levels = c("No", "Si"), direction = "<")

umbral_optimo_rf <- coords(roc_train_rf, "best", best.method = "youden")$threshold
cat("Umbral optimo Random Forest (train):", umbral_optimo_rf, "\n")

## ---- 6. Evaluacion en TEST ----

pred_prob_test_rf  <- predict(modelo_rf, data = test_data)$predictions[, "Si"]
pred_clase_test_rf <- factor(ifelse(pred_prob_test_rf >= umbral_optimo_rf, "Si", "No"),
                             levels = c("No", "Si"))

matriz_conf_rf <- confusionMatrix(pred_clase_test_rf, test_data$ANSIEDADE, positive = "Si")
print(matriz_conf_rf)

accuracy_rf      <- matriz_conf_rf$overall["Accuracy"]
sensibilidad_rf  <- matriz_conf_rf$byClass["Sensitivity"]
especificidad_rf <- matriz_conf_rf$byClass["Specificity"]
f1_rf            <- matriz_conf_rf$byClass["F1"]

roc_test_rf <- roc(response = test_data$ANSIEDADE, predictor = pred_prob_test_rf,
                   levels = c("No", "Si"), direction = "<")
roc_auc_rf  <- auc(roc_test_rf)

pr_test_rf <- pr.curve(
  scores.class0 = pred_prob_test_rf[test_data$ANSIEDADE == "Si"],
  scores.class1 = pred_prob_test_rf[test_data$ANSIEDADE == "No"],
  curve = TRUE
)
pr_auc_rf <- pr_test_rf$auc.integral

cat("\n---- Resumen de metricas en TEST (Random Forest) ----\n")
cat("Accuracy:      ", round(accuracy_rf, 4), "\n")
cat("Sensibilidad:  ", round(sensibilidad_rf, 4), "\n")
cat("Especificidad: ", round(especificidad_rf, 4), "\n")
cat("F1-score:      ", round(f1_rf, 4), "\n")
cat("ROC-AUC:       ", round(roc_auc_rf, 4), "\n")
cat("PR-AUC:        ", round(pr_auc_rf, 4), "\n")

# Referencia: PR-AUC esperado bajo azar = prevalencia de la clase positiva
prevalencia_test <- mean(test_data$ANSIEDADE == "Si")
cat("PR-AUC baseline (azar):", round(prevalencia_test, 4), "\n")

## ---- 7. Curvas ROC y PR ----

plot(roc_test_rf, main = "Curva ROC - Random Forest (TEST)")
plot(pr_test_rf, main = "Curva Precision-Recall - Random Forest (TEST)")

## ---- 8. Interpretacion: importancia de variables ----

# 8.1 Importancia por permutacion (nativa del modelo)
importancia_rf <- sort(modelo_rf$variable.importance, decreasing = TRUE)
print(head(importancia_rf, 15))

vip(modelo_rf, num_features = 15,
    geom = "col", aesthetics = list(fill = "steelblue")) +
  ggtitle("Importancia de variables - Random Forest (permutacion)")

# 8.2 Grafico de dependencia parcial (PDP) para la variable mas importante
variable_top_rf <- names(importancia_rf)[1]

# pdp::partial requiere una funcion de prediccion que devuelva un vector numerico
pred_wrapper <- function(object, newdata) {
  predict(object, data = newdata)$predictions[, "Si"]
}

pdp_rf <- partial(
  modelo_rf,
  pred.var    = variable_top_rf,
  train       = train_data,
  pred.fun    = pred_wrapper,
  grid.resolution = 20
)

plotPartial(pdp_rf, main = paste("Dependencia parcial -", variable_top_rf))

## ---- 9. Comparacion train vs test (chequeo de sobreajuste) ----

pred_clase_train_rf <- factor(ifelse(pred_prob_train_rf >= umbral_optimo_rf, "Si", "No"),
                              levels = c("No", "Si"))
matriz_conf_train_rf <- confusionMatrix(pred_clase_train_rf, train_data$ANSIEDADE, positive = "Si")

cat("\nAccuracy TRAIN:", round(matriz_conf_train_rf$overall["Accuracy"], 4),
    " | Accuracy TEST:", round(accuracy_rf, 4), "\n")
# Random Forest tiende a sobreajustar menos que arboles individuales,
# pero revisa esta brecha de todas formas.

## ---- 10. Comparacion final: Logistica vs XGBoost vs Random Forest ----

resultados_rf <- tibble(
  modelo        = "Random Forest",
  accuracy      = accuracy_rf,
  sensibilidad  = sensibilidad_rf,
  especificidad = especificidad_rf,
  f1_score      = f1_rf,
  roc_auc       = as.numeric(roc_auc_rf),
  pr_auc        = pr_auc_rf,
  umbral        = umbral_optimo_rf
)

# Si ya existe una tabla de comparacion previa (logistica + xgboost), se anexa
if (exists("tabla_comparacion")) {
  tabla_comparacion <- bind_rows(tabla_comparacion, resultados_rf)
} else if (exists("resultados_logit")) {
  tabla_comparacion <- bind_rows(resultados_logit, resultados_rf)
} else {
  tabla_comparacion <- resultados_rf
}

print(tabla_comparacion)






## ---- ESTIMACIÓN DE COSTO ----
##
## Se UTILIZAN LOS DATOS nivel de UTILIZACION (CHAVE_FUNCIONAL +
## DT_UTILIZACAO) en vez de procedimiento individual o beneficiario,
## porque:
##   - A nivel de PROCEDIMIENTO se pierde el contexto de la atencion
##     completa (una consulta puede tener varios procedimientos).
##   - A nivel de BENEFICIARIO se mezclan utilizaciones de distinta
##     naturaleza (una internacion cara con varias consultas baratas),
##     lo que distorsiona el "costo tipico de una atencion".
##   - A nivel de UTILIZACION se obtiene una unidad clinicamente
##     interpretable: "cuanto cuesta en promedio una atencion a un
##     beneficiario con esta enfermedad".
##
## Ajusta esta decision si tu enfoque conceptual es distinto, pero
## DOCUMENTA Y JUSTIFICA la eleccion en tu taller_3.Rmd.

# Filtrar solo utilizaciones de beneficiarios con la enfermedad seleccionada
# (ajustar el filtro segun como identifiques la utilizacion especifica:
# aqui se asume que se cuenta con el CID a nivel de transaccion)

# Agregar a nivel de utilizacion (CHAVE_FUNCIONAL + DT_UTILIZACAO)
# sumando el costo de todos los procedimientos de esa atencion, y
# tomando el resto de caracteristicas como la moda/max de la atencion
df_benef_costo<- df_benef |> 
  filter(MEDIA_VALOR_PROCEDIMENTO > 0 & ANSIEDADE ==1)  # excluir registros de costo 0/negativo

cat("Numero de utilizaciones analizadas:", nrow(df_benef_costo), "\n")

## ---- 2. Analisis descriptivo del costo ----

resumen_costo <- df_benef_costo %>%
  summarise(
    n            = n(),
    media        = mean(MEDIA_VALOR_PROCEDIMENTO),
    mediana      = median(MEDIA_VALOR_PROCEDIMENTO),
    sd           = sd(MEDIA_VALOR_PROCEDIMENTO),
    cv           = sd / mean(MEDIA_VALOR_PROCEDIMENTO),  # coef. de variacion
    p25          = quantile(MEDIA_VALOR_PROCEDIMENTO, 0.25),
    p75          = quantile(MEDIA_VALOR_PROCEDIMENTO, 0.75),
    p95          = quantile(MEDIA_VALOR_PROCEDIMENTO, 0.95),
    minimo       = min(MEDIA_VALOR_PROCEDIMENTO),
    maximo       = max(MEDIA_VALOR_PROCEDIMENTO),
    asimetria    = skewness(MEDIA_VALOR_PROCEDIMENTO),
    curtosis     = kurtosis(MEDIA_VALOR_PROCEDIMENTO)
  )

print(resumen_costo)
# Un coeficiente de variacion alto (>1) y asimetria positiva fuerte son
# tipicos en datos de costos en salud: pocos casos muy costosos (colas
# largas a la derecha) dominan la variabilidad. Esto justifica NO usar
# un modelo lineal simple sobre el costo bruto.

## ---- 3. Visualizacion de la distribucion ----
ggplot(df_benef_costo, aes(x = MEDIA_VALOR_PROCEDIMENTO)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribucion del costo por utilizacion",
       x = "Costo (VALOR_UTILIZACAO agregado)", y = "Frecuencia")

ggplot(df_benef_costo, aes(x = log1p(MEDIA_VALOR_PROCEDIMENTO))) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  labs(title = "Distribucion del costo (escala log)",
       x = "log(1 + costo)", y = "Frecuencia")
# Si la version log se ve mucho mas simetrica, respalda usar un modelo
# log-lineal o GLM Gamma con enlace log (ver seccion 5).

## ---- 4. Analisis de valores extremos (outliers) ----

# Metodo IQR para identificar outliers
q1 <- quantile(df_benef_costo$MEDIA_VALOR_PROCEDIMENTO, 0.25)
q3 <- quantile(df_benef_costo$MEDIA_VALOR_PROCEDIMENTO, 0.75)
iqr <- q3 - q1
limite_superior <- q3 + 1.5 * iqr

n_outliers <- sum(df_benef_costo$MEDIA_VALOR_PROCEDIMENTO > limite_superior)
pct_outliers <- n_outliers / nrow(df_benef) * 100

cat("Limite superior (IQR):", round(limite_superior, 2), "\n")
cat("Numero de outliers:", n_outliers, "(", round(pct_outliers, 2), "% )\n")

# IMPORTANTE: en costos de salud, los "outliers" suelen ser casos reales
# (internaciones largas, UCI, cirugias complejas), NO errores de datos.
# No se recomienda eliminarlos sin evidencia de error; en su lugar, se
# modelan explicitamente (ver GLM Gamma, que maneja bien colas largas).

# Comparar caracteristicas de outliers vs no-outliers
df_benef_costo %>%
  mutate(es_outlier = MEDIA_VALOR_PROCEDIMENTO > limite_superior) %>%
  group_by(es_outlier) %>%
  summarise(
    edad_promedio  = mean(EDADE, na.rm = TRUE),
    costo_total_promedio = mean(MEDIA_VALOR_PROCEDIMENTO)
  )
# Si los outliers tienen mucha mayor tasa de UTI/internacion, confirma
# que son casos clinicamente distintos y no errores -> se deben MODELAR,
# no eliminar.

## ---- 5. Modelo de costo esperado ----
##
## Se usa un GLM Gamma con enlace log en vez de regresion lineal simple,
## porque:
##   - El costo es estrictamente positivo (Gamma respeta ese soporte).
##   - La varianza crece con la media (heterocedasticidad tipica de
##     costos en salud) -> Gamma la modela naturalmente.
##   - El enlace log da coeficientes interpretables como efectos
##     multiplicativos (%) sobre el costo esperado.

# Preparar variables predictoras (excluir identificadores)
costo_modelo <- df_benef_costo %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), droplevels))  %>%  # por si quedan niveles vacios
  dplyr::select(- CHAVE_FUNCIONAL,-ANSIEDADE,-N_PROCEDIMENTOS,-VALOR_PROCEDIMENTO_TOTAL,-MAX_VALOR_PROCEDIMENTO,-N_PROCEDIMENTOS_DIFERENTES)
costo_modelo <- costo_modelo %>% filter(!is.na(EDADE))
costo_modelo <- costo_modelo %>%
  filter(TIPO_BENEFICIARIO != "Não Informado")

# Recien ahora particionar

idx_train_costo <- sample(seq_len(nrow(costo_modelo)),
                          size = 0.75 * nrow(costo_modelo))
train_costo <- costo_modelo[idx_train_costo, ]
test_costo  <- costo_modelo[-idx_train_costo, ]

modelo_costo <- glm(
  MEDIA_VALOR_PROCEDIMENTO ~ . ,
  data   = train_costo,
  family = Gamma(link = "log"),
  control = glm.control(maxit = 100, epsilon = 1e-8)  # default es maxit=25
)


summary(modelo_costo)
# Verificar multicolinealidad
print(vif(modelo_costo))

## ---- 6. Evaluacion del modelo de costo ----

pred_costo_test <- predict(modelo_costo, newdata = test_costo, type = "response")

# Metricas de error tipicas para modelos de costo
mae  <- mean(abs(pred_costo_test - test_costo$MEDIA_VALOR_PROCEDIMENTO))
rmse <- sqrt(mean((pred_costo_test - test_costo$MEDIA_VALOR_PROCEDIMENTO)^2))
mape <- mean(abs((pred_costo_test - test_costo$MEDIA_VALOR_PROCEDIMENTO) /
                   test_costo$MEDIA_VALOR_PROCEDIMENTO)) * 100

# R2 pseudo (correlacion al cuadrado entre observado y predicho)
r2_pseudo <- cor(pred_costo_test, test_costo$MEDIA_VALOR_PROCEDIMENTO)^2

cat("\n---- Metricas del modelo de costo (TEST) ----\n")
cat("MAE:  ", round(mae, 2), "\n")
cat("RMSE: ", round(rmse, 2), "\n")
cat("MAPE: ", round(mape, 2), "%\n")
cat("Pseudo R2:", round(r2_pseudo, 4), "\n")

# Grafico observado vs predicho
ggplot(data.frame(observado = test_costo$MEDIA_VALOR_PROCEDIMENTO,
                  predicho  = pred_costo_test),
       aes(x = observado, y = predicho)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Costo observado vs. predicho",
       x = "Costo observado", y = "Costo predicho")
## ---- 7. Interpretacion: variables asociadas al costo ----

coeficientes_costo <- tidy(modelo_costo) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    efecto_multiplicativo = exp(estimate),          # interpretacion directa
    cambio_pct            = (efecto_multiplicativo - 1) * 100
  ) %>%
  arrange(p.value)

print(coeficientes_costo, n = Inf)
# Interpretacion: un coeficiente con cambio_pct = +35 significa que,
# manteniendo las demas variables constantes, esa condicion incrementa
# el costo esperado en un 35% en promedio (enlace log = efecto
# multiplicativo). Reportar solo terminos con p.value < 0.05 como
# "significativos", pero discutir tambien la magnitud practica.

## ---- 8. Costo esperado bajo distintos perfiles ----
##
## Se construyen perfiles representativos (ej. paciente ambulatorio vs
## paciente internado en UCI) para ilustrar el rango de costo esperado.

perfiles <- test_costo %>%
  distinct( TIPO_BENEFICIARIO,SEXO_BENEFICIARIO,
           TIPO_UNIDADE_PREST_HOSPITALAR, UF_CNES_PREST_HOSPITALAR, .keep_all = TRUE) %>%
  mutate(
    edad = median(costo_modelo$EDADE, na.rm = TRUE),
    n_procedimientos = 1
  )

# Ejemplo explicito de perfiles contrastantes (ajustar niveles segun tus datos)
perfil_leve <- test_costo[1, ] %>%
  mutate(UTI = factor("No", levels = levels(test_costo$UTI)),
         INTERNADO = factor("No", levels = levels(test_costo$INTERNADO)),
         edad = 40, n_procedimientos = 1)

perfil_severo <- test_costo[1, ] %>%
  mutate(UTI = factor("Si", levels = levels(test_costo$UTI)),
         INTERNADO = factor("Si", levels = levels(test_costo$INTERNADO)),
         edad = 70, n_procedimientos = 5)

perfiles_comparacion <- bind_rows(
  perfil_leve    %>% mutate(perfil = "Ambulatorio, sin UCI, 40 anios"),
  perfil_severo  %>% mutate(perfil = "Internado con UCI, 70 anios")
)

perfiles_comparacion$costo_esperado <- predict(
  modelo_costo, newdata = perfiles_comparacion, type = "response"
)

perfiles_comparacion %>%
  select(perfil, costo_esperado) %>%
  print()
# Esta comparacion ilustra directamente como UCI, internacion y edad
# modifican el costo esperado, cumpliendo el requisito de "estimar el
# costo esperado bajo diferentes perfiles o condiciones".

## ---- 9. Costo promedio general para reportar ----

costo_promedio_reportar <- resumen_costo$media
costo_mediana_reportar  <- resumen_costo$mediana

cat("\nCosto promedio de una utilizacion asociada a la enfermedad:",
    round(costo_promedio_reportar, 2), "\n")
cat("Costo mediano (mas robusto ante colas largas):",
    round(costo_mediana_reportar, 2), "\n")
# Se recomienda reportar AMBOS: la media (para calculo de costo total
# esperado / presupuestal) y la mediana (para describir el "caso tipico",
# menos sensible a los outliers costosos).

df_benef_costo %>%
  filter(MEDIA_VALOR_PROCEDIMENTO > 30, MEDIA_VALOR_PROCEDIMENTO < 45) %>%
  count(Terapia,N_PROCEDIMENTOS,MEDIA_VALOR_PROCEDIMENTO) %>%
  arrange(desc(n))
## ---- 10. Limitaciones a documentar en el Markdown ----
## - El modelo Gamma asume una relacion multiplicativa entre predictoras
##   y costo; si hay interacciones fuertes (ej. UCI x edad), el modelo
##   simple puede subestimarlas -> considerar terminos de interaccion
##   si el MAPE es alto.
## - Los "outliers" de costo son casos reales de alta severidad; su
##   inclusion es correcta para el objetivo, pero aumenta la varianza
##   de las predicciones.
## - El costo esta influenciado por politicas de precios/tarifas del
##   prestador y la region (UF), lo que puede no generalizar a otros
##   periodos o localidades fuera de la muestra.
## - No se incluyen costos indirectos (perdida de productividad,
##   transporte, etc.), solo el valor facturado en VALOR_UTILIZACAO.

#---- MODELO POR DOS PARTES ----#
#1. Definir que es un "caso complejo" vs "caso simple"
# Ajusta el umbral segun lo que confirmes al inspeccionar el pico del histograma
# (ej. todo lo que no sea consulta simple de 1 procedimiento)

costo_modelo_2p <- costo_modelo %>%
  mutate(
    caso_complejo = case_when(
      Internacao > 0 | `Pronto Socorro` > 0 ~ "Si",
      Exame > pmax(Consulta,Outros,Terapia) ~ "Si",
      Terapia > pmax(Consulta,Outros)  ~ "No",   # tarifa estandarizada
      Consulta > pmax(Terapia,Outros)  ~ "No",
      T ~ "Si" # tarifa estandarizada
    ),
    caso_complejo = factor(caso_complejo, levels = c("No", "Si"))
  )


table(costo_modelo_2p$caso_complejo)
prop.table(table(costo_modelo_2p$caso_complejo))
## ---- Particion train/test (misma particion para ambas partes) ----

idx_train_2p <- sample(seq_len(nrow(costo_modelo_2p)), size = 0.75 * nrow(costo_modelo_2p))
train_2p <- costo_modelo_2p[idx_train_2p, ]
test_2p  <- costo_modelo_2p[-idx_train_2p, ]

## ---- PARTE 1: Clasificar caso_complejo ----

modelo_parte1 <- glm(
  caso_complejo ~ EDADE + SEXO_BENEFICIARIO + TIPO_BENEFICIARIO +
    TIPO_UNIDADE_PREST_HOSPITALAR + UF_CNES_PREST_HOSPITALAR +
    N_DIAGNOSTICOS_DIFERENTES,
  data   = train_2p,
  family = binomial
)

summary(modelo_parte1)

prob_complejo_test <- predict(modelo_parte1, newdata = test_2p, type = "response")

## ---- PARTE 2a: Costo esperado si es caso SIMPLE ----
## (probablemente casi constante - la tarifa fija que viste en el pico)

train_simple <- train_2p %>% filter(caso_complejo == "No")

costo_esperado_simple <- mean(train_simple$MEDIA_VALOR_PROCEDIMENTO)
cat("Costo esperado (caso simple):", round(costo_esperado_simple, 2), "\n")

# Si quieres algo mas fino que un promedio constante, un GLM simple
# con pocas variables tambien funciona:
modelo_parte2a <- glm(
  MEDIA_VALOR_PROCEDIMENTO ~ EDADE + SEXO_BENEFICIARIO + Terapia + Consulta,
  data = train_simple, family = Gamma(link = "log")
)

## ---- PARTE 2b: Costo esperado si es caso COMPLEJO ----
## (aqui SI vale la pena el modelo mas rico, porque ya es un subconjunto
## mas homogeneo en naturaleza -internacion, UCI, multiples procedimientos-)

train_complejo <- train_2p %>% filter(caso_complejo == "Si")

modelo_parte2b <- glm(
  MEDIA_VALOR_PROCEDIMENTO ~ EDADE + Internacao + `Pronto Socorro` +
    N_UTILIZACAO + TIPO_UNIDADE_PREST_HOSPITALAR + UF_CNES_PREST_HOSPITALAR,
  data    = train_complejo,
  family  = Gamma(link = "log"),
  control = glm.control(maxit = 100)
)

summary(modelo_parte2b)

## ---- Combinar ambas partes para predecir sobre TEST ----

# Prediccion condicional de cada parte
pred_costo_si_simple   <- predict(modelo_parte2a, newdata = test_2p, type = "response")
pred_costo_si_complejo <- predict(modelo_parte2b, newdata = test_2p, type = "response")

# Prediccion final = promedio ponderado por la probabilidad de cada rama
# E[costo] = P(complejo)*E[costo|complejo] + P(simple)*E[costo|simple]
pred_costo_2p <- prob_complejo_test * pred_costo_si_complejo +
  (1 - prob_complejo_test) * pred_costo_si_simple

## ---- Evaluar el modelo combinado ----

mae_2p  <- mean(abs(pred_costo_2p - test_2p$MEDIA_VALOR_PROCEDIMENTO))
rmse_2p <- sqrt(mean((pred_costo_2p - test_2p$MEDIA_VALOR_PROCEDIMENTO)^2))
r2_2p   <- cor(pred_costo_2p, test_2p$MEDIA_VALOR_PROCEDIMENTO)^2

cat("\n---- Metricas modelo en 2 partes (TEST) ----\n")
cat("MAE: ", round(mae_2p, 2), "\n")
cat("RMSE:", round(rmse_2p, 2), "\n")
cat("R2:  ", round(r2_2p, 4), "\n")

# Comparar directamente contra tu modelo de una sola parte
cat("\n--- Comparacion con modelo unico ---\n")
cat("MAE  - unico:", round(mae, 2), " | dos partes:", round(mae_2p, 2), "\n")
cat("RMSE - unico:", round(rmse, 2), " | dos partes:", round(rmse_2p, 2), "\n")
