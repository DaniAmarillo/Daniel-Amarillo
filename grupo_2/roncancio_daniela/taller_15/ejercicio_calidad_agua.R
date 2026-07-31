###############################################################################
# Ejercicio de practica: Calidad de agua en Colombia
# Arbol, Random Forest, XGBoost, Early Stopping, Feature Importance y Stacking
###############################################################################

# install.packages(c("rpart","randomForest","xgboost","caret","pROC","e1071"))

library(rpart)
library(randomForest)
library(xgboost)
library(caret)
library(pROC)

###############################################################################
# 0. Datos
###############################################################################

set.seed(42); n <- 600

df <- data.frame(
  turbiedad  = rexp(n, 0.1),
  pH         = rnorm(n, 7.2, 0.8),
  coliformes = rpois(n, 80),
  DBO        = rexp(n, 0.2),
  poblacion  = sample(5000:500000, n, replace = TRUE),
  altitud    = runif(n, 100, 3000)
)

df$apta <- as.integer(
  df$turbiedad < 5 & df$pH > 6.5 & df$pH < 8.5 &
  df$coliformes < 100 & df$DBO < 5
)

# Convertir a factor para clasificacion ("Apta" = clase positiva)
df$apta <- factor(ifelse(df$apta == 1, "Apta", "NoApta"),
                   levels = c("NoApta", "Apta"))

cat("Distribucion de la variable objetivo:\n")
print(table(df$apta))
print(prop.table(table(df$apta)))

# Particion 80/20 para evaluacion final (puntos b, c y d)
set.seed(123)
idx_train <- sample(seq_len(n), size = 0.8 * n)
train <- df[idx_train, ]
test  <- df[-idx_train, ]

p <- ncol(train) - 1
mtry_val <- floor(sqrt(p))

###############################################################################
# (a) Arbol, Random Forest y XGBoost - CV de 5 pliegues
#     Comparar accuracy, F1 y AUC
###############################################################################

ctrl_cv <- trainControl(method = "cv", number = 5,
                         classProbs = TRUE,
                         savePredictions = "final")

set.seed(1)
modelo_arbol_cv <- train(apta ~ ., data = df,
                          method = "rpart2",
                          tuneGrid = data.frame(maxdepth = 5),
                          trControl = ctrl_cv, metric = "Accuracy")

set.seed(1)
modelo_rf_cv <- train(apta ~ ., data = df,
                       method = "rf",
                       tuneGrid = data.frame(mtry = mtry_val),
                       ntree = 200,
                       trControl = ctrl_cv, metric = "Accuracy")

# F1 calculado directamente desde la matriz de confusion (mas robusto)
f1_score <- function(pred, obs, positivo = "Apta") {
  tab <- table(Pred = pred, Obs = obs)
  TP <- tab[positivo, positivo]
  FP <- sum(tab[positivo, ]) - TP
  FN <- sum(tab[, positivo]) - TP

  precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
  recall    <- if ((TP + FN) == 0) NA else TP / (TP + FN)

  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    return(NA)
  }
  2 * precision * recall / (precision + recall)
}

# Calcula Accuracy, F1 y AUC promedio por pliegue a partir de $pred (caret)
calcular_metricas <- function(modelo) {
  preds <- modelo$pred
  folds <- unique(preds$Resample)

  res <- t(sapply(folds, function(f) {
    d <- preds[preds$Resample == f, ]

    acc <- mean(d$pred == d$obs)
    f1  <- f1_score(d$pred, d$obs)

    roc_obj <- pROC::roc(d$obs, d$Apta,
                          levels = c("NoApta", "Apta"),
                          direction = "<", quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))

    c(Accuracy = acc, F1 = f1, AUC = auc_val)
  }))

  colMeans(res, na.rm = TRUE)
}

met_arbol <- calcular_metricas(modelo_arbol_cv)
met_rf    <- calcular_metricas(modelo_rf_cv)

# ---- XGBoost: CV manual de 5 pliegues (sin pasar por caret) ----
predictores <- c("turbiedad", "pH", "coliformes", "DBO", "poblacion", "altitud")
x_all <- as.matrix(df[, predictores])
y_all <- ifelse(df$apta == "Apta", 1, 0)

set.seed(1)
folds_xgb <- createFolds(df$apta, k = 5, list = TRUE)

metricas_xgb <- t(sapply(folds_xgb, function(idx_test) {
  dtr <- xgb.DMatrix(data = x_all[-idx_test, ], label = y_all[-idx_test])
  dte <- xgb.DMatrix(data = x_all[idx_test, ],  label = y_all[idx_test])

  modelo <- xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "auc",
                   max_depth = 4, eta = 0.1, gamma = 0,
                   colsample_bytree = 0.8, min_child_weight = 1,
                   subsample = 0.8),
    data = dtr, nrounds = 100, verbose = 0)

  prob <- predict(modelo, dte)
  pred <- factor(ifelse(prob > 0.5, "Apta", "NoApta"),
                  levels = c("NoApta", "Apta"))
  obs  <- factor(ifelse(y_all[idx_test] == 1, "Apta", "NoApta"),
                  levels = c("NoApta", "Apta"))

  acc <- mean(pred == obs)
  f1  <- f1_score(pred, obs)

  roc_obj <- pROC::roc(obs, prob, levels = c("NoApta", "Apta"),
                        direction = "<", quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))

  c(Accuracy = acc, F1 = f1, AUC = auc_val)
}))

met_xgb <- colMeans(metricas_xgb, na.rm = TRUE)

resultados_cv <- data.frame(
  Modelo   = c("Arbol (maxdepth=5)", "Random Forest", "XGBoost"),
  Accuracy = c(met_arbol["Accuracy"], met_rf["Accuracy"], met_xgb["Accuracy"]),
  F1       = c(met_arbol["F1"],       met_rf["F1"],       met_xgb["F1"]),
  AUC      = c(met_arbol["AUC"],      met_rf["AUC"],      met_xgb["AUC"])
)

cat("\n=== (a) Comparacion CV de 5 pliegues ===\n")
print(resultados_cv)

###############################################################################
# (b) Early stopping en XGBoost (20 rondas de paciencia)
###############################################################################

x_train_full <- as.matrix(train[, predictores])
x_test       <- as.matrix(test[, predictores])

y_train_full <- ifelse(train$apta == "Apta", 1, 0)
y_test       <- ifelse(test$apta  == "Apta", 1, 0)

# Dividir el entrenamiento en train2 (80%) y validacion (20%) para early stopping
set.seed(456)
idx_tr2 <- sample(seq_len(nrow(x_train_full)), size = 0.8 * nrow(x_train_full))

dtrain <- xgb.DMatrix(data = x_train_full[idx_tr2, ],  label = y_train_full[idx_tr2])
dval   <- xgb.DMatrix(data = x_train_full[-idx_tr2, ], label = y_train_full[-idx_tr2])
dtest  <- xgb.DMatrix(data = x_test, label = y_test)

set.seed(456)
xgb_early <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc",
                 max_depth = 4, eta = 0.1, subsample = 0.8,
                 colsample_bytree = 0.8),
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, validacion = dval),
  early_stopping_rounds = 20,
  verbose = 0
)

cat("\n=== (b) Early stopping en XGBoost ===\n")

best_iter  <- as.numeric(xgb.attr(xgb_early, "best_iteration"))
best_score <- as.numeric(xgb.attr(xgb_early, "best_score"))

# best_iteration es 0-indexado -> sumamos 1 para obtener el numero de arboles
cat("Numero optimo de arboles:", best_iter + 1, "\n")
cat("Mejor AUC en validacion :", best_score, "\n")

###############################################################################
# (c) Feature importance: RF (impureza) vs XGBoost (ganancia)
###############################################################################

set.seed(123)
rf_final <- randomForest(apta ~ ., data = train,
                          ntree = 200, mtry = mtry_val, importance = TRUE)

imp_rf <- importance(rf_final)
imp_rf_ordenada <- imp_rf[order(-imp_rf[, "MeanDecreaseGini"]), ]

imp_xgb <- xgb.importance(model = xgb_early, feature_names = predictores)

cat("\n=== (c) Importancia de variables - Random Forest (MeanDecreaseGini) ===\n")
print(imp_rf_ordenada)

cat("\n=== (c) Importancia de variables - XGBoost (Gain) ===\n")
print(imp_xgb)

cat("\nTop variables RF:", paste(rownames(imp_rf_ordenada)[1:3], collapse = ", "), "\n")
cat("Top variables XGBoost:", paste(imp_xgb$Feature[1:3], collapse = ", "), "\n")

###############################################################################
# (d) Stacking: Arbol + RF (modelos base) -> Regresion logistica (meta-modelo)
###############################################################################

ctrl_oof <- trainControl(method = "cv", number = 5,
                          classProbs = TRUE,
                          savePredictions = "final")

set.seed(1)
base_arbol <- train(apta ~ ., data = train,
                     method = "rpart2",
                     tuneGrid = data.frame(maxdepth = 5),
                     trControl = ctrl_oof, metric = "Accuracy")

set.seed(1)
base_rf <- train(apta ~ ., data = train,
                  method = "rf",
                  tuneGrid = data.frame(mtry = mtry_val),
                  ntree = 200,
                  trControl = ctrl_oof, metric = "Accuracy")

oof_arbol <- base_arbol$pred[order(base_arbol$pred$rowIndex), ]
oof_rf    <- base_rf$pred[order(base_rf$pred$rowIndex), ]

meta_train <- data.frame(
  prob_arbol = oof_arbol$Apta,
  prob_rf    = oof_rf$Apta,
  apta       = train$apta[oof_arbol$rowIndex]
)

meta_modelo <- glm(apta ~ prob_arbol + prob_rf,
                    data = meta_train, family = binomial)

# Entrenar modelos base con TODO el train, predecir sobre 'test'
set.seed(123)
arbol_full <- rpart(apta ~ ., data = train, method = "class",
                     control = rpart.control(maxdepth = 5, cp = 0))
prob_arbol_test <- predict(arbol_full, test, type = "prob")[, "Apta"]
prob_rf_test    <- predict(rf_final, test, type = "prob")[, "Apta"]

meta_test <- data.frame(prob_arbol = prob_arbol_test,
                         prob_rf    = prob_rf_test)

prob_stack_test <- predict(meta_modelo, meta_test, type = "response")
pred_stack_test <- factor(ifelse(prob_stack_test > 0.5, "Apta", "NoApta"),
                           levels = c("NoApta", "Apta"))

pred_arbol_test <- predict(arbol_full, test, type = "class")
pred_rf_test    <- predict(rf_final, test, type = "class")

acc_arbol_test <- mean(pred_arbol_test == test$apta)
acc_rf_test    <- mean(pred_rf_test    == test$apta)
acc_stack_test <- mean(pred_stack_test == test$apta)

resultados_stack <- data.frame(
  Modelo   = c("Arbol", "Random Forest", "Stacking (Arbol+RF -> LogReg)"),
  Accuracy_Test = c(acc_arbol_test, acc_rf_test, acc_stack_test)
)

cat("\n=== (d) Stacking vs modelos individuales (test) ===\n")
print(resultados_stack)
cat("\nCoeficientes del meta-modelo (regresion logistica):\n")
print(coef(meta_modelo))
