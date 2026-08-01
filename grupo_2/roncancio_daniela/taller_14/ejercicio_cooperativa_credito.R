
####################################################################################
# Ejercicio de práctica: Riesgo de mora - Cooperativa de crédito (Robledo, Medellín)
####################################################################################

install.packages(c("rpart", "rpart.plot", "randomForest", "caret"))

library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

###############################################################################
# Generación de los datos sintéticos
###############################################################################

set.seed(42)
n <- 800

cooperativa <- data.frame(
  ingreso     = round(runif(n, 800000, 8000000)),
  edad        = sample(20:70, n, replace = TRUE),
  historial   = sample(c("bueno", "regular", "malo"), n,
                       replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  estrato     = sample(1:6, n, replace = TRUE),
  tipo_empleo = sample(c("formal", "informal"), n,
                       replace = TRUE, prob = c(0.6, 0.4)),
  monto       = round(runif(n, 500000, 20000000))
)

# Variable objetivo: mora (Si = incumplió, No = al día)

prob_mora <- with(cooperativa,
                  0.6 * (historial == "malo") +
                    0.3 * (historial == "regular") +
                    0.2 * (tipo_empleo == "informal") -
                    0.1 * (ingreso / 8000000))

cooperativa$mora <- factor(ifelse(runif(n) < prob_mora, "Si", "No"))

# Convertir variables categóricas a factor
cooperativa$historial   <- factor(cooperativa$historial,
                                  levels = c("malo", "regular", "bueno"))
cooperativa$tipo_empleo <- factor(cooperativa$tipo_empleo)
cooperativa$estrato     <- factor(cooperativa$estrato)

str(cooperativa)
table(cooperativa$mora)

###############################################################################
# División 80% entrenamiento / 20% prueba
###############################################################################

set.seed(123)
idx_train <- sample(seq_len(n), size = 0.8 * n)

train <- cooperativa[idx_train, ]
test  <- cooperativa[-idx_train, ]

cat("Tamaño entrenamiento:", nrow(train), "\n")
cat("Tamaño prueba       :", nrow(test), "\n")

###############################################################################
# 1. Árbol de decisión con max_depth = 5
###############################################################################

arbol <- rpart(mora ~ ingreso + edad + historial + estrato +
                 tipo_empleo + monto,
               data = train, method = "class",
               control = rpart.control(maxdepth = 5, cp = 0))

rpart.plot(arbol, main = "Árbol de decisión (max_depth = 5)")

pred_arbol <- predict(arbol, test, type = "class")
acc_arbol <- mean(pred_arbol == test$mora)

cat("Accuracy del árbol en test:", round(acc_arbol, 4), "\n")

###############################################################################
# 2. Random Forest (ntree = 200, mtry = sqrt(p))
###############################################################################

p <- ncol(train) - 1  # número de predictores (sin contar 'mora')
mtry_val <- floor(sqrt(p))
cat("p =", p, " -> mtry =", mtry_val, "\n")

set.seed(123)
rf <- randomForest(mora ~ ingreso + edad + historial + estrato +
                     tipo_empleo + monto,
                   data = train,
                   ntree = 200,
                   mtry = mtry_val,
                   importance = TRUE)

print(rf)

# Error OOB (Out-Of-Bag) del modelo final
oob_error <- rf$err.rate[nrow(rf$err.rate), "OOB"]
cat("Error OOB:", round(oob_error, 4), "\n")

pred_rf <- predict(rf, test, type = "class")
acc_rf <- mean(pred_rf == test$mora)

cat("Accuracy del Random Forest en test:", round(acc_rf, 4), "\n")

###############################################################################
# 3. Comparación: árbol vs Random Forest
###############################################################################

comparacion <- data.frame(
  Modelo = c("Arbol (max_depth=5)", "Random Forest (ntree=200)"),
  Accuracy_Test = c(acc_arbol, acc_rf)
)

print(comparacion)

mejora <- acc_rf - acc_arbol
cat("Mejora del ensemble sobre el árbol individual:",
    round(mejora, 4), "\n")

###############################################################################
# 4. Importancia de variables (Random Forest)
###############################################################################

varImpPlot(rf, main = "Importancia de variables - Random Forest")

importancia <- importance(rf)
importancia_ordenada <- importancia[order(-importancia[, "MeanDecreaseGini"]), ]
print(importancia_ordenada)

###############################################################################
# 5. Interpretación en el contexto colombiano
###############################################################################

# En este ejercicio, la variable de mayor peso suele ser el historial
# crediticio (bueno/regular/malo), seguida del tipo de empleo
# (formal/informal) y el ingreso mensual. Esto es razonable en el contexto
# Colombiano:
  
#  - El historial crediticio es, en cualquier sistema financiero, el
# predictor mas directo del comportamiento futuro de pago: alguien que
# ha incumplido antes tiene mayor probabilidad de volver a hacerlo.

# - El tipo de empleo (formal vs informal) es un factor critico en
# Colombia, donde una proporcion muy alta de la poblacion economicamente
# activa trabaja en la informalidad. Un ingreso informal implica mayor
# variabilidad en los flujos de caja, lo que se traduce en mayor riesgo
# de mora.

# - El ingreso mensual determina directamente la capacidad de pago: a
# mayor ingreso, menor proporcion del ingreso destinada a la cuota del
# crédito y por tanto menor probabilidad de caer en mora.

# En contraste, variables como edad o estrato socioeconomico suelen tener
# menor importancia, ya que no capturan directamente la estabilidad del
# ingreso ni el comportamiento de pago previo, que son los factores mas
# predictivos en la práctica del analisis de crédito en Colombia.

