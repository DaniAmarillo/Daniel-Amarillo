library(tabpfn)
set_tabpfn_access_token("tabpfn_sk_S_nbZ-Smu9VROocxAkoOFpTsBKmiqUb5yPfHEgnPrCM") 

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
df  <- read.csv(url, header = FALSE)


colnames(df)[2] <- "Diagnosis"

X <- df[, 3:32]                              
y <- ifelse(df$Diagnosis == "M", 1L, 0L)     


set.seed(42)
idx     <- sample(seq_len(nrow(X)), size = floor(0.8 * nrow(X)))
X_train <- X[idx, ]; y_train <- y[idx]
X_test  <- X[-idx, ]; y_test  <- y[-idx]

  clf <- TabPFNClassifier$new()
  clf$fit(X_train, y_train)
  
  pred <- clf$predict(X_test)
  
  cm <- table(Real = y_test, Predicho = pred)
  print(cm)
  
  # Métricas a mano
  TN <- cm["0","0"]; FP <- cm["0","1"]
  FN <- cm["1","0"]; TP <- cm["1","1"]
  
  accuracy    <- (TP + TN) / sum(cm)
  sensibilidad <- TP / (TP + FN)   # recall de malignos
  especificidad<- TN / (TN + FP)
  precision    <- TP / (TP + FP)
  
  cat(sprintf("Accuracy: %.3f | Sens: %.3f | Espec: %.3f | Prec: %.3f\n",
              accuracy, sensibilidad, especificidad, precision))
  
  