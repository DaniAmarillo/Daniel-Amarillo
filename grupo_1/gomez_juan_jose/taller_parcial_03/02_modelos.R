
pkgs <- c("data.table","Matrix","glmnet","xgboost","pROC","PRROC")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(data.table); library(Matrix); library(glmnet)
library(xgboost);   library(pROC);   library(PRROC)

OUT <- "C:/Users/juanj/OneDrive/Desktop/salidas_taller3"
set.seed(2026)

benef <- readRDS(file.path(OUT, "datos_beneficiario.rds"))
y <- benef$y

feats_completo <- c("idade","sexo","uf","tipo_benef","unidade",
  "n_filas","n_util","n_proc_distintos","n_especialidades","n_exame",
  "n_intern_cet","n_consulta","n_pronto","n_terapia","n_outros",
  "any_uti","n_uti","any_internado","n_internado","flag_esp_resp","porte_max",
  "valor_total","valor_medio","valor_max","valor_sd")
feats_basal <- c("idade","sexo","uf","tipo_benef")   # solo perfil demográfico

slog <- function(x) sign(x) * log1p(abs(x))
transform_feats <- function(d) {
  d <- copy(d)
  valc <- intersect(c("valor_total","valor_medio","valor_max","valor_sd"), names(d))
  cntc <- grep("^n_", names(d), value = TRUE)
  for (c in valc) set(d, j = c, value = slog(d[[c]]))
  for (c in cntc) set(d, j = c, value = log1p(d[[c]]))
  d
}

build_X <- function(feats) {
  dd <- transform_feats(benef[, ..feats])
  sparse.model.matrix(~ . - 1, data = as.data.frame(dd))
}
Xc <- build_X(feats_completo)
Xb <- build_X(feats_basal)
cat("Dim matriz completa:", dim(Xc), "| basal:", dim(Xb), "\n")

ipos <- which(y == 1); ineg <- which(y == 0)
trp <- sample(ipos, floor(0.7 * length(ipos)))
trn <- sample(ineg, floor(0.7 * length(ineg)))
tr  <- c(trp, trn); te <- setdiff(seq_along(y), tr)
cat("Train: pos =", length(trp), " neg =", length(trn),
    "| Test: pos =", length(setdiff(ipos, trp)),
    " neg =", length(setdiff(ineg, trn)), "\n")

f1_at <- function(yt, pred) {
  tp <- sum(yt == 1 & pred == 1); fp <- sum(yt == 0 & pred == 1)
  fn <- sum(yt == 1 & pred == 0)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
}
eval_metrics <- function(yt, p) {
  roc_auc <- as.numeric(pROC::auc(pROC::roc(yt, p, quiet = TRUE,
                                            levels = c(0, 1), direction = "<")))
  pr_auc  <- PRROC::pr.curve(scores.class0 = p[yt == 1],
                             scores.class1 = p[yt == 0])$auc.integral
  grid <- unique(quantile(p, probs = seq(0.50, 0.9999, length.out = 300)))
  f1s  <- vapply(grid, function(t) f1_at(yt, as.integer(p >= t)), numeric(1))
  bi <- which.max(f1s); thr <- grid[bi]; pred <- as.integer(p >= thr)
  tp <- sum(yt == 1 & pred == 1); fp <- sum(yt == 0 & pred == 1)
  fn <- sum(yt == 1 & pred == 0); tn <- sum(yt == 0 & pred == 0)
  data.table(ROC_AUC = roc_auc, PR_AUC = pr_auc, F1 = f1s[bi], umbral = thr,
             sens = tp/(tp+fn), esp = tn/(tn+fp),
             prec = if ((tp+fp) > 0) tp/(tp+fp) else 0,
             TP = tp, FP = fp, FN = fn, TN = tn)
}

sub_undersample <- function(train_idx, ratio = 10) {
  p1 <- train_idx[y[train_idx] == 1]
  p0 <- train_idx[y[train_idx] == 0]
  c(p1, sample(p0, min(length(p0), ratio * length(p1))))
}
fit_lr <- function(X, strategy) {
  if (strategy == "weighted") {
    w <- ifelse(y[tr] == 1, sum(y[tr] == 0) / sum(y[tr] == 1), 1)
    cvf <- cv.glmnet(X[tr, ], y[tr], family = "binomial", weights = w,
                     type.measure = "deviance", nfolds = 5)
    idx <- tr
  } else {
    idx <- sub_undersample(tr)
    cvf <- cv.glmnet(X[idx, ], y[idx], family = "binomial",
                     type.measure = "deviance", nfolds = 5)
  }
  p <- as.numeric(predict(cvf, newx = X[te, ], s = "lambda.min", type = "response"))
  list(p = p, model = cvf)
}
fit_xgb <- function(X, strategy) {
  if (strategy == "weighted") {
    idx <- tr; spw <- sum(y[tr] == 0) / sum(y[tr] == 1)
  } else {
    idx <- sub_undersample(tr); spw <- 1
  }
  dtr <- xgb.DMatrix(data = X[idx, ], label = y[idx])
  prm <- list(objective = "binary:logistic", eval_metric = "aucpr",
              max_depth = 5, eta = 0.1, subsample = 0.8,
              colsample_bytree = 0.8, scale_pos_weight = spw)
  bst <- xgb.train(prm, dtr, nrounds = 150, verbose = 0)
  p <- predict(bst, X[te, ])
  list(p = p, model = bst)
}

yte <- y[te]
combos <- expand.grid(featset = c("completo","basal"),
                      algo = c("logistica","xgboost"),
                      balance = c("weighted","undersample"),
                      stringsAsFactors = FALSE)
res <- list()
for (i in seq_len(nrow(combos))) {
  cc <- combos[i, ]
  X <- if (cc$featset == "completo") Xc else Xb
  fit <- if (cc$algo == "logistica") fit_lr(X, cc$balance) else fit_xgb(X, cc$balance)
  m <- eval_metrics(yte, fit$p)
  m[, `:=`(featset = cc$featset, algo = cc$algo, balance = cc$balance)]
  res[[i]] <- m
  cat(sprintf("[%s | %s | %s]  ROC-AUC=%.4f  PR-AUC=%.4f  F1=%.3f\n",
              cc$featset, cc$algo, cc$balance, m$ROC_AUC, m$PR_AUC, m$F1))
}
metrics_df <- rbindlist(res)
setcolorder(metrics_df, c("featset","algo","balance","ROC_AUC","PR_AUC","F1",
                          "sens","esp","prec","umbral","TP","FP","FN","TN"))
fwrite(metrics_df, file.path(OUT, "metricas_modelos.csv"))
cat("\n==== TABLA DE MÉTRICAS ====\n"); print(metrics_df)

main_xgb <- fit_xgb(Xc, "weighted")
imp <- xgb.importance(model = main_xgb$model)
imp <- head(imp[order(-Gain)], 20)
fwrite(imp, file.path(OUT, "importancia_xgb.csv"))
cat("\n==== IMPORTANCIA XGBOOST (top 15) ====\n"); print(head(imp, 15))

main_lr <- fit_lr(Xc, "weighted")
co <- as.matrix(coef(main_lr$model, s = "lambda.min"))
co <- data.table(variable = rownames(co), coef = co[, 1])
co <- co[variable != "(Intercept)" & coef != 0][order(-abs(coef))]
fwrite(co, file.path(OUT, "coeficientes_glmnet.csv"))
cat("\n==== COEFICIENTES GLMNET (top 15 por |coef|) ====\n"); print(head(co, 15))

p_main <- main_xgb$p
mm <- eval_metrics(yte, p_main); thr <- mm$umbral
pred <- as.integer(p_main >= thr)
bt <- benef[te]; bt[, pred := pred]; bt[, real := yte]
perfil <- bt[, .(
  n = .N, idade = mean(idade), any_internado = mean(any_internado),
  any_uti = mean(any_uti), valor_total = mean(valor_total),
  n_internado = mean(n_internado), n_filas = mean(n_filas)
), by = .(grupo = fifelse(real == 1 & pred == 1, "TP",
                  fifelse(real == 0 & pred == 1, "FP",
                  fifelse(real == 1 & pred == 0, "FN", "TN"))))]
cat("\n==== PERFIL DE ERRORES (FP vs TP vs FN) ====\n")
print(perfil[grupo != "TN"])
fwrite(perfil, file.path(OUT, "perfil_errores.csv"))

p_c <- fit_xgb(Xc, "weighted")$p
p_b <- fit_xgb(Xb, "weighted")$p
roc_c <- roc(yte, p_c, quiet = TRUE, levels = c(0,1), direction = "<")
roc_b <- roc(yte, p_b, quiet = TRUE, levels = c(0,1), direction = "<")
png(file.path(OUT, "roc_completo_vs_basal.png"), width = 800, height = 700, res = 110)
plot(roc_c, col = "#6A1B9A", lwd = 2, main = "ROC: modelo completo vs basal (XGBoost)")
plot(roc_b, col = "#F57C00", lwd = 2, add = TRUE)
legend("bottomright", c(sprintf("Completo (AUC=%.3f)", as.numeric(auc(roc_c))),
                        sprintf("Basal (AUC=%.3f)",  as.numeric(auc(roc_b)))),
       col = c("#6A1B9A","#F57C00"), lwd = 2, bty = "n")
dev.off()

pr_c <- pr.curve(scores.class0 = p_c[yte == 1], scores.class1 = p_c[yte == 0], curve = TRUE)
png(file.path(OUT, "pr_completo.png"), width = 800, height = 700, res = 110)
plot(pr_c, main = "Curva Precision-Recall (modelo completo)", col = "#6A1B9A")
dev.off()

png(file.path(OUT, "importancia_xgb.png"), width = 900, height = 700, res = 110)
xgb.plot.importance(imp, top_n = 15, measure = "Gain",
                    main = "Importancia de variables (XGBoost, Gain)")
dev.off()

saveRDS(list(metrics = metrics_df, importancia = imp, coeficientes = co,
             perfil_errores = perfil,
             auc_completo = as.numeric(auc(roc_c)),
             auc_basal = as.numeric(auc(roc_b)),
             split = list(train_pos = length(trp), test_pos = length(setdiff(ipos, trp)))),
        file.path(OUT, "resultados_modelos.rds"))

