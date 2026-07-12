# ==============================================================================
#  02_modelado_costos.R
#  Modelado predictivo, interpretacion, estimacion de costos y calibracion
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
  library(ranger)
  library(glmnet)
  library(probably)
  library(dcurves)
  library(broom)
})
tidymodels_prefer()

SEED <- 2026
set.seed(SEED)
NIVEL_EVENTO <- "second"  

RUTA <- list(datos = "data", figuras = "figuras",
             modelos = "modelos", resultados = "resultados")

guardar_tabla <- function(df, nombre) {
  write_csv(df, file.path(RUTA$resultados, paste0(nombre, ".csv")))
  invisible(df)
}
guardar_fig <- function(plot, nombre, w = 8, h = 5) {
  ggsave(file.path(RUTA$figuras, paste0(nombre, ".png")),
         plot, width = w, height = h, dpi = 300, bg = "white")
  invisible(plot)
}


# ==============================================================================
#  1. STEP PERSONALIZADO: WINSORIZACION SIN FUGA
#  El umbral p99 se aprende en prep() (train) y se aplica en bake() (test).
# ==============================================================================
step_winsorize <- function(recipe, ..., role = NA, trained = FALSE,
                           probs = 0.99, upper = NULL,
                           skip = FALSE, id = recipes::rand_id("winsorize")) {
  recipes::add_step(recipe, step_winsorize_new(
    terms = rlang::enquos(...), role = role, trained = trained,
    probs = probs, upper = upper, skip = skip, id = id))
}
step_winsorize_new <- function(terms, role, trained, probs, upper, skip, id) {
  recipes::step(subclass = "winsorize", terms = terms, role = role,
                trained = trained, probs = probs, upper = upper,
                skip = skip, id = id)
}
prep.step_winsorize <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  upper <- vapply(training[col_names],
                  function(col) stats::quantile(col, probs = x$probs,
                                                na.rm = TRUE, names = FALSE),
                  FUN.VALUE = numeric(1))
  names(upper) <- col_names
  step_winsorize_new(terms = x$terms, role = x$role, trained = TRUE,
                     probs = x$probs, upper = upper, skip = x$skip, id = x$id)
}
bake.step_winsorize <- function(object, new_data, ...) {
  for (nm in names(object$upper)) {
    if (nm %in% names(new_data)) new_data[[nm]] <- pmin(new_data[[nm]], object$upper[[nm]])
  }
  tibble::as_tibble(new_data)
}
print.step_winsorize <- function(x, ...) {
  cat("Winsorizaci\u00f3n p", x$probs * 100, "\n", sep = ""); invisible(x)
}


# ==============================================================================
#  2. PARTICION Y RECETA
# ==============================================================================
# Num_Utilizaciones es identica a Dias_Utilizacion (r = 1.00): se descarta.
datos <- readRDS(file.path(RUTA$resultados, "datos_modelo.rds")) %>%
  select(-Num_Utilizaciones)

set.seed(SEED)
split_i50 <- initial_split(datos, prop = 0.80, strata = I50)
train <- training(split_i50)
test  <- testing(split_i50)

set.seed(SEED)
folds <- vfold_cv(train, v = 10, strata = I50)

metricas <- metric_set(pr_auc, roc_auc, f_meas, sensitivity,
                       specificity, accuracy)

construir_receta <- function(df, normalizar = FALSE) {
  rec <- recipe(I50 ~ ., data = df)
  if ("CHAVE_FUNCIONAL" %in% names(df))
    rec <- update_role(rec, CHAVE_FUNCIONAL, new_role = "id")

  rec <- rec %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors(), new_level = "No_Registrado") %>%
    step_other(any_of(c("Tipo_Beneficiario", "UF_Principal")),
               threshold = 0.01, other = "OTROS") %>%
    step_winsorize(any_of(c("Costo_Total", "Costo_Promedio_Dia")), probs = 0.99) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())

  if (normalizar) rec <- rec %>% step_normalize(all_numeric_predictors())
  rec
}


# ==============================================================================
#  3. TRES MODELOS Y COMPARACION POR VALIDACION CRUZADA
# ==============================================================================
peso_pos <- sum(train$I50 == "Sano") / sum(train$I50 == "Enfermo")

spec_log <- logistic_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine("glmnet") %>% set_mode("classification")

spec_rf <- rand_forest(trees = 500, min_n = 10) %>%
  set_engine("ranger", importance = "impurity", num.threads = 1, seed = SEED,
             class.weights = c(Sano = 1, Enfermo = peso_pos),
             probability = TRUE) %>%
  set_mode("classification")

spec_xgb <- boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.05,
                       min_n = 10) %>%
  set_engine("xgboost", scale_pos_weight = peso_pos,
             eval_metric = "aucpr", nthread = 1) %>%
  set_mode("classification")

wf_log <- workflow() %>% add_recipe(construir_receta(train, TRUE))  %>% add_model(spec_log)
wf_rf  <- workflow() %>% add_recipe(construir_receta(train, FALSE)) %>% add_model(spec_rf)
wf_xgb <- workflow() %>% add_recipe(construir_receta(train, FALSE)) %>% add_model(spec_xgb)

modelos <- as_workflow_set(logistica = wf_log, random_forest = wf_rf,
                           xgboost = wf_xgb)

set.seed(SEED)
res_cv <- modelos %>%
  workflow_map("fit_resamples", resamples = folds, metrics = metricas,
               control = control_resamples(save_pred = TRUE,
                                           event_level = NIVEL_EVENTO),
               seed = SEED, verbose = TRUE)

tabla_comparacion <- rank_results(res_cv, rank_metric = "pr_auc", select_best = TRUE)
guardar_tabla(tabla_comparacion, "modelo_comparacion_cv")
guardar_fig(autoplot(res_cv, metric = "pr_auc") +
              labs(title = "Comparaci\u00f3n de modelos por PR-AUC (CV 10-fold)"),
            "12_comparacion_modelos")


# ==============================================================================
#  4. MODELO FINAL (XGBOOST) Y EVALUACION EN TEST
# ==============================================================================
set.seed(SEED)
fit_xgb   <- fit(wf_xgb, data = train)
pred_test <- augment(fit_xgb, new_data = test)

metricas_test <- pred_test %>%
  metricas(truth = I50, .pred_Enfermo, estimate = .pred_class,
           event_level = NIVEL_EVENTO)
guardar_tabla(metricas_test, "modelo_metricas_test")

print(conf_mat(pred_test, truth = I50, estimate = .pred_class))

# El umbral 0.5 es inservible: con prevalencia de 0.016% las probabilidades son
# diminutas. Se busca el optimo sobre la curva ROC (indice de Youden).
barrido <- pred_test %>%
  roc_curve(truth = I50, .pred_Enfermo, event_level = NIVEL_EVENTO) %>%
  mutate(j_index = sensitivity + specificity - 1)
umbral_opt <- barrido %>% slice_max(j_index, n = 1) %>% pull(.threshold)
guardar_tabla(barrido, "modelo_barrido_umbral")
cat("Umbral \u00f3ptimo (Youden):", umbral_opt, "\n")


# ==============================================================================
#  5. MODELO PARSIMONIOSO (prueba de robustez)
# ==============================================================================
vars_acopladas <- c("Num_Hospitalizaciones", "Dias_Internado",
                    "Dias_UTI", "Num_Procedimientos")
train_par <- train %>% select(-all_of(vars_acopladas))
test_par  <- test  %>% select(-all_of(vars_acopladas))

wf_par <- workflow() %>%
  add_recipe(construir_receta(train_par, FALSE)) %>%
  add_model(spec_xgb)

set.seed(SEED)
res_par <- fit_resamples(wf_par,
                         resamples = vfold_cv(train_par, v = 10, strata = I50),
                         metrics = metricas,
                         control = control_resamples(event_level = NIVEL_EVENTO))
guardar_tabla(collect_metrics(res_par), "modelo_parsimonioso_cv")

set.seed(SEED)
fit_par  <- fit(wf_par, data = train_par)
pred_par <- augment(fit_par, new_data = test_par)


# ==============================================================================
#  6. INTERPRETACION
# ==============================================================================
booster     <- extract_fit_engine(fit_xgb)
receta_prep <- extract_recipe(fit_xgb)

set.seed(SEED)
train_muestra <- bind_rows(
  filter(train, I50 == "Enfermo"),
  slice_sample(filter(train, I50 == "Sano"), n = 8000)
)

# ---- 6.1 Importancia por ganancia ----
tabla_importancia <- xgb.importance(model = booster) %>%
  as_tibble() %>%
  transmute(variable = Feature, ganancia = Gain) %>%
  arrange(desc(ganancia))
guardar_tabla(tabla_importancia, "interp_importancia_ganancia")

fig_imp <- tabla_importancia %>%
  slice_head(n = 15) %>%
  mutate(variable = fct_reorder(variable, ganancia)) %>%
  ggplot(aes(ganancia, variable)) +
  geom_col(fill = "#4C72B0", alpha = 0.85) +
  labs(title = "Importancia de variables (ganancia XGBoost)",
       x = "Ganancia relativa", y = NULL)
guardar_fig(fig_imp, "13_importancia_ganancia", h = 6)

# ---- 6.2 Coeficientes de la logistica (direccion del efecto) ----
fit_log <- workflow() %>%
  add_recipe(construir_receta(train, TRUE)) %>%
  add_model(spec_log) %>%
  fit(data = train)

coefs <- tidy(fit_log) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(odds_ratio = exp(estimate)) %>%
  arrange(desc(abs(estimate)))
guardar_tabla(coefs, "interp_coeficientes_logistica")

# ---- 6.3 PDP manual sobre variables de interes clinico ----
pdp_manual <- function(fit, datos, variable, grid_n = 25) {
  if (!variable %in% names(datos)) return(NULL)
  rejilla <- quantile(datos[[variable]],
                      probs = seq(0.01, 0.99, length.out = grid_n),
                      na.rm = TRUE) %>% unique()
  map_dfr(rejilla, function(v) {
    d <- datos; d[[variable]] <- v
    p <- predict(fit, new_data = d, type = "prob")[[".pred_Enfermo"]]
    tibble(valor = v, prob_media = mean(p, na.rm = TRUE))
  })
}

for (v in c("Costo_Total", "Edad", "Visitas_Cardiologia")) {
  pdp_df <- pdp_manual(fit_xgb, train_muestra, v)
  if (!is.null(pdp_df)) {
    g <- ggplot(pdp_df, aes(valor, prob_media)) +
      geom_line(color = "#C44E52", linewidth = 1) +
      geom_point(color = "#C44E52", size = 1.5) +
      labs(title = paste("PDP:", v), x = v, y = "P(I50) media predicha")
    guardar_fig(g, paste0("15_pdp_", v), w = 6, h = 4)
  }
}

# ---- 6.4 Analisis de errores ----
errores <- pred_test %>%
  mutate(
    pred = if_else(.pred_Enfermo >= umbral_opt, "Enfermo", "Sano"),
    tipo = case_when(
      I50 == "Enfermo" & pred == "Enfermo" ~ "VP",
      I50 == "Sano"    & pred == "Enfermo" ~ "FP",
      I50 == "Enfermo" & pred == "Sano"    ~ "FN",
      TRUE                                 ~ "VN"
    )
  )
print(count(errores, tipo))

perfil_errores <- errores %>%
  filter(tipo %in% c("VP", "FP", "FN")) %>%
  group_by(tipo) %>%
  summarise(
    n = n(),
    med_hospitalizaciones = median(Num_Hospitalizaciones),
    med_dias_internado    = median(Dias_Internado),
    med_costo_total       = median(Costo_Total),
    med_edad              = median(Edad, na.rm = TRUE),
    med_cardiologia       = median(Visitas_Cardiologia),
    .groups = "drop"
  )
guardar_tabla(perfil_errores, "interp_perfil_errores")


# ==============================================================================
#  7. ESTIMACION DE COSTOS - GLM GAMMA
#  Unidad de analisis: la utilizacion. Gamma con enlace log porque el costo es
#  positivo, asimetrico y su varianza crece con la media.
# ==============================================================================
db_limpia <- readRDS(file.path(RUTA$resultados, "db_limpia.rds"))
pacientes <- readRDS(file.path(RUTA$resultados, "pacientes.rds"))

pos_i50 <- pacientes %>% filter(I50 == 1) %>% pull(CHAVE_FUNCIONAL)

costos <- db_limpia %>%
  filter(CHAVE_FUNCIONAL %in% pos_i50, VALOR_UTILIZACAO > 0) %>%
  left_join(select(pacientes, CHAVE_FUNCIONAL, Edad, Sexo), by = "CHAVE_FUNCIONAL") %>%
  transmute(
    VALOR_UTILIZACAO,
    Tipo_Atencion = CETIPO_std,
    Especialidad  = DESC_ESPECIALIDADE,
    UTI = Flag_UTI, Internado = Flag_Inter,
    Tipo_Unidad = TIPO_UNIDADE, UF = UF_HOSP,
    Porte_Anestesico = PORTE_ANESTESICO,
    Edad, Sexo
  )

# ---- 7.1 Costo observado ----
resumen_costo_obs <- costos %>%
  summarise(
    n = n(), media = mean(VALOR_UTILIZACAO), mediana = median(VALOR_UTILIZACAO),
    desv = sd(VALOR_UTILIZACAO),
    cv = sd(VALOR_UTILIZACAO) / mean(VALOR_UTILIZACAO),
    p95 = quantile(VALOR_UTILIZACAO, 0.95), maximo = max(VALOR_UTILIZACAO)
  )
guardar_tabla(resumen_costo_obs, "costo_observado_resumen")

costo_por_tipo <- costos %>%
  group_by(Tipo_Atencion) %>%
  summarise(n = n(), media = mean(VALOR_UTILIZACAO),
            mediana = median(VALOR_UTILIZACAO), .groups = "drop") %>%
  arrange(desc(media))
guardar_tabla(costo_por_tipo, "costo_por_tipo")

fig_costo_tipo <- costos %>%
  ggplot(aes(reorder(Tipo_Atencion, VALOR_UTILIZACAO, median),
             VALOR_UTILIZACAO + 1)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.85, outlier.alpha = 0.1) +
  scale_y_log10(labels = scales::comma) + coord_flip() +
  labs(title = "Costo por utilizaci\u00f3n seg\u00fan tipo de atenci\u00f3n (I50)",
       subtitle = "Escala log", x = NULL, y = "VALOR_UTILIZACAO (log)")
guardar_fig(fig_costo_tipo, "16_costo_por_tipo", h = 4)

# ---- 7.2 Ajuste del GLM ----
set.seed(SEED)
split_c <- initial_split(costos, prop = 0.80)
train_c <- training(split_c)
test_c  <- testing(split_c)

receta_costo <- recipe(VALOR_UTILIZACAO ~ ., data = train_c) %>%
  step_impute_median(Edad) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors(), new_level = "No_Registrado") %>%
  step_other(Especialidad, Tipo_Unidad, UF, Porte_Anestesico,
             threshold = 0.01, other = "OTROS") %>%
  step_mutate(Tipo_Atencion = factor(Tipo_Atencion), Sexo = factor(Sexo))

prep_c <- prep(receta_costo)
train_baked <- bake(prep_c, new_data = NULL)
test_baked  <- bake(prep_c, new_data = test_c)

train_baked <- train_baked %>%
  filter(if_all(where(is.numeric), is.finite)) %>%
  filter(!if_any(everything(), is.na)) %>%
  mutate(across(where(is.factor), droplevels)) %>%
  select(where(~ n_distinct(.) > 1) | any_of("VALOR_UTILIZACAO"))

# El IRLS diverge con enlace log si arranca en cero: se inicia en log(media).
X_terms <- setdiff(names(train_baked), "VALOR_UTILIZACAO")
n_coef  <- ncol(model.matrix(
  as.formula(paste("~", paste(X_terms, collapse = "+"))), data = train_baked))

modelo_costo <- tryCatch({
  glm(VALOR_UTILIZACAO ~ ., data = train_baked,
      family = Gamma(link = "log"),
      start = c(log(mean(train_baked$VALOR_UTILIZACAO)), rep(0, n_coef - 1)),
      control = glm.control(maxit = 200, epsilon = 1e-8))
}, error = function(e) {
  message("Gamma no convergi\u00f3. Se usa quasi-Poisson (misma lectura multiplicativa).")
  glm(VALOR_UTILIZACAO ~ ., data = train_baked,
      family = quasipoisson(link = "log"),
      control = glm.control(maxit = 200))
})

# ---- 7.3 Evaluacion ----
test_baked <- test_baked %>%
  mutate(pred = predict(modelo_costo, newdata = test_baked, type = "response"))

eval_costo <- test_baked %>%
  summarise(
    MAE  = mean(abs(pred - VALOR_UTILIZACAO)),
    MdAE = median(abs(pred - VALOR_UTILIZACAO)),
    RMSE = sqrt(mean((pred - VALOR_UTILIZACAO)^2)),
    media_obs = mean(VALOR_UTILIZACAO), media_pred = mean(pred)
  )
guardar_tabla(eval_costo, "costo_evaluacion")

# ---- 7.4 Factores de costo: exp(beta) = multiplicador ----
factores_costo <- tidy(modelo_costo) %>%
  filter(term != "(Intercept)") %>%
  mutate(multiplicador = exp(estimate),
         efecto = if_else(multiplicador > 1, "Aumenta", "Disminuye")) %>%
  arrange(desc(abs(estimate)))
guardar_tabla(factores_costo, "costo_factores")

fig_factores <- factores_costo %>%
  filter(p.value < 0.05) %>% slice_max(abs(estimate), n = 12) %>%
  mutate(term = fct_reorder(term, multiplicador)) %>%
  ggplot(aes(multiplicador, term, fill = efecto)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 1, color = "grey40") +
  scale_fill_manual(values = c("Aumenta" = "#C44E52", "Disminuye" = "#4C72B0")) +
  labs(title = "Factores multiplicativos del costo (GLM Gamma)",
       x = "exp(coeficiente)", y = NULL)
guardar_fig(fig_factores, "17_factores_costo", w = 8, h = 5)

# ---- 7.5 Perfiles clinicos ----
# UTI/Internado solo se activan en Internacion: combinaciones imposibles
# (una "consulta en UCI") harian extrapolar al modelo fuera de su soporte.
plantilla <- train_baked[1, ] %>% select(-VALOR_UTILIZACAO)

perfiles <- tribble(
  ~Perfil,                 ~Tipo_Atencion, ~Internado, ~UTI,
  "Consulta ambulatoria",  "Consulta",      0L,         0L,
  "Examen ambulatorio",    "Examen",        0L,         0L,
  "Urgencias",             "Urgencias",     0L,         0L,
  "Internaci\u00f3n sin UCI",   "Internacion",   1L,         0L,
  "Internaci\u00f3n con UCI",   "Internacion",   1L,         1L
) %>%
  crossing(Edad = c(60, 80))

perfiles_nd <- plantilla[rep(1, nrow(perfiles)), ]
perfiles_nd$Tipo_Atencion <- factor(perfiles$Tipo_Atencion,
                                    levels = levels(train_baked$Tipo_Atencion))
perfiles_nd$Internado <- perfiles$Internado
perfiles_nd$UTI       <- perfiles$UTI
perfiles_nd$Edad      <- perfiles$Edad

perfiles$costo_esperado <- predict(modelo_costo, newdata = perfiles_nd,
                                   type = "response")
perfiles <- perfiles %>% arrange(desc(costo_esperado))
guardar_tabla(perfiles, "costo_perfiles")

# ---- 7.6 Prima pura anual ----
prima_pura <- pacientes %>%
  mutate(grupo = if_else(I50 == 1, "I50", "No I50")) %>%
  group_by(grupo) %>%
  summarise(n = n(),
            costo_anual_medio   = mean(Costo_Total),
            costo_anual_mediana = median(Costo_Total),
            desv = sd(Costo_Total), .groups = "drop") %>%
  mutate(sobrecosto_vs_no_i50 = costo_anual_medio /
           costo_anual_medio[grupo == "No I50"])
guardar_tabla(prima_pura, "costo_prima_pura")


# ==============================================================================
#  8. CALIBRACION Y CURVA DE DECISION
#  scale_pos_weight descalibra las probabilidades: no afecta el ordenamiento
#  (ROC-AUC) pero si su lectura como riesgo real, que es lo que exige el DCA.
# ==============================================================================
set.seed(SEED)
res_oof <- fit_resamples(
  wf_xgb, resamples = folds,
  metrics = metric_set(pr_auc, roc_auc),
  control = control_resamples(save_pred = TRUE, event_level = NIVEL_EVENTO)
)
pred_oof <- collect_predictions(res_oof)

brier <- function(df) {
  y <- as.integer(df$I50 == "Enfermo")
  mean((df$.pred_Enfermo - y)^2)
}

fig_cal_antes <- cal_plot_breaks(pred_oof, truth = I50, estimate = .pred_Enfermo,
                                 event_level = NIVEL_EVENTO, num_breaks = 10) +
  labs(title = "Calibraci\u00f3n antes de corregir (out-of-fold)")
guardar_fig(fig_cal_antes, "18_calibracion_antes", w = 6, h = 5)

# Isotonica estimada en OOF y aplicada a test (nunca se calibra con el test).
calibrador <- cal_estimate_isotonic(pred_oof, truth = I50,
                                    estimate = dplyr::starts_with(".pred_"),
                                    event_level = NIVEL_EVENTO)
pred_test_cal <- cal_apply(pred_test, calibrador)

tabla_brier <- tibble(
  momento = c("Antes (crudo)", "Despu\u00e9s (isot\u00f3nica)"),
  brier_score = c(brier(pred_test), brier(pred_test_cal))
)
guardar_tabla(tabla_brier, "calibracion_brier")

fig_cal_despues <- cal_plot_breaks(pred_test_cal, truth = I50,
                                   estimate = .pred_Enfermo,
                                   event_level = NIVEL_EVENTO, num_breaks = 10) +
  labs(title = "Calibraci\u00f3n despu\u00e9s de la correcci\u00f3n isot\u00f3nica (test)")
guardar_fig(fig_cal_despues, "19_calibracion_despues", w = 6, h = 5)

cat("ROC-AUC antes:",
    round(roc_auc_vec(pred_test$I50, pred_test$.pred_Enfermo,
                      event_level = NIVEL_EVENTO), 4),
    "| despu\u00e9s:",
    round(roc_auc_vec(pred_test_cal$I50, pred_test_cal$.pred_Enfermo,
                      event_level = NIVEL_EVENTO), 4), "\n")

# ---- 8.1 Decision Curve Analysis ----
# Umbrales bajos: con prevalencia de 0.016% un corte al 5% no marcaria a nadie.
df_dca <- pred_test_cal %>%
  transmute(I50_event = (I50 == "Enfermo"), prob_xgb = .pred_Enfermo)

dca_obj <- dcurves::dca(I50_event ~ prob_xgb, data = df_dca,
                        thresholds = seq(0.0005, 0.02, by = 0.0005),
                        label = list(prob_xgb = "XGBoost (calibrado)"))

fig_dca <- plot(dca_obj, smooth = FALSE) +
  labs(title = "Decision Curve Analysis \u2014 fenotipado I50")
guardar_fig(fig_dca, "20_decision_curve", w = 8, h = 5)

nb <- dcurves::as_tibble(dca_obj) %>%
  select(variable, threshold, net_benefit) %>%
  pivot_wider(names_from = variable, values_from = net_benefit)
guardar_tabla(nb, "dca_beneficio_neto")

ventana <- nb %>% filter(prob_xgb > all, prob_xgb > none)
if (nrow(ventana) > 0) {
  cat("Beneficio neto positivo entre",
      sprintf("%.2f%%", min(ventana$threshold) * 100), "y",
      sprintf("%.2f%%", max(ventana$threshold) * 100), "de riesgo umbral.\n")
}


# ==============================================================================
#  9. PERSISTENCIA
# ==============================================================================
saveRDS(fit_xgb,      file.path(RUTA$modelos, "fit_xgboost.rds"))
saveRDS(fit_par,      file.path(RUTA$modelos, "fit_parsimonioso.rds"))
saveRDS(fit_log,      file.path(RUTA$modelos, "fit_logistica.rds"))
saveRDS(modelo_costo, file.path(RUTA$modelos, "modelo_costo_gamma.rds"))
saveRDS(calibrador,   file.path(RUTA$modelos, "calibrador.rds"))

