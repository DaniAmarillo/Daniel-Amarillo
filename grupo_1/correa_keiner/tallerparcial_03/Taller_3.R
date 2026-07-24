#' TALLER PARCIAL 3
#' KEINER FELIPE CORREA LEGUIZAMON
#' Mineria de Datos
#' Julio 2026
#' analisis completo en código

# 1. Carga de librerias ------------------------------------------------------
library(tidyverse)
library(dplyr)
library(janitor)   
library(lubridate)
library(stringr)
library(data.table)
library(stringi)
library(caret)
library(randomForest)
library(pROC)
library(PRROC)
library(broom)
library(gbm)
library(xgboost)

# 2. Carga de datos -----------------------------------------------
data1 <- data
data <- read.csv("db_2026.csv", header = T)

#resumen de los datos 
glimpse(data)
#se observan variables con NA's lo que nos indica que se debe hacer 
#un proceso de limpieza 

# 3. limpieza de datos -------------------------------------------------------

#semilla fija 
set.seed(100860)

#identificar los NA's y conversion 
data <- data %>%
  mutate(across(where(is.character), ~ str_trim(.x)))

valores_na <- c("N/A", "NA", "Na", "-", "")

data <- data %>%
  mutate(across(where(is.character), ~ replace(.x, .x %in% valores_na, NA)))


# 3.1 Valores faltantes por columna --------------------------------------

na_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(pct_na = round(100 * n_na / nrow(data), 2)) %>%
  arrange(desc(pct_na))

print(na_summary, n = Inf)


na_summary |>
  slice_max(pct_na, n = 6) |> 
  ggplot(aes(x = reorder(variable, pct_na), y = pct_na, fill = pct_na > 30)) +
  geom_col() +
  geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "firebrick"), guide = "none") +
  scale_y_continuous(limits = c(0, 105)) +
  labs(
    title   = "Porcentaje de valores faltantes por columna",
    x       = NULL, y     = "% faltante"
  ) +
  theme_minimal(base_size = 13)

#'Se observa que se encuentran columnas con un porcentaje alto de valores faltantes (NA)
#' Desc_especialidade, Porte_anestesico y CID
#'se recomienda tratar el uf_cnes y tipo_unidade


# 3.2 Limpieza en las variables ---------------------------------------


#Conversión formato fecha
data <- data %>%
  mutate(
    DT_UTILIZACAO = as.Date(DT_UTILIZACAO),
    DT_NASCIMENTO_BENEFICIARIO = as.Date(DT_NASCIMENTO_BENEFICIARIO)
  )

#variable PORTE_ANESTESICO
data <- data %>%
  mutate(
    PORTE_ANESTESICO = if_else(
      is.na(PORTE_ANESTESICO),
      "No_aplica",
      as.character(PORTE_ANESTESICO)
    )
  )
#no se sabe si el paciente tuvo o no, se inmuta con "no aplica" para evitar sesgos


#variable UF_CNES_PREST_HOSPITALAR
data <- data %>%
  mutate(UF_CNES_PREST_HOSPITALAR = if_else(
    is.na(UF_CNES_PREST_HOSPITALAR),
    "Desconocido",
    UF_CNES_PREST_HOSPITALAR
  )) #Se desconoce el nombre del establecimiento por lo tanto la ubicacion faltante
#se inmuta con "Desconocido"



#variable TIPO_UNIDADE_PREST_HOSPITALAR
data <- data %>%
  mutate(
    TIPO_UNIDADE_PREST_HOSPITALAR = TIPO_UNIDADE_PREST_HOSPITALAR %>%
      replace_na("DESCONOCIDO") %>%
      str_to_upper() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_trim()
  ) %>%
  
  mutate(
    tipo_unidad_grp = case_when(
      
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "PRONTO|SOCORRO|URGEN") ~ "URGENCIAS",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "HOSPITAL") ~ "HOSPITAL",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "LABORATORIO|DIAGNOSE|SADT") ~ "DIAGNOSTICO",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "CONSULTORIO|CLINICA|POLICLINICA") ~ "CONSULTA",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "BASICA|POSTO|CENTRO DE SAUDE") ~ "ATENCION_PRIMARIA",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "DOMICILIAR|HOME CARE") ~ "DOMICILIARIO",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "MOVEL") ~ "MOVIL",
      str_detect(TIPO_UNIDADE_PREST_HOSPITALAR, "DESCONOC") ~ "SIN INFORMACION",
      TRUE ~ "OTROS"
    )
  )

data <- data %>%
  mutate(
    unidad_missing_tug = if_else(tipo_unidad_grp == "SIN INFORMACION", 1, 0)
  )
#se resumen en 8 categorias para la interpretación. 
# se crea una variable indicadora con 1 si el registro no tiene informacion 
# de la unidad hospitalar donde fue atendida la persona, 0 en caso contrario.


# variable TIPO_BENEFICIARIO

data <- data %>%
  mutate(
    TIPO_BENEFICIARIO = TIPO_BENEFICIARIO %>%
      replace_na("NAO INFORMADO") %>%
      str_to_upper() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_trim(),
    
    tipo_beneficiario_grp = case_when(
      str_detect(TIPO_BENEFICIARIO, "IGNORADO|NAO INFORM") ~ "SIN INFORMACION",
      str_detect(TIPO_BENEFICIARIO, "TITULAR") ~ "TITULAR",
      str_detect(TIPO_BENEFICIARIO, "DEPENDENTE|FILHO|CONJUGE|MAE") ~ "DEPENDIENTE",
      str_detect(TIPO_BENEFICIARIO, "AGREGADO|OUTROS") ~ "OTROS",
      
      TRUE ~ "OTROS"
    )
  )
# secorrigen categorias para ser mas especificas



#variable CETIPO 

data <- data %>%
  mutate(
    CETIPO = CETIPO %>%
      str_to_upper() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_trim(),
    
    tipo_servicio = case_when(
      str_detect(CETIPO, "CONSULT") ~ "CONSULTA",
      str_detect(CETIPO, "EXAM") ~ "EXAMEN",
      str_detect(CETIPO, "TERAP") ~ "TERAPIA",
      str_detect(CETIPO, "INTERN") ~ "INTERNACION",
      str_detect(CETIPO, "PRONTO|SOCORRO") ~ "URGENCIAS",
      str_detect(CETIPO, "OUTRO") ~ "OTROS",
      TRUE ~ "OTROS"
    )
  )
#se completan nombres 

data$tipo_servicio <- factor(
  data$tipo_servicio,
  levels = c("CONSULTA", "EXAMEN", "TERAPIA", "URGENCIAS", "INTERNACION", "OTROS"),
  ordered = TRUE
)


# 3.3 Inconsistencia: mismo beneficiario con más de un sexo --------------

data <- data %>%
  mutate(
    SEXO_BENEFICIARIO = SEXO_BENEFICIARIO %>%
      str_to_upper() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_trim(),
    
      SEXO_BENEFICIARIO = case_when(
      SEXO_BENEFICIARIO %in% c("M", "MASCULINO") ~ "M",
      SEXO_BENEFICIARIO == "F" ~ "F",
      str_detect(SEXO_BENEFICIARIO, "NAO INFORM") ~ "SIN INFORMACION",
      TRUE ~ "SIN INFORMACION"
    )
  )


inconsistencias_sexo <- data %>%
  filter(!is.na(SEXO_BENEFICIARIO)) %>%
  distinct(CHAVE_FUNCIONAL, SEXO_BENEFICIARIO) %>%
  count(CHAVE_FUNCIONAL) %>%
  filter(n > 1)

cat("Beneficiarios con sexo inconsistente:", nrow(inconsistencias_sexo), "\n")

#se encontraron 525 inconsistencias de genero en pacientes con la misma "CHAVE"
# nos quedamos con el sexo más frecuente entre sus consultas. 


ids_inconsistentes <- inconsistencias_sexo$CHAVE_FUNCIONAL

sexo_por_beneficiario <- data %>%
  filter(CHAVE_FUNCIONAL %in% ids_inconsistentes,
         !is.na(SEXO_BENEFICIARIO)) %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    SEXO_BENEFICIARIO = names(sort(table(SEXO_BENEFICIARIO), decreasing = TRUE))[1],
    .groups = "drop"
  )


data <- data %>%  #coreccion del genero para los beneficiarios 
  left_join(sexo_por_beneficiario, by = "CHAVE_FUNCIONAL", suffix = c("", "_corr")) %>%
  mutate(SEXO_BENEFICIARIO = coalesce(SEXO_BENEFICIARIO_corr, SEXO_BENEFICIARIO)) %>%
  select(-SEXO_BENEFICIARIO_corr)


# 3.4 Inconsistencia: fecha de nacimiento distinta por beneficiario ------

inconsistencias_nacimento <- data %>%
  filter(!is.na(DT_NASCIMENTO_BENEFICIARIO)) %>%
  distinct(CHAVE_FUNCIONAL, DT_NASCIMENTO_BENEFICIARIO) %>%
  count(CHAVE_FUNCIONAL) %>%
  filter(n > 1)

cat("Beneficiarios con fecha de nacimiento inconsistente:",
    nrow(inconsistencias_nacimento), "\n")

#No hay incosistencias de fechas de nacimiento 


# nacimento_por_beneficiario <- data %>%
#   filter(!is.na(DT_NASCIMENTO_BENEFICIARIO)) %>%
#   count(CHAVE_FUNCIONAL, DT_NASCIMENTO_BENEFICIARIO) %>%
#   group_by(CHAVE_FUNCIONAL) %>%
#   slice_max(n, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(CHAVE_FUNCIONAL, DT_NASCIMENTO_BENEFICIARIO)


# 3.5 Fechas inválidas / fuera de rango -----------------------------------


fechas_invalidas <- data %>%
  mutate(
    tipo_error = case_when(
      DT_UTILIZACAO < DT_NASCIMENTO_BENEFICIARIO ~ "uso_antes_nacer",
      DT_NASCIMENTO_BENEFICIARIO > Sys.Date() ~ "nacimiento_futuro",
      DT_UTILIZACAO > Sys.Date() ~ "uso_futuro"
    )
  ) %>%
  filter(!is.na(tipo_error))
# dt_utilizacao no debería ser anterior a dt_nascimento_beneficiario
# dt_nascimento_beneficiario no debería ser futura a la fecha de hoy

fechas_invalidas %>% count(tipo_error)

cat("Registros con fechas inválidas:", nrow(fechas_invalidas), "\n")
#se encontraron 31178 fechas invalidas

hoy <- Sys.Date()

#fechas de nacimiento superiores a hoy 
data <- data %>%
  mutate(
    DT_NASCIMENTO_BENEFICIARIO = if_else(
      DT_NASCIMENTO_BENEFICIARIO > hoy,
      as.Date(NA_character_),
      DT_NASCIMENTO_BENEFICIARIO
    )
  )


#fechas de nacimiento desde 01/01/26 a hoy
data <- data%>%
  mutate(
    error_uso = !is.na(DT_UTILIZACAO) & 
      !is.na(DT_NASCIMENTO_BENEFICIARIO) & 
      DT_UTILIZACAO < DT_NASCIMENTO_BENEFICIARIO
  )

sum(data$error_uso)

data <- data %>%
  mutate(
    DT_UTILIZACAO = if_else(error_uso, as.Date(NA), DT_UTILIZACAO)
  ) %>%
  select(-error_uso)


#registros con fecha de utilizacion antes del nacimiento 
data <- data %>%
  mutate(
    DT_UTILIZACAO = if_else(
      DT_UTILIZACAO > hoy,
      as.Date(NA_character_),
      DT_UTILIZACAO
    )
  )

# 3.6 Valores extremos en VALOR_UTILIZACAO  --------------------------------
summary(data$VALOR_UTILIZACAO)
#se observa que el minimo es un valor negativo y el maximo es muy grande en 
#comparacion con la media 

q <- quantile(data$VALOR_UTILIZACAO, probs = c(0.01, 0.25, 0.5, 0.75, 0.99, 0.999),
              na.rm = TRUE)
print(q)

valores_invalidos <- data %>% filter(VALOR_UTILIZACAO <= 0)
cat("Registros con valor_utilizacao <= 0:", nrow(valores_invalidos), "\n")
#se encuentran 73395 registros con valores invalidos. Estos son, aquellos con
#valores negativos o con ceros.


#como se presencio que los datos tienen una cola pesada se aplica una transformación
#a estos con el logaritmo. (Solo se aplica para los valores mayores a 0, por propiedas del log)
data <- data %>%
  mutate(
    VALOR_UTILIZACAO = ifelse(VALOR_UTILIZACAO <= 0, NA, VALOR_UTILIZACAO),
    log_valor = log1p(VALOR_UTILIZACAO)
  )


#se usó el criterio de rango intercuartílico
iqr_log <- IQR(data$log_valor, na.rm = TRUE)
q3_log  <- quantile(data$log_valor, 0.75, na.rm = TRUE)
umbral_extremo <- expm1(q3_log + 3 * iqr_log)
cat("Umbral de valor extremo (VALOR_UTILIZACAO):", round(umbral_extremo, 2), "\n")


#se marcan los outliers
data <- data %>%
  mutate(
    es_valor_extremo = VALOR_UTILIZACAO > umbral_extremo
  )

#grafico para comparar los outliers de la variable transformada y original
data %>%
  select(VALOR_UTILIZACAO, log_valor) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  ggplot(aes(x = variable, y = valor, fill = variable)) +
  geom_boxplot(outlier.colour = "firebrick", outlier.size = 2) +
  scale_fill_brewer(palette = "Pastel1", guide = "none") +
  labs(title = "Boxplot — detección visual de outliers", x = NULL, y = "Valor") +
  theme_minimal(base_size = 13)


# 3.7 Resumen del dataset ---------------------------------------------------


cat("Número de filas:", nrow(data), "\n")
cat("Número de beneficiarios únicos:", n_distinct(data$CHAVE_FUNCIONAL), "\n")

#'Se trabajara con 9'345.278 consultas que corresponden a 653.631 pacientes.

#se guarda la base con las modificaciones
dir.create("data", showWarnings = FALSE)
saveRDS(data, "data/data.rds")

# data <- readRDS("data/data.rds")

# 3.8 Creación variable -------------------------------------------------------

#' Se identifica que la variable CID presenta un porcentaje de 88.6% de datos 
#' faltante, lo que presenta un desafio para la variable objetivo que se construira
#' mas adelante. Se construye un dataset con aquellas filas en donde se tenga 
#' informacion de la variable CID.

dt <- data %>%
  filter(!is.na(CID)) %>%
  mutate(
    CID = str_to_upper(str_remove_all(CID, "[.\\s]"))
  )


#codigos de itu para clasificar los pacientes
codigos_itu <- c("N390", "N30", "N10", "N11", "N12")

patron_itu <- paste0("^(", paste(codigos_itu, collapse = "|"), ")")


#filtrar el CID por codigos_itu y los cuenta
dt%>%
  filter(str_starts(CID, patron_itu)) %>%
  count(CID, sort = TRUE)

# Regla: un beneficiario recibe etiqueta 1 si tiene al menos una
# transacción con CID que empiece por alguno de los codigo itu
#"N390", "N30", "N10", "N11", "N12", en caso contrario 0.

 target_itu <- dt %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    itu = as.integer(any(str_detect(CID, patron_itu)))
  ) %>%
  ungroup()

 
#numero de pacientes con la enfermedad
dt %>%
  filter(str_detect(CID, patron_itu)) %>%
  summarise(n_personas = n_distinct(CHAVE_FUNCIONAL))
#personas:419


# Distribución de la variable objetivo
target_itu %>%
  count(itu) %>%
  mutate(pct = round(100 * n / sum(n), 2))


#enlace de la variable con el dataset 
dt <- dt %>%
  left_join(target_itu, by = "CHAVE_FUNCIONAL")


# 4. construccion de variables a nivel beneficiario --------------------------------------------


data1 <- as.data.table(data)
setkey(data1, CHAVE_FUNCIONAL)

variables_beneficiario <- data1[, .(
  n_procedimientos     = .N,
  n_utilizaciones      = uniqueN(DT_UTILIZACAO),
  
  costo_total          = sum(VALOR_UTILIZACAO, na.rm = TRUE),
  costo_promedio_proc  = mean(VALOR_UTILIZACAO, na.rm = TRUE),
  costo_max            = max(VALOR_UTILIZACAO, na.rm = TRUE),
  
  n_especialidades = uniqueN(DESC_ESPECIALIDADE, na.rm = TRUE),
  n_tipos_unidad       = uniqueN(tipo_unidad_grp),
  
  n_consultas          = sum(tipo_servicio == "CONSULTA", na.rm = TRUE),
  n_examenes           = sum(tipo_servicio == "EXAMEN", na.rm = TRUE),
  n_terapias           = sum(tipo_servicio == "TERAPIA", na.rm = TRUE),
  n_otros              = sum(tipo_servicio == "OTROS", na.rm = TRUE),
  
  rango_dias           = as.numeric(max(DT_UTILIZACAO, na.rm = TRUE) -
                                      min(DT_UTILIZACAO, na.rm = TRUE)),
  
  n_valores_extremos   = sum(es_valor_extremo, na.rm = TRUE),
  
  # señales específicas de ITU no dependen del CID, dependen de la
  # especialidad del dataset
  
  tuvo_urologia        = as.integer(any(str_detect(str_to_upper(DESC_ESPECIALIDADE),
                                                   "UROLOG|NEFROLOG"), na.rm = TRUE)),
  tuvo_examen_orina    = as.integer(any(str_detect(str_to_upper(DESCRICAO_PROCEDIMENTO),
                                                   "URINA|UROCULTURA|URINALISE"), na.rm = TRUE)),
  
  DT_NASCIMIENTO_BENEFICIARIO = DT_NASCIMENTO_BENEFICIARIO[1]
), by = CHAVE_FUNCIONAL]


utilizaciones_tipo <- data1[, .(
  es_internacion = any(tipo_servicio == "INTERNACION"),
  es_urgencia    = any(tipo_servicio == "URGENCIAS")
), by = .(CHAVE_FUNCIONAL, DT_UTILIZACAO)]

conteos_utilizacion <- utilizaciones_tipo[, .(
  n_internaciones = sum(es_internacion),
  n_urgencias     = sum(es_urgencia)
), by = CHAVE_FUNCIONAL]


variables_beneficiario <- merge(variables_beneficiario, conteos_utilizacion, by = "CHAVE_FUNCIONAL", all.x = TRUE)

variables_beneficiario[, edad := floor(as.numeric(max(data1$DT_UTILIZACAO, na.rm = TRUE) -
                                                    DT_NASCIMIENTO_BENEFICIARIO) / 365.25)]
n_edad_invalida <- sum(variables_beneficiario$edad < 0 | variables_beneficiario$edad > 110, na.rm = TRUE)
cat("Edades fuera de rango tratadas como NA:", n_edad_invalida, "\n")

variables_beneficiario[edad < 0 | edad > 110, edad := NA]


variables_beneficiario[, prop_internaciones := ifelse(n_utilizaciones > 0, n_internaciones / n_utilizaciones, 0)]
variables_beneficiario[, prop_urgencias := ifelse(n_utilizaciones > 0, n_urgencias / n_utilizaciones, 0)]
variables_beneficiario[, costo_log           := log1p(costo_total)]


variables_beneficiario <- merge(variables_beneficiario, target_itu,
                                by = "CHAVE_FUNCIONAL", all.x = TRUE)

variables_beneficiario[is.na(itu), itu := 0L]


sexo_beneficiario <- data1[!is.na(SEXO_BENEFICIARIO),
                           .(SEXO_BENEFICIARIO = first(SEXO_BENEFICIARIO)),
                           by = CHAVE_FUNCIONAL]

tipo_benef_por_beneficiario <- data1[
  !is.na(tipo_beneficiario_grp), .N, by = .(CHAVE_FUNCIONAL, tipo_beneficiario_grp)
][order(CHAVE_FUNCIONAL, -N)][, .SD[1], by = CHAVE_FUNCIONAL
][, .(CHAVE_FUNCIONAL, tipo_beneficiario_grp)]

variables_beneficiario <- merge(variables_beneficiario, sexo_beneficiario,
                                by = "CHAVE_FUNCIONAL", all.x = TRUE)
variables_beneficiario <- merge(variables_beneficiario, tipo_benef_por_beneficiario,
                                by = "CHAVE_FUNCIONAL", all.x = TRUE)

cat("Beneficiarios:", nrow(variables_beneficiario), "\n")
cat("Positivos (ITU):", sum(variables_beneficiario$itu), "\n")



saveRDS(variables_beneficiario, "data/variables_beneficiario.rds")


# 5. Datos descriptivos ---------------------------------------------------

cat("Número de beneficiarios analizados:", nrow(variables_beneficiario), "\n")
cat("Número de utilizaciones/consultas:", sum(variables_beneficiario$n_utilizaciones), "\n")
cat("Número de procedimientos:", sum(variables_beneficiario$n_procedimientos), "\n")

# ---- 5.2 Distribución de la variable objetivo --------------------------------
variables_beneficiario %>%
  count(itu) %>%
  mutate(pct = round(100 * n / sum(n), 2))

# ---- 5.3 Distribución por sexo, tipo de beneficiario -----------------
variables_beneficiario %>% count(SEXO_BENEFICIARIO, itu) %>%
  group_by(SEXO_BENEFICIARIO) %>% mutate(pct = round(100 * n / sum(n), 1))


variables_beneficiario %>% count(tipo_beneficiario_grp, itu) %>%
  group_by(tipo_beneficiario_grp) %>% mutate(pct = round(100 * n / sum(n), 1))


# ---- 5.4 Distribución por edad, comparando ITU = 1 vs ITU = 0 -------------
variables_beneficiario %>%
  group_by(itu) %>%
  summarise(
    edad_media   = mean(edad, na.rm = TRUE),
    edad_mediana = median(edad, na.rm = TRUE),
    edad_sd      = sd(edad, na.rm = TRUE)
  )

ggplot(variables_beneficiario, aes(x = edad, fill = factor(itu))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de edad según ITU",
       x = "Edad", fill = "ITU") +
  theme_minimal()

# ---- 5.5 Especialidades más consultadas por beneficiarios con ITU ----------
data %>%
  inner_join(
    variables_beneficiario %>% filter(itu == 1) %>% select(CHAVE_FUNCIONAL),
    by = "CHAVE_FUNCIONAL"
  ) %>%
  count(DESC_ESPECIALIDADE, sort = TRUE) %>%
  slice_head(n = 15)

# ---- 5.6 Valores faltantes en la tabla final de variables -------------------
variables_beneficiario %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(pct_na = round(100 * n_na / nrow(variables_beneficiario), 2)) %>%
  arrange(desc(pct_na)) %>%
  print(n = Inf)

# ---- 5.7 Valores extremos en el costo, a nivel de beneficiario --------------
ggplot(variables_beneficiario, aes(x = costo_total)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Distribución del costo total por beneficiario (escala log)",
       x = "Costo total (log10)", y = "Frecuencia") +
  theme_minimal()

variables_beneficiario %>%
  summarise(
    costo_p50 = median(costo_total, na.rm = TRUE),
    costo_p90 = quantile(costo_total, 0.90, na.rm = TRUE),
    costo_p99 = quantile(costo_total, 0.99, na.rm = TRUE),
    costo_max = max(costo_total, na.rm = TRUE)
  )




#grafico del factor itu vs costol_total
ggplot(variables_beneficiario, aes(x = factor(itu), y = costo_total)) +
  geom_boxplot() +
  scale_y_log10()



#tabla 
variables_beneficiario %>%
  group_by(itu) %>%
  summarise(
    edad = mean(edad, na.rm =T),
    procedimientos = mean(n_procedimientos),
    costo = mean(costo_total)
  )





#6 Modelamiento de los datos -----------------------------------------------

#semilla fija 
set.seed(100860)


# ---- 6.1 Tabla de modelamiento ----------------------------------------------
modelo_df <- variables_beneficiario %>%
  as_tibble() %>%
  mutate(
    sexo_beneficiario = factor(SEXO_BENEFICIARIO),
    tipo_beneficiario = factor(replace_na(as.character(tipo_beneficiario_grp), "SIN INFORMACION")),
    itu = factor(itu, levels = c(0, 1), labels = c("No", "Si"))
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  filter(!is.na(itu), !is.na(sexo_beneficiario))


formula_modelo <- itu ~ edad + sexo_beneficiario + tipo_beneficiario +
  n_tipos_unidad + rango_dias + prop_internaciones + prop_urgencias +
  costo_log + n_especialidades + n_valores_extremos +
  tuvo_urologia + tuvo_examen_orina

# ---- 6.2 Partición train (70%) / validación (15%) / test (15%) --------------

idx_train <- createDataPartition(modelo_df$itu, p = 0.70, list = FALSE)
train_df  <- modelo_df[idx_train, ]
resto_df  <- modelo_df[-idx_train, ]

idx_val   <- createDataPartition(resto_df$itu, p = 0.50, list = FALSE)
val_df    <- resto_df[idx_val, ]
test_df   <- resto_df[-idx_val, ]

cat("Train:", nrow(train_df), " Val:", nrow(val_df), " Test:", nrow(test_df), "\n")

peso_si <- sum(train_df$itu == "No") / sum(train_df$itu == "Si")

# ---- 6.3 Modelos --------------------------------------------------------------

# Logística ponderada
modelo_logit <- suppressWarnings(glm(
  formula_modelo, data = train_df, family = binomial(),
  weights = ifelse(train_df$itu == "Si", peso_si, 1)
))

# Random Forest, balance interno 4:1 (menos agresivo que sampsize 1:1,
# conserva más señal de la clase mayoritaria)
modelo_rf <- randomForest(
  formula_modelo, data = train_df, ntree = 500,
  sampsize = c(No = 4 * sum(train_df$itu == "Si"), Si = sum(train_df$itu == "Si")),
  strata = train_df$itu, importance = TRUE
)

# Gradient Boosting
train_df$itu_num <- ifelse(train_df$itu == "Si", 1, 0)
modelo_gbm <- gbm(
  update(formula_modelo, itu_num ~ .), data = train_df,
  distribution = "bernoulli", n.trees = 500, interaction.depth = 4,
  shrinkage = 0.03, n.minobsinnode = 15, bag.fraction = 0.7, verbose = FALSE
)

 # XGBoost
x_train <- model.matrix(formula_modelo, data = train_df)[, -1]
x_val   <- model.matrix(formula_modelo, data = val_df)[, -1]
x_test  <- model.matrix(formula_modelo, data = test_df)[, -1]

dtrain <- xgb.DMatrix(x_train, label = ifelse(train_df$itu == "Si", 1, 0))
modelo_xgb <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "aucpr",
                max_depth = 4, eta = 0.05, scale_pos_weight = peso_si),
  data = dtrain, nrounds = 300, verbose = 0
)

# ---- 6.4 Función de evaluación -------------------------------------------------

evaluar_modelo <- function(y_real, y_pred_clase, y_pred_prob, nombre_modelo) {
  cm <- table(Real = y_real, Predicho = y_pred_clase)
  tp <- ifelse("Si" %in% rownames(cm) & "Si" %in% colnames(cm), cm["Si","Si"], 0)
  tn <- ifelse("No" %in% rownames(cm) & "No" %in% colnames(cm), cm["No","No"], 0)
  fp <- ifelse("No" %in% rownames(cm) & "Si" %in% colnames(cm), cm["No","Si"], 0)
  fn <- ifelse("Si" %in% rownames(cm) & "No" %in% colnames(cm), cm["Si","No"], 0)
  accuracy <- (tp + tn) / sum(cm)
  sens <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  esp  <- ifelse(tn + fp == 0, 0, tn / (tn + fp))
  prec <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  f1   <- ifelse(prec + sens == 0, 0, 2 * prec * sens / (prec + sens))
  roc_obj <- roc(y_real, y_pred_prob, levels = c("No","Si"), direction = "<", quiet = TRUE)
  pr_obj  <- pr.curve(scores.class0 = y_pred_prob[y_real == "Si"],
                      scores.class1 = y_pred_prob[y_real == "No"], curve = FALSE)
  tibble(modelo = nombre_modelo, accuracy, f1, sensibilidad = sens,
         especificidad = esp, roc_auc = as.numeric(auc(roc_obj)),
         pr_auc = pr_obj$auc.integral)
}

optimizar_umbral <- function(y_real, y_prob) {
  umbrales <- seq(0.05, 0.9, by = 0.01)
  f1s <- sapply(umbrales, function(u) {
    r <- evaluar_modelo(y_real, factor(ifelse(y_prob > u, "Si","No"), levels = c("No","Si")), y_prob, "x")
    r$f1
  })
  tibble(umbral_optimo = umbrales[which.max(f1s)], f1_val = max(f1s))
}

# ---- 6.5 Probabilidades en validación (para elegir umbral) y test (evaluación final) ----


prob_val <- list(
  logit = predict(modelo_logit, val_df, type = "response"),
  rf    = predict(modelo_rf, val_df, type = "prob")[, "Si"],
  gbm   = predict(modelo_gbm, val_df, n.trees = 500, type = "response"),
  xgb   = predict(modelo_xgb, x_val)
)
prob_test <- list(
  logit = predict(modelo_logit, test_df, type = "response"),
  rf    = predict(modelo_rf, test_df, type = "prob")[, "Si"],
  gbm   = predict(modelo_gbm, test_df, n.trees = 500, type = "response"),
  xgb   = predict(modelo_xgb, x_test)
)

umbrales <- imap_dfr(prob_val, ~ optimizar_umbral(val_df$itu, .x) %>% mutate(modelo = .y, .before = 1))
print(umbrales)

resultados_finales <- imap_dfr(prob_test, function(p, nombre) {
  u <- umbrales$umbral_optimo[umbrales$modelo == nombre]
  clase <- factor(ifelse(p > u, "Si", "No"), levels = c("No", "Si"))
  evaluar_modelo(test_df$itu, clase, p, nombre)
})

print(resultados_finales %>% arrange(desc(pr_auc)) %>% mutate(across(where(is.numeric), ~round(.,3))))

saveRDS(resultados_finales, "data/resultados_modelos.rds")



prevalencia <- mean(modelo_df$itu == "Si")
cat("PR-AUC de un modelo sin poder predictivo (piso):", round(prevalencia, 3), "\n")




#7. Interpretación de resultados --------------------------------------------

# ---- 7.1 Importancia de variables (Random Forest, modelo principal) --------
varImpPlot(modelo_rf, main = "Importancia de variables - Random Forest")

importancia_rf <- importance(modelo_rf) %>% as.data.frame() %>%
  rownames_to_column("variable") %>% arrange(desc(MeanDecreaseGini))
print(importancia_rf)


# ---- 7.2 Coeficientes / odds ratios (Regresión logística) --------------------
coef_logit <- tidy(modelo_logit, exponentiate = TRUE, conf.int = TRUE) %>%
  arrange(desc(abs(estimate - 1)))
print(coef_logit)

# ---- 7.3 Comparación entre modelos --------------------------------------------
resultados_finales %>%
  arrange(desc(pr_auc)) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  knitr::kable()

# Nota para el informe: Random Forest tiene el mejor F1/PR-AUC/ROC-AUC.
# XGBoost logra un PR-AUC casi idéntico (0.076 vs 0.077) pero con
# sensibilidad muy superior (0.59 vs 0.213) a costa de algo de
# especificidad -un trade-off relevante si el objetivo de negocio
# prioriza no dejar pasar casos de ITU por encima de minimizar falsas
# alarmas-. GBM quedó con especificidad casi perfecta pero sensibilidad
# muy baja (0.066): su umbral óptimo terminó siendo demasiado
# conservador, prediciendo "No" casi siempre.

# ---- 7.4 Análisis de errores (sobre el modelo principal: Random Forest) -----
umbral_rf <- umbrales$umbral_optimo[umbrales$modelo == "rf"]

test_con_pred <- test_df %>%
  mutate(
    prob_rf    = prob_test$rf,
    pred_rf    = factor(ifelse(prob_rf > umbral_rf, "Si", "No"), levels = c("No", "Si")),
    tipo_error = case_when(
      itu == "Si" & pred_rf == "No" ~ "Falso negativo",
      itu == "No" & pred_rf == "Si" ~ "Falso positivo",
      TRUE ~ "Correcto"
    )
  )

test_con_pred %>%
  group_by(tipo_error) %>%
  summarise(
    n = n(),
    edad_media       = mean(edad, na.rm = TRUE),
    costo_medio      = mean(exp(costo_log) - 1, na.rm = TRUE),
    internaciones    = mean(prop_internaciones, na.rm = TRUE),
    tuvo_examen_orina_pct = round(100 * mean(tuvo_examen_orina, na.rm = TRUE), 1)
  )

# Nota para el informe: revisar si los falsos negativos (casos de ITU no
# detectados) tienen rango_dias más bajo (poco tiempo de observación) o
# menos procedimientos registrados en general -es decir, si el modelo
# falla más con pacientes de historial corto en la base, lo cual sería
# una limitación de los datos, no del modelo-.

# ---- 7.5 Sesgos y limitaciones (para discutir en el Rmd) ---------------------
# - Prevalencia extrema (~0.1%): incluso un buen modelo tendrá PR-AUC
#   bajo en términos absolutos; el punto de referencia correcto es el
#   piso teórico (~0.001), no accuracy ni un umbral arbitrario como 85%.
# - La variable objetivo depende de que la ITU haya sido *codificada*
#   como CID en alguna transacción: subregistro es probable, y afecta
#   tanto entrenamiento como evaluación.
# - rango_dias como predictor importante sugiere sesgo por tiempo de
#   observación: a más historial en la base, más oportunidad de que
#   quede registrado un diagnóstico.
# - tuvo_examen_orina es un predictor "río abajo" del proceso clínico
#   (ver 7.1), lo que puede limitar la generalización del modelo.
# - Se evaluó ROSE como estrategia adicional de balanceo, pero no fue
#   compatible con las variables categóricas del modelo sin trabajo
#   adicional de codificación; se optó por mantener class weights +
#   muestreo estratificado (sampsize/strata), técnicas estándar y más
#   fáciles de justificar metodológicamente.

# ==============================================================================
# 8. ESTIMACIÓN DEL COSTO ASOCIADO A ITU
# ==============================================================================

# Unidad de análisis: costo total por BENEFICIARIO (costo_total, ya
# construida en la sección 4, sobre la utilización completa del
# paciente). Se elige el beneficiario como unidad porque el objetivo es
# estimar el costo esperado que representa un beneficiario con ITU para
# el asegurador, no el costo de un procedimiento puntual.

# ---- 8.1 Costo promedio observado, ITU vs no ITU -----------------------------
costo_por_grupo <- modelo_df %>%
  group_by(itu) %>%
  summarise(
    n             = n(),
    costo_medio   = mean(costo_total, na.rm = TRUE),
    costo_mediana = median(costo_total, na.rm = TRUE),
    costo_sd      = sd(costo_total, na.rm = TRUE),
    costo_p90     = quantile(costo_total, 0.90, na.rm = TRUE)
  )
print(costo_por_grupo)

ggplot(modelo_df, aes(x = itu, y = costo_total)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Costo total por beneficiario según diagnóstico de ITU",
       x = "ITU", y = "Costo total (log10)") +
  theme_minimal()

# ---- 8.2 Modelo de costo esperado ---------------------------------------------
# GLM Gamma con enlace log: el costo es continuo, positivo y con cola
# pesada (ver histograma de la sección 5.7 y el umbral de valores
# extremos definido en la sección 3.6).
modelo_costo_df <- modelo_df %>% filter(costo_total > 0)

cat("Beneficiarios excluidos del modelo de costo (costo_total <= 0):",
    sum(modelo_df$costo_total <= 0), "de", nrow(modelo_df), "\n")

modelo_costo <- glm(
  costo_total ~ edad + sexo_beneficiario + tipo_beneficiario + n_utilizaciones +
    n_internaciones + n_urgencias + n_especialidades + itu,
  data = modelo_costo_df, family = Gamma(link = "log"),
  control = glm.control(maxit = 100)
)
summary(modelo_costo)


modelo_costo_simple <- glm(
  costo_total ~ edad + sexo_beneficiario + tipo_beneficiario + itu,
  data = modelo_costo_df, family = Gamma(link = "log"),
  control = glm.control(maxit = 100)
)

tidy(modelo_costo_simple, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald") %>%
  filter(term == "ituSi")
summary(modelo_costo_simple)

coef_costo <- tidy(modelo_costo, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald") %>%
  arrange(desc(abs(estimate - 1)))
print(coef_costo)

# Cada coeficiente exponenciado es un factor multiplicativo sobre el
# costo esperado (ej. estimate = 1.30 implica 30% más costo esperado,
# manteniendo lo demás constante).

# ---- 8.3 Costo esperado bajo distintos perfiles (ITU = Sí vs No) -------------
perfil_base <- modelo_df %>%
  summarise(
    edad              = median(edad, na.rm = TRUE),
    sexo_beneficiario = names(sort(table(sexo_beneficiario), decreasing = TRUE))[1],
    tipo_beneficiario = names(sort(table(tipo_beneficiario), decreasing = TRUE))[1],
    n_utilizaciones   = median(n_utilizaciones, na.rm = TRUE),
    n_internaciones   = median(n_internaciones, na.rm = TRUE),
    n_urgencias       = median(n_urgencias, na.rm = TRUE),
    n_especialidades  = median(n_especialidades, na.rm = TRUE)
  )

perfiles_itu <- perfil_base %>%
  slice(rep(1, 2)) %>%
  mutate(
    itu               = factor(c("No", "Si"), levels = levels(modelo_df$itu)),
    sexo_beneficiario = factor(sexo_beneficiario, levels = levels(modelo_df$sexo_beneficiario)),
    tipo_beneficiario = factor(tipo_beneficiario, levels = levels(modelo_df$tipo_beneficiario))
  )

perfiles_itu$costo_esperado <- predict(modelo_costo, newdata = perfiles_itu, type = "response")
print(perfiles_itu %>% select(itu, costo_esperado))

# ---- 8.4 Variables asociadas al costo y limitaciones (para el Rmd) ----------
# - Revisar en coef_costo cuáles variables aumentan/disminuyen más el
#   costo esperado (se espera que n_internaciones, n_urgencias e
#   itu = "Si" estén entre las de mayor efecto, consistente con la
#   importancia de variables de la sección 7.1).
# - Limitaciones: el modelo asume relación estable en el tiempo, no
#   incorpora inflación médica ni cambios de tarifario; costo_total
#   depende del periodo de observación disponible por beneficiario
#   (alguien con menos tiempo en la base tendrá, en promedio, menor
#   costo acumulado aunque tenga ITU) — la misma limitación de
#   rango_dias discutida en la sección 7.5.

saveRDS(modelo_costo, "data/modelo_costo.rds")









