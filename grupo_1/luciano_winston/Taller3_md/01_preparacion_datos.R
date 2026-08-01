# ==============================================================================
#  01_preparacion_datos.R
#  Ingesta, calidad, variable objetivo, feature engineering y EDA
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

SEED <- 2026
set.seed(SEED)

RUTA <- list(datos = "data", figuras = "figuras",
             modelos = "modelos", resultados = "resultados")
invisible(lapply(RUTA, dir.create, showWarnings = FALSE, recursive = TRUE))

CID_OBJETIVO <- "I50"
ANIO_CORTE   <- 2026
COLOR_CLASE  <- c("Sano" = "#4C72B0", "Enfermo" = "#C44E52")

theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey35"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top")
)

# ---- Funciones auxiliares ----

ACENTOS <- "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00fd\u00c1\u00c0\u00c2\u00c3\u00c4\u00c5\u00c7\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf\u00d1\u00d3\u00d2\u00d4\u00d5\u00d6\u00da\u00d9\u00db\u00dc\u00dd"
PLANOS  <- "aaaaaaceeeeiiiinooooouuuuyAAAAAACEEEEIIIINOOOOOUUUUY"

norm_texto <- function(x) {
  x <- chartr(ACENTOS, PLANOS, as.character(x))
  x <- toupper(trimws(x))
  na_if(na_if(x, ""), "-")
}

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

norm_cetipo <- function(x) {
  x <- norm_texto(x)  
  case_when(
    x %in% c("C", "CONSULTA")                    ~ "Consulta",
    x %in% c("E", "EXAME", "EXAMEN")             ~ "Examen",
    x %in% c("T", "TERAPIA")                     ~ "Terapia",
    x %in% c("I", "INTERNACAO", "INTERNACION")   ~ "Internacion",
    x %in% c("P", "PRONTO SOCORRO", "URGENCIAS") ~ "Urgencias",
    x %in% c("O", "OUTROS", "OTROS")             ~ "Otros",
    is.na(x)                                     ~ NA_character_,
    TRUE                                         ~ "Otros"
  )
}

norm_binario_sn <- function(x) {
  x <- norm_texto(x)
  if_else(x %in% c("S", "SIM", "SI", "1", "Y"), 1L, 0L, missing = 0L)
}

parse_valor <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  if (any(grepl(",", x), na.rm = TRUE)) {
    x <- gsub("\\.", "", x)
    x <- gsub(",", ".", x)
  }
  suppressWarnings(as.numeric(x))
}

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
#  1. INGESTA Y LIMPIEZA TRANSACCIONAL
# ==============================================================================
db_raw <- read_csv(file.path(RUTA$datos, "db_2026.csv"),
                   show_col_types = FALSE, name_repair = "minimal")

db_limpia <- db_raw %>%
  mutate(
    CID_clean = str_replace_all(str_trim(CID), "\\.", ""),
    CID_grupo = str_sub(CID_clean, 1, 3),

    Fecha_Nac = suppressWarnings(parse_date_time(
      DT_NASCIMENTO_BENEFICIARIO,
      orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmy HMS"))),
    Fecha_Uso = suppressWarnings(parse_date_time(
      DT_UTILIZACAO,
      orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmy HMS"))),
    Anio_Nacimiento = year(Fecha_Nac),

    SEXO_BENEFICIARIO = norm_texto(SEXO_BENEFICIARIO),
    TIPO_BENEFICIARIO = norm_texto(TIPO_BENEFICIARIO),
    UF_HOSP           = norm_texto(UF_CNES_PREST_HOSPITALAR),
    TIPO_UNIDADE      = norm_texto(TIPO_UNIDADE_PREST_HOSPITALAR),
    PORTE_ANESTESICO  = norm_texto(PORTE_ANESTESICO),


    DESC_ESPECIALIDADE = norm_texto(DESC_ESPECIALIDADE),
    DESC_ESPECIALIDADE = if_else(is.na(DESC_ESPECIALIDADE),
                                 "NAO INFORMADA", DESC_ESPECIALIDADE),

    CETIPO_std = norm_cetipo(CETIPO),
    Flag_UTI   = norm_binario_sn(UTI),
    Flag_Inter = norm_binario_sn(INTERNADO),

    VALOR_UTILIZACAO = parse_valor(VALOR_UTILIZACAO)
  )

saveRDS(db_limpia, file.path(RUTA$resultados, "db_limpia.rds"))


# ==============================================================================
#  2. CALIDAD DE DATOS
# ==============================================================================

# ---- 2.1 Valores faltantes ----
faltantes <- db_limpia %>%
  summarise(across(everything(),
                   list(n_na = ~sum(is.na(.)), pct_na = ~mean(is.na(.)) * 100))) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"),
               names_pattern = "(.*)_(n_na|pct_na)") %>%
  arrange(desc(pct_na))
guardar_tabla(faltantes, "calidad_faltantes")

fig_faltantes <- faltantes %>%
  filter(pct_na > 0) %>%
  mutate(variable = fct_reorder(variable, pct_na)) %>%
  ggplot(aes(pct_na, variable)) +
  geom_col(fill = "#C44E52", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", pct_na)), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Valores faltantes por variable", x = "% faltante", y = NULL)
guardar_fig(fig_faltantes, "01_faltantes")

# ---- 2.2 Inconsistencias por beneficiario ----
inconsistencias_persona <- db_limpia %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    n_sexos     = n_distinct(SEXO_BENEFICIARIO[!is.na(SEXO_BENEFICIARIO)]),
    n_anios_nac = n_distinct(Anio_Nacimiento[!is.na(Anio_Nacimiento)]),
    n_tipos     = n_distinct(TIPO_BENEFICIARIO[!is.na(TIPO_BENEFICIARIO)]),
    .groups = "drop"
  )

resumen_inconsistencias <- tibble(
  inconsistencia = c("Beneficiarios con >1 SEXO",
                     "Beneficiarios con >1 A\u00d1O DE NACIMIENTO",
                     "Beneficiarios con >1 TIPO_BENEFICIARIO"),
  n_beneficiarios = c(sum(inconsistencias_persona$n_sexos     > 1),
                      sum(inconsistencias_persona$n_anios_nac > 1),
                      sum(inconsistencias_persona$n_tipos     > 1))
) %>%
  mutate(pct = n_beneficiarios / n_distinct(db_limpia$CHAVE_FUNCIONAL) * 100)
guardar_tabla(resumen_inconsistencias, "calidad_inconsistencias")

# ---- 2.3 Fechas y edades invalidas ----
diagnostico_fechas <- db_limpia %>%
  summarise(
    nac_no_parseables = sum(is.na(Fecha_Nac)),
    uso_no_parseables = sum(is.na(Fecha_Uso)),
    edad_negativa     = sum((ANIO_CORTE - Anio_Nacimiento) < 0, na.rm = TRUE),
    edad_mayor_110    = sum((ANIO_CORTE - Anio_Nacimiento) > 110, na.rm = TRUE)
  )
guardar_tabla(diagnostico_fechas, "calidad_fechas")

# ---- 2.4 Valores extremos en el costo ----
resumen_costo <- db_limpia %>%
  summarise(
    n = n(),
    n_na        = sum(is.na(VALOR_UTILIZACAO)),
    n_negativos = sum(VALOR_UTILIZACAO < 0, na.rm = TRUE),
    n_cero      = sum(VALOR_UTILIZACAO == 0, na.rm = TRUE),
    minimo      = min(VALOR_UTILIZACAO, na.rm = TRUE),
    mediana     = median(VALOR_UTILIZACAO, na.rm = TRUE),
    promedio    = mean(VALOR_UTILIZACAO, na.rm = TRUE),
    p95         = quantile(VALOR_UTILIZACAO, 0.95, na.rm = TRUE),
    p99         = quantile(VALOR_UTILIZACAO, 0.99, na.rm = TRUE),
    maximo      = max(VALOR_UTILIZACAO, na.rm = TRUE),
    desv_est    = sd(VALOR_UTILIZACAO, na.rm = TRUE)
  )
guardar_tabla(resumen_costo, "calidad_costo_resumen")

fig_costo <- db_limpia %>%
  filter(VALOR_UTILIZACAO > 0) %>%
  ggplot(aes(VALOR_UTILIZACAO)) +
  geom_histogram(bins = 60, fill = "#4C72B0", alpha = 0.85) +
  scale_x_log10(labels = label_number(big.mark = ".")) +
  labs(title = "Distribuci\u00f3n de VALOR_UTILIZACAO (escala log)",
       subtitle = "Asimetr\u00eda fuerte: cola larga de costos altos",
       x = "Costo por transacci\u00f3n (log10)", y = "Frecuencia")
guardar_fig(fig_costo, "02_costo_distribucion")


# ==============================================================================
#  3. VARIABLE OBJETIVO Y AGREGACION A NIVEL BENEFICIARIO
# ==============================================================================

# Etiqueta 1 si el beneficiario tiene al menos una transaccion con CID del grupo I50.
df_target <- db_limpia %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(I50 = as.integer(any(CID_grupo == CID_OBJETIVO, na.rm = TRUE)),
            .groups = "drop")

# Inconsistencias resueltas por moda; procedimientos y utilizaciones se cuentan
# por separado para evitar duplicados de granularidad.
pacientes <- db_limpia %>%
  group_by(CHAVE_FUNCIONAL) %>%
  summarise(
    Sexo              = moda(SEXO_BENEFICIARIO),
    Tipo_Beneficiario = moda(TIPO_BENEFICIARIO),
    UF_Principal      = moda(UF_HOSP),
    Anio_Nacimiento   = suppressWarnings(min(Anio_Nacimiento, na.rm = TRUE)),
    Edad              = ANIO_CORTE - Anio_Nacimiento,

    Num_Procedimientos = n(),
    Num_Utilizaciones  = n_distinct(paste(CHAVE_FUNCIONAL, Fecha_Uso)),
    Dias_Utilizacion   = n_distinct(Fecha_Uso),

    Num_Consultas         = sum(CETIPO_std == "Consulta",    na.rm = TRUE),
    Num_Examenes          = sum(CETIPO_std == "Examen",      na.rm = TRUE),
    Num_Terapias          = sum(CETIPO_std == "Terapia",     na.rm = TRUE),
    Num_Urgencias         = sum(CETIPO_std == "Urgencias",   na.rm = TRUE),
    Num_Hospitalizaciones = sum(CETIPO_std == "Internacion", na.rm = TRUE),
    Num_Otros_Servicios   = sum(CETIPO_std == "Otros",       na.rm = TRUE),

    Visitas_Clinica_Medica   = sum(DESC_ESPECIALIDADE == "CLINICA MEDICA", na.rm = TRUE),
    Visitas_Cardiologia      = sum(str_detect(coalesce(DESC_ESPECIALIDADE, ""),
                                              "CARDIOLOGIA"), na.rm = TRUE),
    Especialidades_Distintas = n_distinct(DESC_ESPECIALIDADE),

    Dias_UTI       = sum(Flag_UTI,   na.rm = TRUE),
    Dias_Internado = sum(Flag_Inter, na.rm = TRUE),

    Tipos_Unidad_Distintos = n_distinct(TIPO_UNIDADE),
    UFs_Distintas          = n_distinct(UF_HOSP),
    Con_Anestesia          = as.integer(any(!is.na(PORTE_ANESTESICO) &
                                            PORTE_ANESTESICO != "NAO INFORMADA")),

    Costo_Total = sum(VALOR_UTILIZACAO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Costo_Promedio_Dia = Costo_Total / pmax(Dias_Utilizacion, 1),
    Edad = if_else(!is.finite(Edad) | Edad < 0 | Edad > 110, NA_real_, Edad)
  ) %>%
  left_join(df_target, by = "CHAVE_FUNCIONAL")

cat("Beneficiarios:", nrow(pacientes),
    "| Positivos I50:", sum(pacientes$I50 == 1),
    "| Prevalencia:", sprintf("%.4f%%", mean(pacientes$I50) * 100), "\n")

saveRDS(pacientes, file.path(RUTA$resultados, "pacientes.rds"))

# Frame de modelado: faltantes como NA explicitos (los trata la receta, sin fuga).
# "Enfermo" queda como segundo nivel para coherencia con scale_pos_weight.
datos_modelo <- pacientes %>%
  transmute(
    CHAVE_FUNCIONAL,
    I50 = factor(I50, levels = c(0, 1), labels = c("Sano", "Enfermo")),
    Sexo = factor(Sexo),
    Tipo_Beneficiario = factor(Tipo_Beneficiario),
    UF_Principal = factor(UF_Principal),
    Edad,
    Num_Procedimientos, Num_Utilizaciones, Dias_Utilizacion,
    Num_Consultas, Num_Examenes, Num_Terapias, Num_Urgencias,
    Num_Hospitalizaciones, Num_Otros_Servicios,
    Visitas_Clinica_Medica, Visitas_Cardiologia, Especialidades_Distintas,
    Dias_UTI, Dias_Internado,
    Tipos_Unidad_Distintos, UFs_Distintas, Con_Anestesia,
    Costo_Total, Costo_Promedio_Dia
  )

saveRDS(datos_modelo, file.path(RUTA$resultados, "datos_modelo.rds"))


# ==============================================================================
#  4. ANALISIS DESCRIPTIVO
# ==============================================================================

# ---- 4.1 Volumetria ----
volumetria <- tibble(
  metrica = c("Beneficiarios", "Utilizaciones (CHAVE x fecha)",
              "Procedimientos (transacciones)"),
  valor = c(n_distinct(db_limpia$CHAVE_FUNCIONAL),
            n_distinct(paste(db_limpia$CHAVE_FUNCIONAL, db_limpia$Fecha_Uso)),
            nrow(db_limpia))
)
guardar_tabla(volumetria, "eda_volumetria")

# ---- 4.2 Distribucion del objetivo ----
dist_objetivo <- pacientes %>%
  count(I50) %>%
  mutate(clase = if_else(I50 == 1, "Enfermo", "Sano"), pct = n / sum(n) * 100)
guardar_tabla(dist_objetivo, "eda_distribucion_objetivo")

fig_objetivo <- dist_objetivo %>%
  ggplot(aes(clase, n, fill = clase)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(n)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = COLOR_CLASE, guide = "none") +
  scale_y_log10(labels = comma) +
  labs(title = "Distribuci\u00f3n de la variable objetivo (I50)",
       subtitle = sprintf("Prevalencia: %.4f%% \u2014 %d enfermos de %s beneficiarios",
                          mean(pacientes$I50) * 100, sum(pacientes$I50 == 1),
                          comma(nrow(pacientes))),
       x = NULL, y = "Beneficiarios (escala log)")
guardar_fig(fig_objetivo, "03_distribucion_objetivo")

# ---- 4.3 Tasa de I50 por categoria ----
tasa_por_categoria <- function(df, var, titulo, top = NULL) {
  d <- df %>%
    filter(!is.na({{ var }})) %>%
    group_by(cat = {{ var }}) %>%
    summarise(n = n(), enfermos = sum(I50 == 1), .groups = "drop") %>%
    mutate(tasa_10k = enfermos / n * 1e4)
  if (!is.null(top)) d <- d %>% slice_max(n, n = top)

  g <- d %>%
    mutate(cat = fct_reorder(factor(cat), tasa_10k)) %>%
    ggplot(aes(tasa_10k, cat)) +
    geom_col(fill = "#C44E52", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f (n=%s)", tasa_10k, comma(enfermos))),
              hjust = -0.05, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = titulo, subtitle = "Positivos I50 por cada 10.000 beneficiarios",
         x = "Tasa I50 (por 10k)", y = NULL)
  list(tabla = d, figura = g)
}

r_sexo <- tasa_por_categoria(pacientes, Sexo, "Tasa de I50 por sexo")
guardar_tabla(r_sexo$tabla, "eda_tasa_sexo")
guardar_fig(r_sexo$figura, "04_tasa_sexo", h = 3.5)

r_tipo <- tasa_por_categoria(pacientes, Tipo_Beneficiario,
                             "Tasa de I50 por tipo de beneficiario")
guardar_tabla(r_tipo$tabla, "eda_tasa_tipo")
guardar_fig(r_tipo$figura, "05_tasa_tipo")

r_uf <- tasa_por_categoria(pacientes, UF_Principal,
                           "Tasa de I50 por estado (UF)", top = 15)
guardar_tabla(r_uf$tabla, "eda_tasa_uf")
guardar_fig(r_uf$figura, "06_tasa_uf", h = 6)

pacientes_edad <- pacientes %>%
  filter(!is.na(Edad)) %>%
  mutate(Grupo_Edad = cut(Edad, breaks = c(0, 40, 50, 60, 70, 80, 120),
                          labels = c("0-40", "40-50", "50-60",
                                     "60-70", "70-80", "80+"),
                          right = FALSE))
r_edad <- tasa_por_categoria(pacientes_edad, Grupo_Edad,
                             "Tasa de I50 por grupo etario")
guardar_tabla(r_edad$tabla, "eda_tasa_edad")
guardar_fig(r_edad$figura, "07_tasa_edad", h = 3.5)

# ---- 4.4 Edad por clase ----
fig_edad <- pacientes %>%
  filter(!is.na(Edad)) %>%
  mutate(clase = if_else(I50 == 1, "Enfermo", "Sano")) %>%
  ggplot(aes(clase, Edad, fill = clase)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.15) +
  scale_fill_manual(values = COLOR_CLASE, guide = "none") +
  labs(title = "Edad seg\u00fan estado cl\u00ednico", x = NULL, y = "Edad (a\u00f1os)")
guardar_fig(fig_edad, "08_edad_por_clase", h = 4)

# ---- 4.5 Perfil numerico comparado (guia la seleccion de variables) ----
vars_numericas <- c("Num_Procedimientos", "Num_Utilizaciones", "Dias_Utilizacion",
                    "Num_Consultas", "Num_Examenes", "Num_Terapias",
                    "Num_Urgencias", "Num_Hospitalizaciones",
                    "Visitas_Clinica_Medica", "Visitas_Cardiologia",
                    "Especialidades_Distintas", "Dias_UTI", "Dias_Internado",
                    "Costo_Total", "Costo_Promedio_Dia")

perfil_numerico <- pacientes %>%
  mutate(clase = if_else(I50 == 1, "Enfermo", "Sano")) %>%
  group_by(clase) %>%
  summarise(across(all_of(vars_numericas), median), .groups = "drop") %>%
  pivot_longer(-clase, names_to = "variable", values_to = "mediana") %>%
  pivot_wider(names_from = clase, values_from = mediana) %>%
  mutate(razon_enf_sano = round(Enfermo / pmax(Sano, 0.01), 2)) %>%
  arrange(desc(razon_enf_sano))
guardar_tabla(perfil_numerico, "eda_perfil_numerico")

fig_perfil <- pacientes %>%
  mutate(clase = if_else(I50 == 1, "Enfermo", "Sano")) %>%
  select(clase, all_of(c("Num_Procedimientos", "Num_Hospitalizaciones",
                         "Visitas_Cardiologia", "Dias_Internado",
                         "Costo_Total", "Costo_Promedio_Dia"))) %>%
  pivot_longer(-clase, names_to = "variable", values_to = "valor") %>%
  ggplot(aes(clase, valor + 1, fill = clase)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.1) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = COLOR_CLASE, guide = "none") +
  labs(title = "Perfil de utilizaci\u00f3n y costo: Sano vs Enfermo",
       subtitle = "Escala log (valor + 1)", x = NULL, y = "Valor (log)")
guardar_fig(fig_perfil, "09_perfil_utilizacion", w = 9, h = 6)

# ---- 4.6 Especialidades entre enfermos ----
esp_enfermos <- db_limpia %>%
  inner_join(select(pacientes, CHAVE_FUNCIONAL, I50), by = "CHAVE_FUNCIONAL") %>%
  filter(I50 == 1, !is.na(DESC_ESPECIALIDADE)) %>%
  count(DESC_ESPECIALIDADE, sort = TRUE, name = "atenciones") %>%
  mutate(pct = atenciones / sum(atenciones) * 100) %>%
  slice_head(n = 12)
guardar_tabla(esp_enfermos, "eda_especialidades_enfermos")

fig_esp <- esp_enfermos %>%
  mutate(DESC_ESPECIALIDADE = fct_reorder(DESC_ESPECIALIDADE, atenciones)) %>%
  ggplot(aes(atenciones, DESC_ESPECIALIDADE)) +
  geom_col(fill = "#4C72B0", alpha = 0.85) +
  scale_x_continuous(labels = comma) +
  labs(title = "Especialidades m\u00e1s consultadas por enfermos I50",
       x = "Atenciones", y = NULL)
guardar_fig(fig_esp, "10_especialidades_enfermos", h = 5)

# ---- 4.7 Correlacion  ----
mat_cor <- pacientes %>%
  select(all_of(vars_numericas)) %>%
  cor(use = "pairwise.complete.obs")

fig_cor <- as.data.frame(as.table(mat_cor)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.5) +
  scale_fill_gradient2(low = "#4C72B0", mid = "white", high = "#C44E52",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlaci\u00f3n entre predictores num\u00e9ricos", x = NULL, y = NULL)
guardar_fig(fig_cor, "11_correlacion", w = 9, h = 8)

