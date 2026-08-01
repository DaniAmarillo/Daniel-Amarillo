#------------------------------------------------------------
# TALLER 3 MINERÍA DE DATOS
#------------------------------------------------------------

# Variable seleccionada: predicción de cálculo renal / urinario mediante CID N20.
# Unidad de clasificación: beneficiario identificado por CHAVE_FUNCIONAL.
# Unidad de costo: utilización N20 aproximada por CHAVE_FUNCIONAL + fecha_utilizacion.
#
# Este script está estructurado para satisfacer las instrucciones del taller p3_md.pdf:
# 1) Código reproducible para la preparación de datos.
# 2) Código de construcción de la variable objetivo.
# 3) Código de análisis descriptivo.
# 4) Código de entrenamiento de modelos.
# 5) Código de evaluación y comparación de modelos.
# 6) Código o salidas de interpretación de variables importantes.
# 7) Código o resultados de estimación del costo esperado.
#
# Nota para su correcto uso:
# Ejecutar desde la carpeta donde se encuentra db_2026.csv.
# Las salidas finales se guardan en salidas/ y alimentan el Rmd final. Por lo que
# se debe ejecutar este script primero antes de generar el archivo Rmd.
# Abrir y guardar este archivo como UTF-8 para evitar posibles errores de ejecución.

#------------------------------------------------------------
# 0. Parámetros generales
#------------------------------------------------------------

# Rutas principales del proyecto.
archivo_base <- "db_2026.csv"  # Archivo transaccional principal; debe estar en el directorio de trabajo.
carpeta_salida <- "salidas"  # Carpeta única donde se concentran los resultados finales del flujo.
carpeta_figuras <- file.path(carpeta_salida, "figuras")  # Subcarpeta para guardar gráficos generados por ggplot2.
carpeta_modelos <- file.path(carpeta_salida, "modelos")  # Subcarpeta opcional para guardar objetos RDS si se activa guardar_modelos.
ruta_sqlite_taller3 <- file.path(carpeta_salida, "taller3_resultados.sqlite")  # Archivo SQLite con tablas resumidas del análisis.

# Definición clínica y unidad de análisis del taller.
nombre_enfermedad <- "Cálculo renal / urinario"  # Nombre interpretativo de la condición clínica seleccionada.
codigo_cid <- "N20"  # Código CID base asociado a cálculo renal / urinario.
patron_cid <- "^N20"  # Patrón regular que captura N20 y sus subcódigos.
variable_objetivo <- "tiene_calculo_renal"  # Nombre de la etiqueta binaria a nivel beneficiario.
unidad_modelo <- "CHAVE_FUNCIONAL"  # Unidad de análisis usada para clasificación.

# Parámetros de modelamiento y reproducibilidad.
semilla <- 2026  # Semilla para reproducibilidad de particiones y modelos.
prop_train <- 0.80  # Proporción de datos destinada al entrenamiento.
n_arboles_rf <- 300  # Número de árboles del random forest.
max_categorias_modelo <- 12  # Límite para agrupar categorías poco frecuentes.
max_filas_csv_utilizaciones <- Inf  # Permite exportar todas las utilizaciones N20 si no se define límite.

# Opciones de salida del análisis.
crear_sqlite <- TRUE  # Activa la escritura de tablas resumidas en SQLite.
crear_graficos <- TRUE  # Activa la generación de gráficos finales.
guardar_modelos <- FALSE  # Evita guardar objetos pesados RDS salvo que se cambie a TRUE.
mostrar_resumen_consola <- TRUE  # Imprime al final un resumen corto de ejecución.

# Opciones para limitar el tamaño de las tablas en SQLite.
guardar_base_raw_sqlite <- FALSE  # Evita guardar la base cruda completa en SQLite para ahorrar espacio.
guardar_base_cid_sqlite <- FALSE  # Evita guardar la base CID completa en SQLite para controlar tamaño.

set.seed(semilla)  # Fija reproducibilidad global del flujo.
options(width = 140, scipen = 999)
options(error = NULL) # Restablece el manejo normal de errores de la sesión.

#------------------------------------------------------------
# 0.1 Bibliotecas requeridas
#------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  DBI,
  RSQLite,
  readr,
  dplyr,
  tidyr,
  stringr,
  stringi,
  tibble,
  purrr,
  lubridate,
  forcats,
  ggplot2,
  scales,
  rsample,
  recipes,
  yardstick,
  rpart,
  ranger,
  broom
)

#------------------------------------------------------------
# 0.2 Funciones auxiliares
#------------------------------------------------------------

# Funciones de escritura, lectura y consulta de resultados.
crear_carpetas <- function(...) {  # Crea las carpetas necesarias sin detenerse si ya existen.
  carpetas <- c(...)
  invisible(purrr::walk(carpetas, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)))
}

preparar_para_sqlite <- function(datos) {  # Convierte fechas y lógicos a formatos compatibles con SQLite.
  datos |>
    dplyr::mutate(
      dplyr::across(where(~ inherits(.x, "Date")), as.character),
      dplyr::across(where(is.logical), as.integer)
    )
}

guardar_tabla_sqlite <- function(datos, tabla, overwrite = TRUE, append = FALSE) {  # Guarda una tabla en SQLite cuando esta salida está activa.
  if (!crear_sqlite) return(invisible(NULL))
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_sqlite_taller3)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(
    con,
    name = tabla,
    value = preparar_para_sqlite(datos),
    overwrite = overwrite,
    append = append
  )
  invisible(TRUE)
}

consultar_sqlite <- function(sql) {  # Ejecuta consultas SQL sobre el SQLite de resultados.
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_sqlite_taller3)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql) |> tibble::as_tibble()
}

tabla_sqlite_existe <- function(tabla) {  # Verifica si una tabla ya existe dentro del SQLite.
  if (!crear_sqlite || is.na(tabla) || !nzchar(as.character(tabla))) return(FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_sqlite_taller3)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExistsTable(con, as.character(tabla))
}

contar_sqlite <- function(tabla) {  # Cuenta registros de una tabla SQLite existente.
  if (!tabla_sqlite_existe(tabla)) return(NA_integer_)
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_sqlite_taller3)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- paste0("SELECT COUNT(*) AS n FROM ", DBI::dbQuoteIdentifier(con, as.character(tabla)))
  as.integer(DBI::dbGetQuery(con, sql)$n[1])
}

guardar_csv <- function(datos, archivo) {  # Exporta resultados a CSV dentro de carpeta_salida.
  readr::write_csv(datos, file.path(carpeta_salida, archivo), na = "")
  invisible(datos)
}

guardar_rds <- function(objeto, archivo) {  # Guarda objetos RDS solo cuando guardar_modelos está activo.
  if (!guardar_modelos) return(invisible(NULL))
  saveRDS(objeto, file.path(carpeta_modelos, archivo))
  invisible(TRUE)
}

validar_columnas <- function(datos, columnas, nombre_objeto = "datos") {  # Detiene el flujo si faltan variables obligatorias.
  faltantes <- setdiff(columnas, names(datos))
  if (length(faltantes) > 0) {
    stop(nombre_objeto, " no contiene columnas requeridas: ", paste(faltantes, collapse = ", "))
  }
  invisible(TRUE)
}

# Valores textuales que se tratarán como datos no informados.
valores_no_informados <- c(
  "", "N/A", "NA", "NAN", "NULL", "NULO", "-", "--", ".", "S/I", "S/D",
  "SIN INFORMACION", "SIN INFORMACION", "SEM INFORMACAO", "SEM INFORMACAO",
  "SIN DATO", "SIN DATOS", "NO INFORMADO", "NO INFORMA", "NO APLICA",
  "SEM CID", "SIN CID", "NAO INFORMADO", "NAO INFORMADO", "NAO INFORMA",
  "NAO INFORMA", "IGNORADO", "IGNORADA", "DESCONHECIDO", "DESCONHECIDA",
  "SEM DADO", "SEM DADOS"
) |> unique()

# Funciones de limpieza de texto, diagnósticos, fechas y números.
normalizar_texto <- function(x) {  # Estandariza texto para comparar categorías y valores no informados.
  x_txt <- as.character(x) |>
    stringr::str_replace_all("\u00a0", " ") |>
    stringr::str_squish() |>
    stringr::str_to_upper()
  x_txt <- stringi::stri_trans_general(x_txt, "Latin-ASCII")
  dplyr::if_else(is.na(x_txt) | x_txt %in% valores_no_informados, NA_character_, x_txt)
}

normalizar_cid <- function(x) {  # Limpia el campo CID para identificar diagnósticos N20.
  x_txt <- normalizar_texto(x)
  x_txt <- x_txt |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_replace_all("\\s+", "") |>
    stringr::str_replace_all("[^A-Z0-9]", "")
  dplyr::if_else(is.na(x_txt) | x_txt == "", NA_character_, x_txt)
}

convertir_fecha_segura <- function(x) {  # Convierte fechas en varios formatos sin detener el flujo por errores.
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  x_txt <- normalizar_texto(x)
  fecha <- suppressWarnings(lubridate::parse_date_time(
    x_txt,
    orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY"),
    quiet = TRUE
  ))
  as.Date(fecha)
}

convertir_numero_seguro <- function(x) {  # Convierte costos a número manejando coma decimal y separadores.
  if (is.numeric(x)) return(as.numeric(x))
  x_txt <- as.character(x) |>
    stringr::str_replace_all("\u00a0", " ") |>
    stringr::str_squish()
  x_txt <- dplyr::if_else(is.na(x_txt) | stringr::str_to_upper(x_txt) %in% valores_no_informados, NA_character_, x_txt)
  decimal_coma <- stringr::str_detect(x_txt, "\\.\\d{3},\\d+$")
  x_txt <- dplyr::if_else(decimal_coma, stringr::str_replace_all(x_txt, "\\.", "") |> stringr::str_replace(",", "."), x_txt)
  coma_simple <- stringr::str_detect(x_txt, ",") & !stringr::str_detect(x_txt, "\\.")
  x_txt <- dplyr::if_else(coma_simple, stringr::str_replace(x_txt, ",", "."), x_txt)
  suppressWarnings(readr::parse_number(x_txt, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")))
}

normalizar_binaria <- function(x) {  # Convierte variables tipo sí/no a 1, 0 o NA.
  x_txt <- normalizar_texto(x)
  dplyr::case_when(
    x_txt %in% c("1", "SIM", "S", "YES", "Y", "TRUE", "T") ~ 1L,
    x_txt %in% c("0", "NAO", "NAO", "N", "NO", "FALSE", "F") ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Funciones de resumen usadas durante las agregaciones.
moda_segura <- function(x, etiqueta_na = "NO_INFORMADO") {  # Obtiene la categoría más frecuente con salida segura si todo falta.
  x_chr <- as.character(x)
  x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
  if (length(x_chr) == 0) return(etiqueta_na)
  names(sort(table(x_chr), decreasing = TRUE))[1]
}

sd_segura <- function(x) {  # Calcula desviación estándar evitando errores con pocos datos.
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(0)
  stats::sd(x)
}

mediana_segura <- function(x, defecto = 0) {  # Calcula mediana y devuelve un valor por defecto si no hay datos válidos.
  x <- as.numeric(x)
  valor <- suppressWarnings(stats::median(x, na.rm = TRUE))
  if (length(valor) == 0 || is.na(valor) || is.nan(valor) || is.infinite(valor)) return(defecto)
  valor
}

promedio_seguro <- function(x, defecto = 0) {  # Calcula promedio y controla NA, NaN o infinitos.
  x <- as.numeric(x)
  valor <- suppressWarnings(mean(x, na.rm = TRUE))
  if (length(valor) == 0 || is.na(valor) || is.nan(valor) || is.infinite(valor)) return(defecto)
  valor
}

maximo_seguro <- function(x, defecto = 0) {  # Calcula máximo y controla vectores sin valores válidos.
  x <- as.numeric(x)
  valor <- suppressWarnings(max(x, na.rm = TRUE))
  if (length(valor) == 0 || is.na(valor) || is.nan(valor) || is.infinite(valor)) return(defecto)
  valor
}

# Funciones usadas en preprocesamiento, evaluación y tratamiento de valores extremos.
extraer_prob_evento <- function(predicciones, evento = "Con N20") {  # Extrae la probabilidad de la clase positiva desde distintos modelos.
  if (is.null(predicciones)) return(NULL)
  if (is.vector(predicciones) && is.numeric(predicciones)) return(as.numeric(predicciones))
  pred_df <- as.data.frame(predicciones, check.names = FALSE)
  nombres <- colnames(pred_df)
  if (evento %in% nombres) return(as.numeric(pred_df[[evento]]))
  evento_make <- make.names(evento)
  if (evento_make %in% make.names(nombres)) {
    return(as.numeric(pred_df[[which(make.names(nombres) == evento_make)[1]]]))
  }
  if (ncol(pred_df) >= 2) return(as.numeric(pred_df[[ncol(pred_df)]]))
  as.numeric(pred_df[[1]])
}

winsorizar_p99 <- function(x) {  # Recorta valores extremos al percentil 99 para reducir impacto de outliers.
  x_num <- as.numeric(x)
  p99 <- suppressWarnings(as.numeric(stats::quantile(x_num, 0.99, na.rm = TRUE)))
  if (is.na(p99) || is.nan(p99) || is.infinite(p99)) return(x_num)
  pmin(x_num, p99)
}

agrupar_top <- function(x, n_top = max_categorias_modelo, etiqueta_otro = "OTRO", etiqueta_na = "NO_INFORMADO") {  # Conserva categorías frecuentes y agrupa el resto como OTRO.
  x_chr <- as.character(x)
  x_chr <- dplyr::if_else(is.na(x_chr) | x_chr == "", etiqueta_na, x_chr)
  frec <- sort(table(x_chr), decreasing = TRUE)
  top <- names(frec)[seq_len(min(n_top, length(frec)))]
  dplyr::if_else(x_chr %in% top, x_chr, etiqueta_otro)
}

calcular_metrica_segura <- function(expr) {  # Ejecuta métricas yardstick evitando que un caso degenerado detenga el flujo.
  tryCatch(
    suppressWarnings(as.numeric(expr)),
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )
}

metricas_clasificacion <- function(datos, prob_col, umbral, modelo = NA_character_) {  # Calcula métricas de clasificación usando probabilidades y un umbral.
  datos_eval <- datos |>
    dplyr::mutate(
      prob_evento = suppressWarnings(as.numeric(.data[[prob_col]])),  # Probabilidad del evento positivo.
      pred_clase = factor(dplyr::if_else(prob_evento >= umbral, "Con N20", "Sin N20"), levels = c("Sin N20", "Con N20")),
      verdad = factor(tiene_calculo_renal_factor, levels = c("Sin N20", "Con N20"))
    ) |>
    dplyr::filter(is.finite(prob_evento), !is.na(verdad))
  
  if (nrow(datos_eval) == 0 || dplyr::n_distinct(datos_eval$verdad) < 2) {
    return(tibble::tibble(
      modelo = modelo, umbral = as.numeric(umbral), roc_auc = NA_real_, pr_auc = NA_real_,
      accuracy = NA_real_, sens = NA_real_, spec = NA_real_, precision = NA_real_, f_meas = NA_real_
    ))
  }
  
  tibble::tibble(
    modelo = modelo,
    umbral = as.numeric(umbral),
    roc_auc = calcular_metrica_segura(yardstick::roc_auc_vec(datos_eval$verdad, datos_eval$prob_evento, event_level = "second")),
    pr_auc = calcular_metrica_segura(yardstick::pr_auc_vec(datos_eval$verdad, datos_eval$prob_evento, event_level = "second")),
    accuracy = calcular_metrica_segura(yardstick::accuracy_vec(datos_eval$verdad, datos_eval$pred_clase)),
    sens = calcular_metrica_segura(yardstick::sens_vec(datos_eval$verdad, datos_eval$pred_clase, event_level = "second")),
    spec = calcular_metrica_segura(yardstick::spec_vec(datos_eval$verdad, datos_eval$pred_clase, event_level = "second")),
    precision = calcular_metrica_segura(yardstick::precision_vec(datos_eval$verdad, datos_eval$pred_clase, event_level = "second")),
    f_meas = calcular_metrica_segura(yardstick::f_meas_vec(datos_eval$verdad, datos_eval$pred_clase, event_level = "second"))
  )
}

matriz_confusion_compacta <- function(datos, prob_col, umbral) {  # Construye matriz de confusión y etiqueta cada tipo de error/acierto.
  datos |>
    dplyr::mutate(
      prob_evento = as.numeric(.data[[prob_col]]),  # Toma la probabilidad del evento desde la columna indicada.
      pred_clase = factor(dplyr::if_else(prob_evento >= umbral, "Con N20", "Sin N20"), levels = c("Sin N20", "Con N20")),  # Convierte probabilidad en clase usando el umbral.
      verdad = factor(tiene_calculo_renal_factor, levels = c("Sin N20", "Con N20"))  # Estandariza la clase real para calcular métricas.
    ) |>
    dplyr::count(verdad, pred_clase, name = "n") |>
    tidyr::complete(verdad, pred_clase, fill = list(n = 0L)) |>
    dplyr::mutate(tipo_resultado = dplyr::case_when(
      verdad == "Con N20" & pred_clase == "Con N20" ~ "Verdadero positivo",
      verdad == "Con N20" & pred_clase == "Sin N20" ~ "Falso negativo",
      verdad == "Sin N20" & pred_clase == "Con N20" ~ "Falso positivo",
      verdad == "Sin N20" & pred_clase == "Sin N20" ~ "Verdadero negativo",
      TRUE ~ "Otro"
    ))
}

resumir_costo <- function(x) {  # Resume distribución de costos con media, mediana y percentiles.
  x <- as.numeric(x)
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) {
    return(tibble::tibble(
      n = 0L, total = 0, promedio = NA_real_, mediana = NA_real_, desviacion = NA_real_,
      minimo = NA_real_, p25 = NA_real_, p75 = NA_real_, p90 = NA_real_,
      p95 = NA_real_, p99 = NA_real_, maximo = NA_real_
    ))
  }
  tibble::tibble(
    n = length(x_ok),
    total = sum(x_ok, na.rm = TRUE),
    promedio = mean(x_ok, na.rm = TRUE),
    mediana = stats::median(x_ok, na.rm = TRUE),
    desviacion = sd_segura(x_ok),
    minimo = min(x_ok, na.rm = TRUE),
    p25 = as.numeric(stats::quantile(x_ok, 0.25, na.rm = TRUE)),
    p75 = as.numeric(stats::quantile(x_ok, 0.75, na.rm = TRUE)),
    p90 = as.numeric(stats::quantile(x_ok, 0.90, na.rm = TRUE)),
    p95 = as.numeric(stats::quantile(x_ok, 0.95, na.rm = TRUE)),
    p99 = as.numeric(stats::quantile(x_ok, 0.99, na.rm = TRUE)),
    maximo = max(x_ok, na.rm = TRUE)
  )
}

#------------------------------------------------------------
# 1. Preparación de datos
#------------------------------------------------------------

crear_carpetas(carpeta_salida, carpeta_figuras, carpeta_modelos)  # Asegura que existan las carpetas de salida antes de guardar archivos.

if (!file.exists(archivo_base)) {
  stop("No se encontró ", archivo_base, ". Revise que db_2026.csv esté en el directorio de trabajo o ajuste archivo_base.")
}

# Cargar la base original antes de cualquier transformación.
base_raw <- readr::read_csv(archivo_base, show_col_types = FALSE, guess_max = 100000)  # Carga la base transaccional completa desde CSV.

# Columnas mínimas requeridas para diagnóstico, modelamiento y costos.
variables_requeridas <- c(
  "CID", "CHAVE_FUNCIONAL", "DT_UTILIZACAO", "VALOR_UTILIZACAO", "UTI", "INTERNADO",
  "PORTE_ANESTESICO", "DESC_ESPECIALIDADE", "TIPO_UNIDADE_PREST_HOSPITALAR",
  "UF_CNES_PREST_HOSPITALAR", "DT_NASCIMENTO_BENEFICIARIO", "TIPO_BENEFICIARIO",
  "SEXO_BENEFICIARIO", "CETIPO", "CD_PROCEDIMENTO", "DESCRICAO_PROCEDIMENTO"
)
validar_columnas(base_raw, variables_requeridas, "base_raw")  # Verifica que estén las columnas necesarias para el taller.

if (crear_sqlite && guardar_base_raw_sqlite) guardar_tabla_sqlite(base_raw, "base_raw", overwrite = TRUE)  # Guarda la base cruda completa si se activa ese respaldo.

# Se crea una base transaccional limpia: CID normalizado, fechas, costos y banderas clínicas.
base_cid <- base_raw |>
  dplyr::mutate(
    CID_NORM = normalizar_cid(CID),  # Normaliza el diagnóstico para hacer búsquedas consistentes.
    CID_VALIDO = !is.na(CID_NORM),  # Marca registros con diagnóstico aprovechable.
    es_cid_n20 = CID_VALIDO & stringr::str_detect(CID_NORM, patron_cid),  # Identifica registros cuyo CID empieza por N20.
    flag_n20 = as.integer(es_cid_n20),  # Convierte la marca N20 a variable binaria.
    fecha_utilizacion = convertir_fecha_segura(DT_UTILIZACAO),  # Convierte la fecha de utilización a Date.
    fecha_nacimiento = convertir_fecha_segura(DT_NASCIMENTO_BENEFICIARIO),  # Convierte la fecha de nacimiento a Date.
    fecha_nacimiento_posterior_uso = !is.na(fecha_nacimiento) & !is.na(fecha_utilizacion) & fecha_nacimiento > fecha_utilizacion,
    fecha_nacimiento_1900 = !is.na(fecha_nacimiento) & fecha_nacimiento <= as.Date("1900-01-01"),
    fecha_nacimiento_valida_edad = !is.na(fecha_nacimiento) & !fecha_nacimiento_posterior_uso & !fecha_nacimiento_1900,
    valor_utilizacion_num = convertir_numero_seguro(VALOR_UTILIZACAO),  # Convierte el valor de utilización a numérico.
    valor_utilizacion_cero = !is.na(valor_utilizacion_num) & valor_utilizacion_num == 0,
    valor_utilizacion_negativo = !is.na(valor_utilizacion_num) & valor_utilizacion_num < 0,
    valor_utilizacion_positivo = !is.na(valor_utilizacion_num) & valor_utilizacion_num > 0,
    UTI_BIN = normalizar_binaria(UTI),  # Normaliza indicador de UTI a 1/0.
    INTERNADO_BIN = normalizar_binaria(INTERNADO),  # Normaliza indicador de internación a 1/0.
    porte_anestesico_num = convertir_numero_seguro(PORTE_ANESTESICO),  # Convierte porte anestésico a valor numérico.
    sexo_norm = normalizar_texto(SEXO_BENEFICIARIO),
    sexo_norm = dplyr::case_when(sexo_norm %in% c("F", "FEMININO") ~ "F", sexo_norm %in% c("M", "MASCULINO") ~ "M", TRUE ~ sexo_norm),
    tipo_beneficiario_norm = normalizar_texto(TIPO_BENEFICIARIO),
    cetipo_norm = normalizar_texto(CETIPO),
    desc_especialidade_norm = normalizar_texto(DESC_ESPECIALIDADE),
    tipo_unidade_prest_hospitalar_norm = normalizar_texto(TIPO_UNIDADE_PREST_HOSPITALAR),
    uf_cnes_prest_hospitalar_norm = normalizar_texto(UF_CNES_PREST_HOSPITALAR),
    descricao_procedimento_norm = normalizar_texto(DESCRICAO_PROCEDIMENTO)
  )

# Crear una base resumida con las variables clave ya normalizadas.
base_cid_sqlite <- base_cid |>
  dplyr::transmute(
    CHAVE_FUNCIONAL,
    CID,
    CID_NORM,
    CID_VALIDO = as.integer(CID_VALIDO),
    CID_N20 = flag_n20,
    es_cid_n20 = flag_n20,
    flag_n20,
    fecha_utilizacion = as.character(fecha_utilizacion),
    fecha_nacimiento = as.character(fecha_nacimiento),
    fecha_nacimiento_valida_edad = as.integer(fecha_nacimiento_valida_edad),
    CD_PROCEDIMENTO,
    DESCRICAO_PROCEDIMENTO,
    valor_utilizacion_num,
    valor_utilizacion_cero = as.integer(valor_utilizacion_cero),
    valor_utilizacion_negativo = as.integer(valor_utilizacion_negativo),
    valor_utilizacion_positivo = as.integer(valor_utilizacion_positivo),
    UTI_BIN,
    INTERNADO_BIN,
    porte_anestesico_num,
    sexo_norm,
    tipo_beneficiario_norm,
    cetipo_norm,
    desc_especialidade_norm,
    tipo_unidade_prest_hospitalar_norm,
    uf_cnes_prest_hospitalar_norm,
    descricao_procedimento_norm
  )
if (crear_sqlite && guardar_base_cid_sqlite) guardar_tabla_sqlite(base_cid_sqlite, "base_cid_limpia", overwrite = TRUE)  # Guarda la base CID detallada si se requiere revisar registros normalizados.

#------------------------------------------------------------
# 2.1 Variable objetivo binaria
#------------------------------------------------------------

# Se agregan registros a nivel beneficiario para construir la variable objetivo.
# Un beneficiario toma valor 1 si presenta al menos una transacción compatible con CID N20.
objetivo_beneficiario <- base_cid |>
  dplyr::group_by(CHAVE_FUNCIONAL) |>
  dplyr::summarise(
    n_registros_beneficiario = dplyr::n(),  # Cuenta filas transaccionales del beneficiario.
    n_registros_cid_valido = sum(CID_VALIDO, na.rm = TRUE),  # Cuenta registros con diagnóstico válido.
    n_registros_n20 = sum(flag_n20 == 1, na.rm = TRUE),  # Cuenta registros N20 del beneficiario.
    tiene_cid_valido = as.integer(any(CID_VALIDO, na.rm = TRUE)),  # Indica si el beneficiario tuvo algún CID válido.
    tiene_calculo_renal = as.integer(any(flag_n20 == 1, na.rm = TRUE)),  # Etiqueta objetivo: 1 si tuvo al menos un registro N20.
    .groups = "drop"
  )
guardar_tabla_sqlite(objetivo_beneficiario, "objetivo_beneficiario", overwrite = TRUE)  # Guarda la etiqueta final por beneficiario.

# Resumir los conteos principales de preparación y construcción diagnóstica.
control_preparacion <- tibble::tibble(
  elemento = c(
    "archivo_base", "filas_base", "columnas_base", "beneficiarios", "registros_cid_valido",
    "registros_n20", "beneficiarios_n20", "patron_cid", "campo_diagnostico", "campo_procedimiento_no_diagnostico"
  ),
  valor = c(
    archivo_base,
    as.character(nrow(base_raw)),
    as.character(ncol(base_raw)),
    as.character(dplyr::n_distinct(base_raw$CHAVE_FUNCIONAL)),
    as.character(sum(base_cid$CID_VALIDO, na.rm = TRUE)),
    as.character(sum(base_cid$flag_n20 == 1, na.rm = TRUE)),
    as.character(sum(objetivo_beneficiario$tiene_calculo_renal == 1, na.rm = TRUE)),
    patron_cid,
    "CID_NORM",
    "CD_PROCEDIMENTO"
  )
)
guardar_csv(control_preparacion, "compacto_01_control_preparacion_cid_objetivo.csv")  # Exporta el primer control de preparación y diagnóstico N20.

# Resumen de granularidad.
# Documenta la diferencia entre beneficiario, utilización aproximada y registro/procedimiento.
# Calcular unidades aproximadas de atención para separar registros, utilizaciones y beneficiarios.
n_utilizaciones_aprox_compacto <- dplyr::n_distinct(paste(base_cid$CHAVE_FUNCIONAL, base_cid$fecha_utilizacion, sep = "__"))
n_utilizaciones_n20_aprox_compacto <- base_cid |>
  dplyr::filter(flag_n20 == 1) |>
  dplyr::distinct(CHAVE_FUNCIONAL, fecha_utilizacion) |>
  nrow()

# Documentar los niveles de análisis usados en el taller.
control_granularidad_compacto <- tibble::tibble(
  nivel = c(
    "Beneficiario",
    "Utilización aproximada",
    "Registro/procedimiento",
    "Procedimiento distinto",
    "Registro N20",
    "Utilización N20 aproximada",
    "Beneficiario N20"
  ),
  definicion_operativa = c(
    "CHAVE_FUNCIONAL",
    "CHAVE_FUNCIONAL + fecha_utilización",
    "Fila de la base transaccional",
    "CD_PROCEDIMENTO",
    "Fila con CID_NORM LIKE 'N20%'",
    "CHAVE_FUNCIONAL + fecha_utilización dentro de registros N20",
    "CHAVE_FUNCIONAL con al menos un registro N20"
  ),
  conteo = c(
    dplyr::n_distinct(base_cid$CHAVE_FUNCIONAL),
    n_utilizaciones_aprox_compacto,
    nrow(base_cid),
    dplyr::n_distinct(base_cid$CD_PROCEDIMENTO, na.rm = TRUE),
    sum(base_cid$flag_n20 == 1, na.rm = TRUE),
    n_utilizaciones_n20_aprox_compacto,
    sum(objetivo_beneficiario$tiene_calculo_renal == 1, na.rm = TRUE)
  ),
  uso_metodologico = c(
    "Unidad de clasificación y modelamiento.",
    "Unidad aproximada de atención; sirve para interpretar frecuencia de uso.",
    "No se usa como unidad de modelamiento porque puede duplicar atenciones.",
    "Catálogo de procedimientos/servicios; no define diagnóstico.",
    "Base transaccional usada para identificar eventos N20.",
    "Unidad usada luego para costo observado y esperado N20.",
    "Clase positiva de la variable objetivo."
  )
)

# Calcular razones que ayudan a interpretar intensidad transaccional y posible duplicidad.
control_ratios_granularidad_compacto <- tibble::tibble(
  indicador = c(
    "registros_por_beneficiario",
    "utilizaciones_por_beneficiario",
    "registros_por_utilizacion_aproximada",
    "registros_n20_por_beneficiario_n20",
    "registros_n20_por_utilizacion_n20"
  ),
  valor = c(
    nrow(base_cid) / dplyr::n_distinct(base_cid$CHAVE_FUNCIONAL),
    n_utilizaciones_aprox_compacto / dplyr::n_distinct(base_cid$CHAVE_FUNCIONAL),
    nrow(base_cid) / n_utilizaciones_aprox_compacto,
    sum(base_cid$flag_n20 == 1, na.rm = TRUE) / sum(objetivo_beneficiario$tiene_calculo_renal == 1, na.rm = TRUE),
    sum(base_cid$flag_n20 == 1, na.rm = TRUE) / n_utilizaciones_n20_aprox_compacto
  ),
  interpretacion = c(
    "Promedio de filas/procedimientos por beneficiario.",
    "Promedio de utilizaciones aproximadas por beneficiario.",
    "Promedio de filas por utilización aproximada.",
    "Intensidad transaccional N20 entre beneficiarios positivos.",
    "Número medio de registros N20 por utilización N20 aproximada."
  )
)

guardar_csv(control_granularidad_compacto, "compacto_01b_control_granularidad.csv")  # Exporta conteos por nivel metodológico.
guardar_csv(control_ratios_granularidad_compacto, "compacto_01c_ratios_granularidad.csv")  # Exporta ratios para revisar duplicidad e intensidad transaccional.
guardar_tabla_sqlite(control_granularidad_compacto, "compacto_control_granularidad", overwrite = TRUE)
guardar_tabla_sqlite(control_ratios_granularidad_compacto, "compacto_ratios_granularidad", overwrite = TRUE)


#------------------------------------------------------------
# 2. Variable objetivo y tabla analítica
#------------------------------------------------------------

#------------------------------------------------------------
# 2.2 Agregación a nivel de beneficiario
#------------------------------------------------------------

# Función para tomar la categoría dominante de cada beneficiario en variables repetidas.
crear_moda_beneficiario <- function(datos, variable, nombre_salida) {
  salida <- datos |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::count(CHAVE_FUNCIONAL, valor_moda = .data[[variable]], name = "n_moda") |>
    dplyr::arrange(CHAVE_FUNCIONAL, dplyr::desc(n_moda), valor_moda) |>
    dplyr::group_by(CHAVE_FUNCIONAL) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(CHAVE_FUNCIONAL, valor_moda)
  names(salida)[names(salida) == "valor_moda"] <- nombre_salida
  salida
}

# Obtener categorías dominantes por beneficiario para variables demográficas y de atención.
sexo_beneficiario <- crear_moda_beneficiario(base_cid, "sexo_norm", "sexo_dominante")
tipo_beneficiario <- crear_moda_beneficiario(base_cid, "tipo_beneficiario_norm", "tipo_beneficiario_dominante")
cetipo_dominante <- crear_moda_beneficiario(base_cid, "cetipo_norm", "cetipo_dominante")
especialidad_dominante <- crear_moda_beneficiario(base_cid, "desc_especialidade_norm", "especialidad_dominante")
estado_prestador_dominante <- crear_moda_beneficiario(base_cid, "uf_cnes_prest_hospitalar_norm", "estado_prestador_dominante")
tipo_unidad_dominante <- crear_moda_beneficiario(base_cid, "tipo_unidade_prest_hospitalar_norm", "tipo_unidad_dominante")

# Elegir una fecha de nacimiento válida por beneficiario para calcular edad.
fecha_nacimiento_beneficiario <- base_cid |>
  dplyr::filter(fecha_nacimiento_valida_edad) |>
  dplyr::group_by(CHAVE_FUNCIONAL) |>
  dplyr::summarise(
    fecha_nacimiento_referencia = min(fecha_nacimiento, na.rm = TRUE),
    n_fechas_nacimiento_validas = dplyr::n_distinct(fecha_nacimiento, na.rm = TRUE),
    .groups = "drop"
  )

# Resumir frecuencia, diversidad y trayectoria de uso del servicio por beneficiario.
utilizacion_beneficiario <- base_cid |>
  dplyr::group_by(CHAVE_FUNCIONAL) |>
  dplyr::summarise(
    n_registros_total = dplyr::n(),  # Cuenta todos los registros del beneficiario.
    n_utilizaciones_aprox = dplyr::n_distinct(fecha_utilizacion),  # Aproxima número de utilizaciones por fecha.
    fecha_primera_utilizacion = min(fecha_utilizacion, na.rm = TRUE),
    fecha_ultima_utilizacion = max(fecha_utilizacion, na.rm = TRUE),
    n_meses_con_utilizacion = dplyr::n_distinct(lubridate::floor_date(fecha_utilizacion, unit = "month")),
    n_procedimientos_distintos = dplyr::n_distinct(CD_PROCEDIMENTO, na.rm = TRUE),
    n_descripciones_procedimiento_distintas = dplyr::n_distinct(DESCRICAO_PROCEDIMENTO, na.rm = TRUE),
    n_especialidades_distintas = dplyr::n_distinct(desc_especialidade_norm, na.rm = TRUE),
    n_estados_prestador_distintos = dplyr::n_distinct(uf_cnes_prest_hospitalar_norm, na.rm = TRUE),
    n_tipos_unidad_distintos = dplyr::n_distinct(tipo_unidade_prest_hospitalar_norm, na.rm = TRUE),
    n_tipos_cetipo_distintos = dplyr::n_distinct(cetipo_norm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dias_entre_primera_y_ultima_utilizacion = as.integer(fecha_ultima_utilizacion - fecha_primera_utilizacion),
    promedio_registros_por_utilizacion = round(n_registros_total / pmax(n_utilizaciones_aprox, 1), 4)
  )

# Construir señales de complejidad asistencial por beneficiario.
severidad_beneficiario <- base_cid |>
  dplyr::group_by(CHAVE_FUNCIONAL) |>
  dplyr::summarise(
    tuvo_uti = as.integer(any(UTI_BIN == 1, na.rm = TRUE)),  # Marca si el beneficiario tuvo al menos un registro UTI.
    tuvo_internacion = as.integer(any(INTERNADO_BIN == 1, na.rm = TRUE)),  # Marca si tuvo al menos un registro de internación.
    n_registros_uti = sum(UTI_BIN == 1, na.rm = TRUE),
    n_registros_internacion = sum(INTERNADO_BIN == 1, na.rm = TRUE),
    n_registros_porte_anestesico = sum(!is.na(porte_anestesico_num)),
    max_porte_anestesico = if (all(is.na(porte_anestesico_num))) NA_real_ else max(porte_anestesico_num, na.rm = TRUE),
    promedio_porte_anestesico = if (all(is.na(porte_anestesico_num))) NA_real_ else mean(porte_anestesico_num, na.rm = TRUE),
    .groups = "drop"
  )

# Resumir costos generales por beneficiario sin usar información diagnóstica directa como predictor.
costos_beneficiario <- base_cid |>
  dplyr::group_by(CHAVE_FUNCIONAL) |>
  dplyr::summarise(
    costo_total_general = sum(valor_utilizacion_num, na.rm = TRUE),  # Suma todo el costo observado del beneficiario.
    costo_total_positivo_general = sum(dplyr::if_else(!is.na(valor_utilizacion_num) & valor_utilizacion_num > 0, valor_utilizacion_num, 0), na.rm = TRUE),  # Suma solo costos positivos para evitar reversos o ceros.
    costo_promedio_general = mean(valor_utilizacion_num, na.rm = TRUE),
    costo_mediana_general = median(valor_utilizacion_num, na.rm = TRUE),
    costo_maximo_general = max(valor_utilizacion_num, na.rm = TRUE),
    costo_sd_general = sd_segura(valor_utilizacion_num),
    n_registros_costo_positivo = sum(valor_utilizacion_positivo, na.rm = TRUE),
    n_registros_costo_cero = sum(valor_utilizacion_cero, na.rm = TRUE),
    n_registros_costo_negativo = sum(valor_utilizacion_negativo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    total_registros_costo_control = n_registros_costo_positivo + n_registros_costo_cero + n_registros_costo_negativo,
    pct_registros_costo_positivo = dplyr::if_else(total_registros_costo_control > 0, 100 * n_registros_costo_positivo / total_registros_costo_control, NA_real_),
    tiene_costo_cero = as.integer(n_registros_costo_cero > 0),
    tiene_costo_negativo = as.integer(n_registros_costo_negativo > 0)
  ) |>
  dplyr::select(-total_registros_costo_control)

# Se integran variables demográficas, uso del servicio, severidad y costos por beneficiario.
analitica_beneficiario <- objetivo_beneficiario |>
  dplyr::select(CHAVE_FUNCIONAL, tiene_calculo_renal) |>
  dplyr::left_join(sexo_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(tipo_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(fecha_nacimiento_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(utilizacion_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(severidad_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(costos_beneficiario, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(cetipo_dominante, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(especialidad_dominante, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(estado_prestador_dominante, by = "CHAVE_FUNCIONAL") |>
  dplyr::left_join(tipo_unidad_dominante, by = "CHAVE_FUNCIONAL") |>
  dplyr::mutate(
    edad_primera_utilizacion = as.integer(floor(lubridate::time_length(lubridate::interval(fecha_nacimiento_referencia, fecha_primera_utilizacion), unit = "years"))),
    edad_ultima_utilizacion = as.integer(floor(lubridate::time_length(lubridate::interval(fecha_nacimiento_referencia, fecha_ultima_utilizacion), unit = "years")))
  ) |>
  dplyr::mutate(
    dplyr::across(where(is.character), ~ dplyr::if_else(is.na(.x) | .x == "", "NO_INFORMADO", .x))
  )

guardar_tabla_sqlite(analitica_beneficiario, "analitica_beneficiario", overwrite = TRUE)  # Guarda la tabla base a nivel beneficiario.

# Preparación final de variables para modelamiento.
# Preparar variables finales para modelamiento: edades válidas, costos transformados y categorías agrupadas.
analitica_beneficiario_auditada <- analitica_beneficiario |>
  dplyr::mutate(
    edad_primera_utilizacion_modelo = dplyr::if_else(edad_primera_utilizacion >= 0 & edad_primera_utilizacion <= 110, as.numeric(edad_primera_utilizacion), NA_real_),
    edad_ultima_utilizacion_modelo = dplyr::if_else(edad_ultima_utilizacion >= 0 & edad_ultima_utilizacion <= 110, as.numeric(edad_ultima_utilizacion), NA_real_),
    edad_primera_utilizacion_modelo = dplyr::if_else(is.na(edad_primera_utilizacion_modelo), mediana_segura(edad_primera_utilizacion_modelo, defecto = 0), edad_primera_utilizacion_modelo),
    edad_ultima_utilizacion_modelo = dplyr::if_else(is.na(edad_ultima_utilizacion_modelo), mediana_segura(edad_ultima_utilizacion_modelo, defecto = 0), edad_ultima_utilizacion_modelo),
    max_porte_anestesico_modelo = dplyr::if_else(is.na(max_porte_anestesico), 0, as.numeric(max_porte_anestesico)),
    promedio_porte_anestesico_modelo = dplyr::if_else(is.na(promedio_porte_anestesico), 0, as.numeric(promedio_porte_anestesico)),
    costo_total_general_winsor_p99 = winsorizar_p99(costo_total_general),
    costo_total_positivo_general_winsor_p99 = winsorizar_p99(costo_total_positivo_general),
    costo_promedio_general_winsor_p99 = winsorizar_p99(costo_promedio_general),
    costo_mediana_general_winsor_p99 = winsorizar_p99(costo_mediana_general),
    costo_maximo_general_winsor_p99 = winsorizar_p99(costo_maximo_general),
    costo_sd_general_winsor_p99 = winsorizar_p99(costo_sd_general),
    log1p_costo_total_general_winsor_p99 = log1p(pmax(costo_total_general_winsor_p99, 0)),
    log1p_costo_total_positivo_general = log1p(pmax(costo_total_positivo_general, 0)),
    log1p_costo_promedio_general = log1p(pmax(costo_promedio_general, 0)),
    log1p_costo_mediana_general = log1p(pmax(costo_mediana_general, 0)),
    log1p_costo_maximo_general_winsor_p99 = log1p(pmax(costo_maximo_general_winsor_p99, 0)),
    log1p_costo_sd_general = log1p(pmax(costo_sd_general, 0)),
    tiene_calculo_renal_factor = factor(dplyr::if_else(tiene_calculo_renal == 1, "Con N20", "Sin N20"), levels = c("Sin N20", "Con N20")),  # Convierte la etiqueta a factor para yardstick y modelos.
    sexo_dominante = agrupar_top(sexo_dominante, n_top = 4),
    tipo_beneficiario_dominante = agrupar_top(tipo_beneficiario_dominante, n_top = 6),
    cetipo_dominante = agrupar_top(cetipo_dominante, n_top = 8),
    especialidad_dominante = agrupar_top(especialidad_dominante, n_top = 10),
    estado_prestador_dominante = agrupar_top(estado_prestador_dominante, n_top = 10),
    tipo_unidad_dominante = agrupar_top(tipo_unidad_dominante, n_top = 10)
  )

guardar_tabla_sqlite(analitica_beneficiario_auditada, "analitica_beneficiario_auditada", overwrite = TRUE)  # Guarda la tabla final de modelamiento ya auditada.

# Resumir tamaño, duplicados y presencia de variables prohibidas en la tabla analítica.
control_analitica <- tibble::tibble(
  elemento = c(
    "filas_analitica", "beneficiarios_distintos", "positivos_n20", "negativos_n20",
    "columnas_analitica", "duplicados_chave", "variables_prohibidas_directas_presentes"
  ),
  valor = c(
    as.character(nrow(analitica_beneficiario_auditada)),
    as.character(dplyr::n_distinct(analitica_beneficiario_auditada$CHAVE_FUNCIONAL)),
    as.character(sum(analitica_beneficiario_auditada$tiene_calculo_renal == 1, na.rm = TRUE)),
    as.character(sum(analitica_beneficiario_auditada$tiene_calculo_renal == 0, na.rm = TRUE)),
    as.character(ncol(analitica_beneficiario_auditada)),
    as.character(nrow(analitica_beneficiario_auditada) - dplyr::n_distinct(analitica_beneficiario_auditada$CHAVE_FUNCIONAL)),
    as.character(length(intersect(c("CID", "CID_NORM", "CID_VALIDO", "flag_n20", "es_cid_n20", "n_registros_n20"), names(analitica_beneficiario_auditada))))
  )
)
guardar_csv(control_analitica, "compacto_02_control_analitica_auditada.csv")  # Exporta indicadores básicos de la tabla analítica.

# Resumen de calidad de datos.
# Resume faltantes, fechas inválidas, costos atípicos y posibles fuentes de fuga de información.
# Definir variables que no pueden entrar al modelo porque contienen diagnóstico o derivados directos.
variables_prohibidas_directas_compacto <- c(
  "CID", "CID_NORM", "CID_VALIDO", "CID_N20", "es_cid_n20", "flag_n20",
  "n_registros_n20", "conteo_n20", "tiene_n20_utilizacion", "codigos_cid_n20",
  "n_registros_n20_consulta"
)
variables_prohibidas_presentes_compacto <- intersect(variables_prohibidas_directas_compacto, names(analitica_beneficiario_auditada))
variables_sospechosas_nombre_compacto <- setdiff(
  names(analitica_beneficiario_auditada)[stringr::str_detect(names(analitica_beneficiario_auditada), stringr::regex("CID|N20|FLAG", ignore_case = TRUE))],
  c("tiene_calculo_renal", "tiene_calculo_renal_factor")
)

# Reunir indicadores de calidad de datos para la presentación de resultados.
control_calidad_compacto <- tibble::tibble(
  dimension = c(
    "Identificador",
    "CID",
    "CID",
    "Fecha utilización",
    "Fecha nacimiento",
    "Fecha nacimiento",
    "Costo",
    "Costo",
    "Costo",
    "Binaria UTI",
    "Binaria internacion",
    "Analítica",
    "Analítica",
    "Fuga de información",
    "Fuga de información"
  ),
  control = c(
    "CHAVE_FUNCIONAL faltante en base transaccional",
    "CID_NORM faltante o no informado",
    "Registros N20 detectados",
    "fecha_utilizacion faltante",
    "fecha_nacimiento posterior a utilización",
    "fecha_nacimiento 1900 o anterior",
    "valor_utilizacion_num faltante",
    "valor_utilizacion_num igual a cero",
    "valor_utilizacion_num negativo",
    "UTI_BIN faltante después de normalizar",
    "INTERNADO_BIN faltante después de normalizar",
    "Duplicados de CHAVE_FUNCIONAL",
    "Faltantes totales en tabla analítica auditada",
    "Variables prohibidas directas presentes",
    "Variables sospechosas por nombre presentes"
  ),
  valor = c(
    sum(is.na(base_cid$CHAVE_FUNCIONAL)),
    sum(is.na(base_cid$CID_NORM)),
    sum(base_cid$flag_n20 == 1, na.rm = TRUE),
    sum(is.na(base_cid$fecha_utilizacion)),
    sum(base_cid$fecha_nacimiento_posterior_uso, na.rm = TRUE),
    sum(base_cid$fecha_nacimiento_1900, na.rm = TRUE),
    sum(is.na(base_cid$valor_utilizacion_num)),
    sum(base_cid$valor_utilizacion_cero, na.rm = TRUE),
    sum(base_cid$valor_utilizacion_negativo, na.rm = TRUE),
    sum(is.na(base_cid$UTI_BIN)),
    sum(is.na(base_cid$INTERNADO_BIN)),
    nrow(analitica_beneficiario_auditada) - dplyr::n_distinct(analitica_beneficiario_auditada$CHAVE_FUNCIONAL),
    sum(is.na(analitica_beneficiario_auditada)),
    length(variables_prohibidas_presentes_compacto),
    length(variables_sospechosas_nombre_compacto)
  ),
  decision = c(
    "Debe ser cero para modelar por beneficiario.",
    "Se trata como no informado; no define N20.",
    "Debe ser mayor que cero para construir el objetivo.",
    "Idealmente cero para granularidad por utilización.",
    "Se marca como inválida para edad.",
    "Se marca como inválida para edad.",
    "Se conserva como control de calidad de costo.",
    "Se conserva; puede reflejar registros sin costo positivo.",
    "Se conserva como alerta de calidad o reverso contable.",
    "Se imputa/trata posteriormente si entra al modelo.",
    "Se imputa/trata posteriormente si entra al modelo.",
    "Debe ser cero en la tabla analítica.",
    "Se resuelve en receta/preprocesamiento antes del modelo.",
    "Debe ser cero; evita fuga diagnóstica directa.",
    "Debe ser cero salvo objetivo/factor permitido."
  )
)

guardar_csv(control_calidad_compacto, "compacto_02b_control_calidad.csv")  # Exporta indicadores de calidad y fuga de información.
guardar_tabla_sqlite(control_calidad_compacto, "compacto_control_calidad", overwrite = TRUE)


#------------------------------------------------------------
# 3. Análisis descriptivo
#------------------------------------------------------------

# Crear el resumen general de beneficiarios, registros, utilización y costo.
resumen_descriptivo_general <- tibble::tibble(
  indicador = c(
    "beneficiarios", "positivos_n20", "prevalencia_pct", "registros_base", "utilizaciones_aprox",
    "procedimientos_distintos", "costo_total_general", "costo_mediano_general"
  ),
  valor = c(
    nrow(analitica_beneficiario_auditada),
    sum(analitica_beneficiario_auditada$tiene_calculo_renal == 1, na.rm = TRUE),
    100 * mean(analitica_beneficiario_auditada$tiene_calculo_renal == 1, na.rm = TRUE),
    nrow(base_cid),
    dplyr::n_distinct(paste(base_cid$CHAVE_FUNCIONAL, base_cid$fecha_utilizacion)),
    dplyr::n_distinct(base_cid$CD_PROCEDIMENTO, na.rm = TRUE),
    sum(base_cid$valor_utilizacion_num, na.rm = TRUE),
    median(analitica_beneficiario_auditada$costo_mediana_general, na.rm = TRUE)
  )
)

# Crear grupos de edad para las tablas descriptivas.
analitica_descriptiva <- analitica_beneficiario_auditada |>
  dplyr::mutate(
    grupo_edad = cut(
      edad_ultima_utilizacion_modelo,
      breaks = c(-Inf, 17, 29, 39, 49, 59, 69, Inf),
      labels = c("00-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      right = TRUE
    ),
    grupo_edad = dplyr::if_else(is.na(as.character(grupo_edad)), "NO_INFORMADO", as.character(grupo_edad))
  )

# Construir tablas descriptivas por etiqueta N20 para variables principales.
tablas_descriptivas <- list(
  sexo = analitica_descriptiva |> dplyr::count(sexo_dominante, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(sexo_dominante) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  edad = analitica_descriptiva |> dplyr::count(grupo_edad, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(grupo_edad) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  tipo_beneficiario = analitica_descriptiva |> dplyr::count(tipo_beneficiario_dominante, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(tipo_beneficiario_dominante) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  estado_prestador = analitica_descriptiva |> dplyr::count(estado_prestador_dominante, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(estado_prestador_dominante) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  internacion = analitica_descriptiva |> dplyr::count(tuvo_internacion, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(tuvo_internacion) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  uti = analitica_descriptiva |> dplyr::count(tuvo_uti, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(tuvo_uti) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup(),
  especialidad = analitica_descriptiva |> dplyr::count(especialidad_dominante, tiene_calculo_renal_factor, name = "n") |> dplyr::group_by(especialidad_dominante) |> dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup()
)

purrr::iwalk(tablas_descriptivas, ~ guardar_csv(.x, paste0("compacto_03_tabla_descriptiva_", .y, ".csv")))
guardar_csv(resumen_descriptivo_general, "compacto_03_resumen_descriptivo_general.csv")  # Exporta resumen descriptivo global.

#------------------------------------------------------------
# 4. Entrenamiento de modelos
#------------------------------------------------------------

# Variables que se retiran antes del modelamiento por ser fechas crudas, costos no transformados o valores auxiliares.
variables_excluir_modelo <- c(
  "tiene_calculo_renal", "fecha_nacimiento_referencia", "fecha_primera_utilizacion", "fecha_ultima_utilizacion",
  "costo_total_general", "costo_promedio_general", "costo_mediana_general", "costo_maximo_general", "costo_sd_general",
  "max_porte_anestesico", "promedio_porte_anestesico"
)

# Armar la base final usada para la partición y el entrenamiento.
datos_modelo <- analitica_beneficiario_auditada |>
  dplyr::select(-dplyr::any_of(variables_excluir_modelo)) |>
  dplyr::mutate(tiene_calculo_renal_factor = factor(tiene_calculo_renal_factor, levels = c("Sin N20", "Con N20")))

split_obj <- rsample::initial_split(datos_modelo, prop = prop_train, strata = tiene_calculo_renal_factor)  # Realiza partición train/test estratificada por la clase N20.
train_data <- rsample::training(split_obj)  # Extrae la muestra de entrenamiento.
test_data <- rsample::testing(split_obj)  # Extrae la muestra de prueba.

# La receta aprende el preprocesamiento en train y luego lo replica sobre test.
# Definir el preprocesamiento que se aprende solo en entrenamiento y se replica en prueba.
receta_modelo <- recipes::recipe(tiene_calculo_renal_factor ~ ., data = train_data) |>
  recipes::update_role(CHAVE_FUNCIONAL, new_role = "id") |>
  recipes::step_novel(
    recipes::all_nominal_predictors(),
    new_level = "NUEVA_CATEGORIA"
  ) |>
  recipes::step_unknown(
    recipes::all_nominal_predictors(),
    new_level = "SIN_INFORMACION"
  ) |>
  recipes::step_other(
    recipes::all_nominal_predictors(),
    threshold = 0.005,
    other = "OTRA_CATEGORIA"
  ) |>
  recipes::step_impute_median(recipes::all_numeric_predictors()) |>
  recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |>
  recipes::step_zv(recipes::all_predictors())

receta_preparada <- recipes::prep(receta_modelo, training = train_data, retain = TRUE)  # Aprende imputaciones, agrupaciones y dummies usando solo train.
train_proc <- recipes::bake(receta_preparada, new_data = train_data)  # Aplica la receta al entrenamiento.
test_proc <- recipes::bake(receta_preparada, new_data = test_data)  # Aplica la misma receta al test.

# Limpiar cualquier NA numérico residual después de aplicar la receta.
train_proc <- train_proc |> dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(is.na(.x), 0, .x)))
test_proc <- test_proc |> dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(is.na(.x), 0, .x)))

guardar_rds(receta_preparada, "compacto_receta_preparada.rds")
guardar_rds(train_proc, "compacto_train_procesado.rds")
guardar_rds(test_proc, "compacto_test_procesado.rds")

# Guardar tamaño, prevalencia y columnas después del preprocesamiento.
control_particion_receta <- tibble::tibble(
  elemento = c("filas_train", "filas_test", "positivos_train", "positivos_test", "prevalencia_train_pct", "prevalencia_test_pct", "columnas_train_proc", "columnas_test_proc", "na_train_proc", "na_test_proc"),
  valor = c(
    nrow(train_data), nrow(test_data),
    sum(train_data$tiene_calculo_renal_factor == "Con N20"), sum(test_data$tiene_calculo_renal_factor == "Con N20"),
    100 * mean(train_data$tiene_calculo_renal_factor == "Con N20"), 100 * mean(test_data$tiene_calculo_renal_factor == "Con N20"),
    ncol(train_proc), ncol(test_proc),
    sum(is.na(train_proc)), sum(is.na(test_proc))
  ) |> as.character()
)
guardar_csv(control_particion_receta, "compacto_04_control_particion_receta.csv")

#------------------------------------------------------------
# 4.1 Entrenamiento de modelos de clasificación
#------------------------------------------------------------

# CHAVE_FUNCIONAL es identificador del beneficiario, no predictor.
# Por eso se retira de las bases usadas por regresión logística y árbol.
# Así el ajuste se concentra en variables agregadas del beneficiario y no en su código individual.

# Bases procesadas para modelos que no deben recibir el identificador.
train_proc_modelos_base <- train_proc |>
  dplyr::select(-dplyr::any_of("CHAVE_FUNCIONAL"))

test_proc_modelos_base <- test_proc |>
  dplyr::select(-dplyr::any_of("CHAVE_FUNCIONAL"))

train_pred_modelos_base <- train_proc_modelos_base |>
  dplyr::select(-dplyr::any_of("tiene_calculo_renal_factor"))

test_pred_modelos_base <- test_proc_modelos_base |>
  dplyr::select(-dplyr::any_of("tiene_calculo_renal_factor"))

# Se garantiza que test tenga exactamente las mismas columnas predictoras de train.
test_pred_modelos_base <- test_pred_modelos_base |>
  dplyr::select(dplyr::all_of(names(train_pred_modelos_base)))

# Ajustar la regresión logística usando una respuesta binaria numérica.
ajustar_logistica <- function(train_proc_modelos_base) {
  datos_glm <- train_proc_modelos_base |>
    dplyr::mutate(y_n20 = as.integer(tiene_calculo_renal_factor == "Con N20")) |>
    dplyr::select(-tiene_calculo_renal_factor)
  
  stats::glm(
    y_n20 ~ .,
    data = datos_glm,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100)
  )
}

# Entrenar modelos candidatos y continuar la comparación si algún ajuste falla.
modelo_logistico <- tryCatch(
  suppressWarnings(ajustar_logistica(train_proc_modelos_base)),
  error = function(e) e
)

modelo_arbol <- tryCatch(
  rpart::rpart(
    tiene_calculo_renal_factor ~ .,
    data = train_proc_modelos_base,
    method = "class",
    parms = list(split = "gini"),
    control = rpart::rpart.control(cp = 0.001, maxdepth = 8, minsplit = 50)
  ),
  error = function(e) e
)

# Random forest se ajusta sin usar CHAVE_FUNCIONAL y con pesos para atender el desbalance.
n_pred_rf <- ncol(train_proc) - 2  # Excluye objetivo e identificador.
pesos_clase <- table(train_proc$tiene_calculo_renal_factor)
pesos_clase <- sum(pesos_clase) / (length(pesos_clase) * pesos_clase)

modelo_rf <- ranger::ranger(
  tiene_calculo_renal_factor ~ . - CHAVE_FUNCIONAL,
  data = train_proc,
  probability = TRUE,
  num.trees = n_arboles_rf,
  mtry = max(1, floor(sqrt(n_pred_rf))),
  min.node.size = 20,
  importance = "impurity",
  class.weights = pesos_clase,
  seed = semilla,
  num.threads = max(1, parallel::detectCores() - 1)
)

guardar_rds(modelo_logistico, "compacto_modelo_logistico.rds")
guardar_rds(modelo_arbol, "compacto_modelo_arbol.rds")
guardar_rds(modelo_rf, "compacto_modelo_random_forest.rds")

#------------------------------------------------------------
# 5. Evaluación y comparación de modelos
#------------------------------------------------------------

# Función única para obtener probabilidades comparables desde cada modelo.
predecir_prob <- function(modelo, datos, tipo) {
  if (inherits(modelo, "error")) return(rep(NA_real_, nrow(datos)))
  
  if (tipo == "glm") {
    prob <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = datos, type = "response")),
      error = function(e) rep(NA_real_, nrow(datos))
    )
    return(pmax(pmin(prob, 1), 0))
  }
  
  if (tipo == "rpart") {
    prob <- tryCatch(
      extraer_prob_evento(stats::predict(modelo, newdata = datos, type = "prob"), evento = "Con N20"),
      error = function(e) rep(NA_real_, nrow(datos))
    )
    return(pmax(pmin(prob, 1), 0))
  }
  
  if (tipo == "ranger") {
    prob <- tryCatch(
      extraer_prob_evento(stats::predict(modelo, data = datos)$predictions, evento = "Con N20"),
      error = function(e) rep(NA_real_, nrow(datos))
    )
    return(pmax(pmin(prob, 1), 0))
  }
  
  rep(NA_real_, nrow(datos))
}

# Se generan probabilidades en entrenamiento para estimar umbrales operativos.
# Para logística y árbol se usan bases sin CHAVE_FUNCIONAL, porque ese campo es identificador.
pred_train <- train_proc |>
  dplyr::select(CHAVE_FUNCIONAL, tiene_calculo_renal_factor) |>
  dplyr::mutate(
    prob_logistica = predecir_prob(modelo_logistico, train_pred_modelos_base, "glm"),
    prob_arbol = predecir_prob(modelo_arbol, train_pred_modelos_base, "rpart"),
    prob_rf = predecir_prob(modelo_rf, train_proc, "ranger")
  )

# Se generan probabilidades en test para evaluar desempeño fuera de muestra.
# El identificador se conserva para reconocer registros, pero no entra como predictor.
pred_test <- test_proc |>
  dplyr::select(CHAVE_FUNCIONAL, tiene_calculo_renal_factor) |>
  dplyr::mutate(
    prob_logistica = predecir_prob(modelo_logistico, test_pred_modelos_base, "glm"),
    prob_arbol = predecir_prob(modelo_arbol, test_pred_modelos_base, "rpart"),
    prob_rf = predecir_prob(modelo_rf, test_proc, "ranger")
  )

# Predicciones completas para comparar los modelos.
guardar_csv(pred_train, "compacto_05_predicciones_train_modelos.csv")
guardar_csv(pred_test, "compacto_05_predicciones_test_modelos.csv")

prevalencia_train <- mean(train_proc$tiene_calculo_renal_factor == "Con N20")  # Calcula prevalencia de N20 en entrenamiento para definir umbral operativo.
calcular_umbral_operativo <- function(probabilidades, prevalencia) {  # Define umbral por prevalencia usando solo probabilidades finitas de entrenamiento.
  probabilidades <- suppressWarnings(as.numeric(probabilidades))
  probabilidades <- probabilidades[is.finite(probabilidades)]
  if (length(probabilidades) == 0 || !is.finite(prevalencia) || prevalencia <= 0 || prevalencia >= 1) return(0.5)
  umbral <- as.numeric(stats::quantile(probabilidades, probs = 1 - prevalencia, na.rm = TRUE, names = FALSE))
  if (!is.finite(umbral)) 0.5 else umbral
}

# Calcular un umbral operativo por modelo usando la prevalencia de entrenamiento.
umbrales_modelos <- tibble::tibble(
  modelo = c("Regresión logística base", "Árbol de decisión", "Random forest ranger"),
  prob_col = c("prob_logistica", "prob_arbol", "prob_rf")
) |>
  dplyr::mutate(
    umbral_operativo = purrr::map_dbl(prob_col, ~ calcular_umbral_operativo(pred_train[[.x]], prevalencia_train))
  )

# Contar probabilidades válidas producidas por cada modelo en entrenamiento y prueba.
diagnostico_modelos <- tibble::tibble(
  modelo = c("Regresión logística base", "Árbol de decisión", "Random forest ranger"),
  objeto_ajustado = c(!inherits(modelo_logistico, "error"), !inherits(modelo_arbol, "error"), inherits(modelo_rf, "ranger")),
  probabilidades_train_finitas = c(sum(is.finite(pred_train$prob_logistica)), sum(is.finite(pred_train$prob_arbol)), sum(is.finite(pred_train$prob_rf))),
  probabilidades_test_finitas = c(sum(is.finite(pred_test$prob_logistica)), sum(is.finite(pred_test$prob_arbol)), sum(is.finite(pred_test$prob_rf))),
  nota = c(
    "Base lineal para respuesta binaria; puede presentar separación por evento raro.",
    "Modelo no lineal interpretable con criterio Gini.",
    "Ensamble tipo Random Forest con pesos de clase."
  )
)
guardar_csv(diagnostico_modelos, "compacto_05_diagnostico_modelos.csv")

# Evaluar todos los modelos en el conjunto de prueba con sus respectivos umbrales.
metricas_modelos <- purrr::pmap_dfr(
  list(umbrales_modelos$modelo, umbrales_modelos$prob_col, umbrales_modelos$umbral_operativo),
  ~ metricas_clasificacion(pred_test, prob_col = ..2, umbral = ..3, modelo = ..1)
) |>
  dplyr::arrange(dplyr::desc(pr_auc), dplyr::desc(f_meas), dplyr::desc(sens))

modelos_con_metricas_finitas <- sum(is.finite(metricas_modelos$pr_auc) | is.finite(metricas_modelos$f_meas))
if (modelos_con_metricas_finitas < 2) {
  warning(
    "Menos de dos modelos tienen métricas finitas. El código entrenó más de un clasificador, " ,
    "pero conviene revisar diagnostico_modelos y las predicciones antes de interpretar la comparación."
  )
}

# Seleccionar el modelo candidato y recuperar su columna de probabilidad y umbral.
modelo_candidato <- metricas_modelos |> dplyr::slice_head(n = 1)  # Selecciona el mejor modelo según el ordenamiento de métricas.
modelo_candidato_nombre <- modelo_candidato$modelo[1]
prob_candidato_col <- umbrales_modelos$prob_col[match(modelo_candidato_nombre, umbrales_modelos$modelo)]
umbral_candidato <- umbrales_modelos$umbral_operativo[match(modelo_candidato_nombre, umbrales_modelos$modelo)]

metricas_candidato_final <- metricas_clasificacion(pred_test, prob_candidato_col, umbral_candidato, modelo_candidato_nombre)  # Evalúa el modelo candidato en test.
matriz_candidato <- matriz_confusion_compacta(pred_test, prob_candidato_col, umbral_candidato)  # Construye matriz de confusión final.
# Resumir tipos de acierto y error del modelo candidato.
resumen_errores <- matriz_candidato |>
  dplyr::group_by(tipo_resultado) |>
  dplyr::summarise(beneficiarios = sum(n), .groups = "drop") |>
  dplyr::mutate(pct_test = 100 * beneficiarios / sum(beneficiarios))

# Métricas por umbral del candidato.
# Este bloque resume el cambio entre sensibilidad, precisión y volumen de alertas.
umbrales_evaluacion_candidato <- tibble::tibble(
  tipo_umbral = c("0.01", "0.02", "0.05", "0.10", "0.20", "operativo_train_prevalencia", "0.30", "0.50"),
  umbral = c(0.01, 0.02, 0.05, 0.10, 0.20, umbral_candidato, 0.30, 0.50)
) |>
  dplyr::mutate(umbral = as.numeric(umbral)) |>
  dplyr::distinct(tipo_umbral, umbral, .keep_all = TRUE)

# Evaluar cómo cambian las métricas del candidato bajo diferentes umbrales.
metricas_por_umbral_candidato <- purrr::map2_dfr(
  umbrales_evaluacion_candidato$tipo_umbral,
  umbrales_evaluacion_candidato$umbral,
  function(tipo_umbral, umbral_eval) {
    metricas_clasificacion(pred_test, prob_candidato_col, umbral_eval, modelo_candidato_nombre) |>
      dplyr::mutate(tipo_umbral = tipo_umbral, .before = modelo)
  }
)

# Contar VP, FN, FP, VN y alertas para cada umbral evaluado.
conteos_por_umbral_candidato <- purrr::map2_dfr(
  umbrales_evaluacion_candidato$tipo_umbral,
  umbrales_evaluacion_candidato$umbral,
  function(tipo_umbral, umbral_eval) {
    matriz_confusion_compacta(pred_test, prob_candidato_col, umbral_eval) |>
      dplyr::summarise(
        tipo_umbral = tipo_umbral,
        umbral = umbral_eval,
        verdaderos_positivos = sum(n[tipo_resultado == "Verdadero positivo"], na.rm = TRUE),
        falsos_negativos = sum(n[tipo_resultado == "Falso negativo"], na.rm = TRUE),
        falsos_positivos = sum(n[tipo_resultado == "Falso positivo"], na.rm = TRUE),
        verdaderos_negativos = sum(n[tipo_resultado == "Verdadero negativo"], na.rm = TRUE),
        alertas_positivas = verdaderos_positivos + falsos_positivos,
        .groups = "drop"
      )
  }
)

metricas_por_umbral_candidato <- metricas_por_umbral_candidato |>
  dplyr::left_join(conteos_por_umbral_candidato, by = c("tipo_umbral", "umbral")) |>
  dplyr::arrange(umbral)

# Construir la tabla final de predicciones del candidato en prueba.
pred_test_final <- pred_test |>
  dplyr::mutate(
    prob_candidato = .data[[prob_candidato_col]],  # Probabilidad final producida por el modelo candidato.
    pred_candidato = factor(dplyr::if_else(prob_candidato >= umbral_candidato, "Con N20", "Sin N20"), levels = c("Sin N20", "Con N20")),
    tipo_resultado = dplyr::case_when(
      tiene_calculo_renal_factor == "Con N20" & pred_candidato == "Con N20" ~ "Verdadero positivo",
      tiene_calculo_renal_factor == "Con N20" & pred_candidato == "Sin N20" ~ "Falso negativo",
      tiene_calculo_renal_factor == "Sin N20" & pred_candidato == "Con N20" ~ "Falso positivo",
      TRUE ~ "Verdadero negativo"
    )
  )

guardar_csv(metricas_modelos, "compacto_05_metricas_modelos.csv")  # Exporta comparación de modelos exigida por el taller.
guardar_csv(umbrales_modelos, "compacto_05_umbrales_modelos.csv")
guardar_csv(metricas_por_umbral_candidato, "compacto_05_metricas_por_umbral_candidato.csv")  # Exporta sensibilidad, precisión y F1 bajo varios umbrales.
guardar_csv(metricas_candidato_final, "compacto_05_metricas_candidato_final.csv")
guardar_csv(matriz_candidato, "compacto_05_matriz_confusion_candidato.csv")
guardar_csv(resumen_errores, "compacto_05_resumen_errores_candidato.csv")
guardar_csv(pred_test_final, "compacto_05_predicciones_test_candidato.csv")  # Exporta predicciones finales del conjunto de prueba.

guardar_tabla_sqlite(metricas_modelos, "compacto_metricas_modelos", overwrite = TRUE)
guardar_tabla_sqlite(metricas_candidato_final, "compacto_metricas_candidato_final", overwrite = TRUE)
guardar_tabla_sqlite(matriz_candidato, "compacto_matriz_confusion_candidato", overwrite = TRUE)
guardar_tabla_sqlite(metricas_por_umbral_candidato, "compacto_metricas_por_umbral_candidato", overwrite = TRUE)

#------------------------------------------------------------
# 6. Interpretación de variables importantes
#------------------------------------------------------------

# Extraer y clasificar la importancia de variables del Random Forest.
importancia_rf <- tibble::tibble(  # Extrae importancia de variables del random forest.
  variable = names(modelo_rf$variable.importance),
  importancia = as.numeric(modelo_rf$variable.importance)
) |>
  dplyr::arrange(dplyr::desc(importancia)) |>
  dplyr::mutate(
    importancia_relativa = importancia / max(importancia, na.rm = TRUE),
    grupo_interpretativo = dplyr::case_when(
      stringr::str_detect(variable, regex("costo|log1p", ignore_case = TRUE)) ~ "Costo / intensidad económica",
      stringr::str_detect(variable, regex("porte|uti|internacion", ignore_case = TRUE)) ~ "Severidad / complejidad asistencial",
      stringr::str_detect(variable, regex("registro|utilización|procedimiento|especialidad|unidad|cetipo", ignore_case = TRUE)) ~ "Uso del servicio",
      stringr::str_detect(variable, regex("edad|sexo|beneficiario", ignore_case = TRUE)) ~ "Demografía",
      stringr::str_detect(variable, regex("estado", ignore_case = TRUE)) ~ "Región / prestador",
      TRUE ~ "Otro"
    ),
    lectura_predictiva = "Variable asociada al ordenamiento de riesgo del modelo; no debe leerse como causalidad clínica."
  )

importancia_top <- importancia_rf |> dplyr::slice_head(n = 30)  # Conserva las 30 variables más importantes para el informe.

# Extraer coeficientes logísticos como referencia interpretativa del modelo lineal.
coeficientes_logisticos <- if (!inherits(modelo_logistico, "error")) {
  broom::tidy(modelo_logistico) |>
    dplyr::arrange(dplyr::desc(abs(estimate))) |>
    dplyr::mutate(advertencia = "Coeficiente logístico del modelo base; interpretar con cautela por evento raro y posible separación.")
} else {
  tibble::tibble(term = character(), estimate = numeric(), std.error = numeric(), statistic = numeric(), p.value = numeric(), advertencia = character())
}

# Unir predicciones con subgrupos para calcular sensibilidad por perfiles.
pred_test_con_subgrupos <- pred_test_final |>
  dplyr::left_join(
    test_data |>
      dplyr::select(CHAVE_FUNCIONAL, sexo_dominante, tipo_beneficiario_dominante, edad_ultima_utilizacion_modelo, estado_prestador_dominante, especialidad_dominante, tuvo_internacion, tuvo_uti),
    by = "CHAVE_FUNCIONAL"
  ) |>
  dplyr::mutate(grupo_edad = cut(edad_ultima_utilizacion_modelo, breaks = c(-Inf, 17, 29, 39, 49, 59, 69, Inf), labels = c("00-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")))

# Calcular sensibilidad y alertas por sexo, edad, estado, especialidad y complejidad.
sensibilidad_subgrupos <- pred_test_con_subgrupos |>
  dplyr::select(tiene_calculo_renal_factor, pred_candidato, sexo_dominante, tipo_beneficiario_dominante, grupo_edad, estado_prestador_dominante, especialidad_dominante, tuvo_internacion, tuvo_uti) |>
  dplyr::mutate(
    dplyr::across(
      -c(tiene_calculo_renal_factor, pred_candidato),
      ~ dplyr::if_else(is.na(as.character(.x)) | as.character(.x) == "", "NO_INFORMADO", as.character(.x))
    )
  ) |>
  tidyr::pivot_longer(cols = -c(tiene_calculo_renal_factor, pred_candidato), names_to = "variable_subgrupo", values_to = "subgrupo") |>
  dplyr::filter(!is.na(subgrupo)) |>
  dplyr::group_by(variable_subgrupo, subgrupo) |>
  dplyr::summarise(
    n = dplyr::n(),
    positivos_reales = sum(tiene_calculo_renal_factor == "Con N20"),
    positivos_predichos = sum(pred_candidato == "Con N20"),
    verdaderos_positivos = sum(tiene_calculo_renal_factor == "Con N20" & pred_candidato == "Con N20"),
    sensibilidad = dplyr::if_else(positivos_reales > 0, verdaderos_positivos / positivos_reales, NA_real_),
    .groups = "drop"
  ) |>
  dplyr::arrange(variable_subgrupo, dplyr::desc(n))

# Registrar limitaciones interpretativas que acompañan la lectura del modelo.
discusion_sesgos_limitaciones <- tibble::tribble(
  ~tema, ~discusion, ~impacto_en_informe,
  "Importancia no causal", "La importancia de variables del random forest refleja utilidad predictiva, no causalidad clínica.", "Alto",
  "Uso del servicio", "Variables de registros, procedimientos, especialidades o costos pueden representar intensidad de uso y no riesgo biológico previo.", "Alto",
  "Temporalidad", "Algunas señales pueden ocurrir durante o después de la atención asociada al diagnóstico.", "Alto",
  "Desbalance", "La exactitud (accuracy) es alta por la rareza del evento; PR-AUC, sensibilidad, precisión y F1 son más informativas.", "Alto",
  "Subgrupos", "Diferencias por sexo, edad, región o prestador pueden reflejar disponibilidad de atención o calidad de registro.", "Medio",
  "Falsos negativos", "El error operativo más delicado es no detectar un beneficiario que sí tuvo N20.", "Alto"
)

guardar_csv(importancia_rf, "compacto_06_importancia_variables_rf.csv")  # Exporta importancia completa de variables.
guardar_csv(importancia_top, "compacto_06_importancia_variables_top.csv")
guardar_csv(coeficientes_logisticos, "compacto_06_coeficientes_logisticos.csv")
guardar_csv(sensibilidad_subgrupos, "compacto_06_sensibilidad_subgrupos.csv")
guardar_csv(discusion_sesgos_limitaciones, "compacto_06_discusion_sesgos_limitaciones.csv")

guardar_tabla_sqlite(importancia_top, "compacto_importancia_variables_top", overwrite = TRUE)
guardar_tabla_sqlite(discusion_sesgos_limitaciones, "compacto_discusion_sesgos_limitaciones", overwrite = TRUE)

#------------------------------------------------------------
# 7. Estimación del costo esperado
#------------------------------------------------------------

# Filtrar registros N20 y preparar variables básicas para el análisis de costos.
base_n20 <- base_cid |>
  dplyr::filter(flag_n20 == 1) |>
  dplyr::mutate(
    CHAVE_FUNCIONAL = as.character(CHAVE_FUNCIONAL),
    valor_utilizacion_positivo_n20 = pmax(valor_utilizacion_num, 0, na.rm = FALSE),
    edad_registro = as.numeric(floor(as.numeric(fecha_utilizacion - fecha_nacimiento) / 365.25)),
    edad_registro = dplyr::if_else(edad_registro >= 0 & edad_registro <= 110, edad_registro, NA_real_)
  )

# Se cambia de unidad: de beneficiario a utilización N20 aproximada para analizar costos.
utilizaciones_N20_costos <- base_n20 |>
  dplyr::mutate(id_utilizacion_n20 = paste(CHAVE_FUNCIONAL, as.character(fecha_utilizacion), sep = "__")) |>
  dplyr::group_by(CHAVE_FUNCIONAL, fecha_utilizacion, id_utilizacion_n20) |>
  dplyr::summarise(
    n_registros_n20 = dplyr::n(),
    n_cid_n20_distintos = dplyr::n_distinct(CID_NORM),
    cid_n20_principal = moda_segura(CID_NORM),
    n_procedimientos_distintos = dplyr::n_distinct(CD_PROCEDIMENTO),
    procedimiento_codigo_principal = moda_segura(CD_PROCEDIMENTO),
    procedimiento_descripcion_principal = moda_segura(descricao_procedimento_norm),
    costo_total_observado_utilizacion = sum(valor_utilizacion_num, na.rm = TRUE),  # Suma costo observado por utilización N20 aproximada.
    costo_total_positivo_utilizacion = sum(valor_utilizacion_positivo_n20, na.rm = TRUE),  # Suma solo costo positivo por utilización N20.
    sexo = moda_segura(sexo_norm),
    tipo_beneficiario = moda_segura(tipo_beneficiario_norm),
    edad_utilizacion = suppressWarnings(median(edad_registro, na.rm = TRUE)),
    uti = dplyr::if_else(max(UTI_BIN, na.rm = TRUE) == 1L, "Con UTI", "Sin UTI"),
    internacion = dplyr::if_else(max(INTERNADO_BIN, na.rm = TRUE) == 1L, "Internado", "Ambulatorio"),
    especialidad = moda_segura(desc_especialidade_norm),
    unidad_prestador = moda_segura(tipo_unidade_prest_hospitalar_norm),
    estado_prestador = moda_segura(uf_cnes_prest_hospitalar_norm),
    porte_anestesico_max = suppressWarnings(max(porte_anestesico_num, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    edad_utilizacion = dplyr::if_else(is.infinite(edad_utilizacion) | is.nan(edad_utilizacion), NA_real_, as.numeric(edad_utilizacion)),
    porte_anestesico_max = dplyr::if_else(is.infinite(porte_anestesico_max) | is.nan(porte_anestesico_max), 0, as.numeric(porte_anestesico_max)),
    costo_log1p = log1p(pmax(costo_total_positivo_utilizacion, 0))
  )

# Resumir la distribución de costos observados y costos usados para modelamiento.
resumen_costos_observados <- dplyr::bind_rows(
  resumir_costo(utilizaciones_N20_costos$costo_total_observado_utilizacion) |> dplyr::mutate(tipo_costo = "costo_total_observado_utilizacion"),
  resumir_costo(utilizaciones_N20_costos$costo_total_positivo_utilizacion) |> dplyr::mutate(tipo_costo = "costo_total_positivo_utilizacion")
) |>
  dplyr::select(tipo_costo, dplyr::everything())

# Conservar solo utilizaciones con costo positivo para ajustar modelos de costo.
datos_modelo_costo <- utilizaciones_N20_costos |>
  dplyr::filter(costo_total_positivo_utilizacion > 0)

if (nrow(datos_modelo_costo) < 30) {
  stop("No hay suficientes utilizaciones N20 con costo positivo para ajustar modelos de costo. Filas disponibles: ", nrow(datos_modelo_costo))
}

# Usar una edad de referencia para completar edades faltantes en el modelo de costos.
edad_ref_costo <- mediana_segura(datos_modelo_costo$edad_utilizacion, defecto = mediana_segura(analitica_beneficiario_auditada$edad_ultima_utilizacion_modelo, defecto = 0))

# Preparar predictores del modelo de costo: categorías agrupadas, indicadores clínicos y transformación logarítmica.
datos_modelo_costo <- datos_modelo_costo |>
  dplyr::mutate(
    edad_modelo = dplyr::if_else(is.na(edad_utilizacion), edad_ref_costo, edad_utilizacion),
    sexo_modelo = factor(agrupar_top(sexo, 4)),
    tipo_beneficiario_modelo = factor(agrupar_top(tipo_beneficiario, 5)),
    uti_modelo = factor(stringr::str_to_upper(uti)),
    internacion_modelo = factor(stringr::str_to_upper(internacion)),
    especialidad_modelo = factor(agrupar_top(especialidad, 8)),
    unidad_modelo = factor(agrupar_top(unidad_prestador, 8)),
    estado_modelo = factor(agrupar_top(estado_prestador, 8)),
    procedimiento_modelo = factor(agrupar_top(procedimiento_descripcion_principal, 10)),
    n_registros_n20_modelo = as.numeric(n_registros_n20),
    n_procedimientos_modelo = as.numeric(n_procedimientos_distintos),
    porte_anestesico_modelo = as.numeric(porte_anestesico_max),
    log1p_costo_modelo = log1p(costo_total_positivo_utilizacion)  # Transforma costo positivo para modelar asimetría.
  )

# Definir candidatos a predictores para explicar el costo de utilizaciones N20.
predictores_costo <- c(
  "edad_modelo", "sexo_modelo", "tipo_beneficiario_modelo", "uti_modelo", "internacion_modelo",
  "especialidad_modelo", "unidad_modelo", "estado_modelo", "procedimiento_modelo",
  "n_registros_n20_modelo", "n_procedimientos_modelo", "porte_anestesico_modelo"
)
# Retirar predictores sin variabilidad, porque no aportan al ajuste.
predictores_costo <- predictores_costo[purrr::map_lgl(datos_modelo_costo[predictores_costo], ~ dplyr::n_distinct(.x, na.rm = TRUE) > 1)]

modelo_costo_loglineal <- stats::lm(stats::reformulate(predictores_costo, "log1p_costo_modelo"), data = datos_modelo_costo)  # Ajusta modelo log-lineal para costo positivo N20.
factor_smearing <- mean(exp(stats::residuals(modelo_costo_loglineal)), na.rm = TRUE)  # Calcula corrección de smearing para volver de log-costo a costo.
# Predecir costos en la escala original con el modelo log-lineal.
pred_loglineal_costo <- pmax(exp(as.numeric(stats::predict(modelo_costo_loglineal, datos_modelo_costo))) * factor_smearing - 1, 0)

modelo_costo_gamma <- tryCatch(  # Ajusta modelo Gamma log-link como alternativa para costos asimétricos.
  stats::glm(stats::reformulate(predictores_costo, "costo_total_positivo_utilizacion"), family = Gamma(link = "log"), data = datos_modelo_costo, control = stats::glm.control(maxit = 100)),
  error = function(e) e
)
# Identificar si el modelo Gamma se ajustó correctamente antes de usar sus predicciones.
gamma_ok <- inherits(modelo_costo_gamma, "glm")
pred_gamma_costo <- if (gamma_ok) pmax(as.numeric(stats::predict(modelo_costo_gamma, datos_modelo_costo, type = "response")), 0) else rep(NA_real_, nrow(datos_modelo_costo))

# Función para evaluar modelos de costo con error absoluto, error cuadrático, sesgo y correlación.
metricas_regresion <- function(obs, pred) {
  obs <- as.numeric(obs)
  pred <- as.numeric(pred)
  ok <- is.finite(obs) & is.finite(pred)
  if (sum(ok) == 0) {
    return(tibble::tibble(mae = NA_real_, rmse = NA_real_, sesgo_promedio = NA_real_, correlacion = NA_real_))
  }
  tibble::tibble(
    mae = mean(abs(obs[ok] - pred[ok]), na.rm = TRUE),
    rmse = sqrt(mean((obs[ok] - pred[ok])^2, na.rm = TRUE)),
    sesgo_promedio = mean(pred[ok] - obs[ok], na.rm = TRUE),
    correlacion = if (sum(ok) > 1) suppressWarnings(stats::cor(obs[ok], pred[ok], use = "complete.obs")) else NA_real_
  )
}

tabla_modelos_costo <- dplyr::bind_rows(  # Compara modelos de costo con MAE, RMSE, sesgo y correlación.
  metricas_regresion(datos_modelo_costo$costo_total_positivo_utilizacion, pred_loglineal_costo) |>
    dplyr::mutate(modelo = "Log-lineal con smearing", ajustado = TRUE),
  metricas_regresion(datos_modelo_costo$costo_total_positivo_utilizacion, pred_gamma_costo) |>
    dplyr::mutate(modelo = "Gamma log-link", ajustado = gamma_ok)
) |>
  dplyr::select(modelo, ajustado, mae, rmse, sesgo_promedio, correlacion) |>
  dplyr::mutate(rmse_valido = is.finite(rmse) & !is.na(rmse)) |>
  dplyr::arrange(dplyr::desc(ajustado), dplyr::desc(rmse_valido), rmse)

# Elegir el modelo de costo con menor RMSE entre los modelos ajustados válidos.
modelo_costo_recomendado <- tabla_modelos_costo |>
  dplyr::filter(ajustado, rmse_valido) |>
  dplyr::arrange(rmse) |>
  dplyr::pull(modelo)

# Definir un modelo alternativo si no hay un candidato válido por RMSE.
modelo_costo_recomendado <- if (length(modelo_costo_recomendado) == 0 || is.na(modelo_costo_recomendado[1])) {
  "Log-lineal con smearing"
} else {
  modelo_costo_recomendado[1]
}

# Función para crear perfiles de referencia a partir de medianas y categorías dominantes.
crear_perfil_costo <- function(nombre, internacion_valor = NULL, uti_valor = NULL) {
  datos_ref <- datos_modelo_costo
  if (!is.null(internacion_valor)) datos_ref <- datos_ref |> dplyr::filter(as.character(internacion_modelo) == internacion_valor)
  if (!is.null(uti_valor)) datos_ref <- datos_ref |> dplyr::filter(as.character(uti_modelo) == uti_valor)
  if (nrow(datos_ref) == 0) datos_ref <- datos_modelo_costo
  tibble::tibble(
    perfil = nombre,
    edad_modelo = mediana_segura(datos_ref$edad_modelo, defecto = edad_ref_costo),
    sexo_modelo = moda_segura(datos_ref$sexo_modelo),
    tipo_beneficiario_modelo = moda_segura(datos_ref$tipo_beneficiario_modelo),
    uti_modelo = ifelse(is.null(uti_valor), moda_segura(datos_ref$uti_modelo), uti_valor),
    internacion_modelo = ifelse(is.null(internacion_valor), moda_segura(datos_ref$internacion_modelo), internacion_valor),
    especialidad_modelo = moda_segura(datos_ref$especialidad_modelo),
    unidad_modelo = moda_segura(datos_ref$unidad_modelo),
    estado_modelo = moda_segura(datos_ref$estado_modelo),
    procedimiento_modelo = moda_segura(datos_ref$procedimiento_modelo),
    n_registros_n20_modelo = mediana_segura(datos_ref$n_registros_n20_modelo, defecto = 1),
    n_procedimientos_modelo = mediana_segura(datos_ref$n_procedimientos_modelo, defecto = 1),
    porte_anestesico_modelo = mediana_segura(datos_ref$porte_anestesico_modelo, defecto = 0),
    n_utilizaciones_referencia = nrow(datos_ref),
    costo_observado_mediano_referencia = mediana_segura(datos_ref$costo_total_positivo_utilizacion, defecto = 0),
    costo_observado_promedio_referencia = promedio_seguro(datos_ref$costo_total_positivo_utilizacion, defecto = 0)
  )
}

perfiles_costo_esperado <- dplyr::bind_rows(  # Construye perfiles de referencia para estimar costo esperado N20.
  crear_perfil_costo("Ambulatorio sin UTI", internacion_valor = "AMBULATORIO", uti_valor = "SIN UTI"),
  crear_perfil_costo("Internado sin UTI", internacion_valor = "INTERNADO", uti_valor = "SIN UTI"),
  crear_perfil_costo("Internado con UTI", internacion_valor = "INTERNADO", uti_valor = "CON UTI"),
  crear_perfil_costo("Perfil dominante observado")
)

# Alinear niveles de factores entre los perfiles nuevos y los datos usados para entrenar costos.
for (v in predictores_costo) {
  if (v %in% names(perfiles_costo_esperado) && is.factor(datos_modelo_costo[[v]])) {
    niveles <- levels(datos_modelo_costo[[v]])
    valores <- as.character(perfiles_costo_esperado[[v]])
    valores[!(valores %in% niveles) | is.na(valores)] <- moda_segura(datos_modelo_costo[[v]])
    perfiles_costo_esperado[[v]] <- factor(valores, levels = niveles)
  }
}

# Estimar costo esperado de cada perfil con el modelo log-lineal.
perfiles_costo_esperado$costo_esperado_loglineal <- tryCatch(
  pmax(exp(as.numeric(stats::predict(modelo_costo_loglineal, perfiles_costo_esperado))) * factor_smearing - 1, 0),
  error = function(e) rep(NA_real_, nrow(perfiles_costo_esperado))
)

# Estimar costo esperado de cada perfil con el modelo Gamma cuando está disponible.
perfiles_costo_esperado$costo_esperado_gamma <- if (gamma_ok) {
  tryCatch(
    pmax(as.numeric(stats::predict(modelo_costo_gamma, perfiles_costo_esperado, type = "response")), 0),
    error = function(e) rep(NA_real_, nrow(perfiles_costo_esperado))
  )
} else {
  rep(NA_real_, nrow(perfiles_costo_esperado))
}

# Asignar el costo esperado recomendado según el modelo seleccionado.
if (identical(modelo_costo_recomendado, "Gamma log-link") && any(is.finite(perfiles_costo_esperado$costo_esperado_gamma))) {
  perfiles_costo_esperado$costo_esperado_recomendado <- perfiles_costo_esperado$costo_esperado_gamma
} else {
  perfiles_costo_esperado$costo_esperado_recomendado <- perfiles_costo_esperado$costo_esperado_loglineal
  modelo_costo_recomendado <- "Log-lineal con smearing"
}

perfiles_costo_esperado$modelo_recomendado <- modelo_costo_recomendado

# Registrar límites metodológicos del componente de costos.
limitaciones_costo <- tibble::tribble(
  ~tema, ~limitacion,
  "Cambio de unidad", "La clasificación es por beneficiario; el costo se estima por utilización N20 aproximada.",
  "Costo observado", "Solo resume registros con CID_NORM LIKE 'N20%'.",
  "Costo esperado", "Los modelos explican patrones observados, no causalidad clínica.",
  "Asimetría", "Se reportan mediana y percentiles porque el promedio es sensible a outliers.",
  "Perfiles", "Los perfiles son escenarios sintéticos basados en valores dominantes o medianos."
)

guardar_csv(utilizaciones_N20_costos |> dplyr::mutate(fecha_utilizacion = as.character(fecha_utilizacion)), "compacto_07_utilizaciones_N20_costos.csv")
guardar_csv(resumen_costos_observados, "compacto_07_resumen_costos_observados.csv")  # Exporta estadísticas observadas de costo N20.
guardar_csv(tabla_modelos_costo, "compacto_07_tabla_modelos_costo.csv")
guardar_csv(perfiles_costo_esperado, "compacto_07_perfiles_costo_esperado.csv")  # Exporta costo esperado por perfiles.
guardar_csv(limitaciones_costo, "compacto_07_limitaciones_costo.csv")

guardar_tabla_sqlite(utilizaciones_N20_costos |> dplyr::mutate(fecha_utilizacion = as.character(fecha_utilizacion)), "compacto_utilizaciones_N20_costos", overwrite = TRUE)
guardar_tabla_sqlite(perfiles_costo_esperado, "compacto_perfiles_costo_esperado", overwrite = TRUE)

#------------------------------------------------------------
# 8. Gráficos de resultados
#------------------------------------------------------------

# Generar gráficos de apoyo cuando crear_graficos está activo.
if (crear_graficos) {
  # Comparar métricas principales de los modelos de clasificación.
  grafico_metricas <- metricas_modelos |>
    dplyr::mutate(dplyr::across(c(pr_auc, sens, precision, f_meas), ~ suppressWarnings(as.numeric(.x)))) |>
    tidyr::pivot_longer(cols = c(pr_auc, sens, precision, f_meas), names_to = "metrica", values_to = "valor") |>
    dplyr::filter(is.finite(valor)) |>
    ggplot2::ggplot(ggplot2::aes(x = modelo, y = valor, fill = metrica)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Comparación compacta de modelos", x = "Modelo", y = "Valor", fill = "Métrica")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_01_metricas_modelos.png"), grafico_metricas, width = 9, height = 6, dpi = 300)
  
  # Visualizar aciertos y errores del modelo candidato.
  grafico_matriz <- matriz_candidato |>
    ggplot2::ggplot(ggplot2::aes(x = pred_clase, y = verdad, fill = n)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(n))) +
    ggplot2::labs(title = "Matriz de confusión del modelo candidato", x = "Predicción", y = "Real", fill = "n")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_02_matriz_confusion_candidato.png"), grafico_matriz, width = 7, height = 5, dpi = 300)
  
  # Mostrar el intercambio entre sensibilidad, precisión y F1 al mover el umbral.
  grafico_umbral <- metricas_por_umbral_candidato |>
    dplyr::select(tipo_umbral, umbral, sens, precision, f_meas) |>
    tidyr::pivot_longer(cols = c(sens, precision, f_meas), names_to = "metrica", values_to = "valor") |>
    ggplot2::ggplot(ggplot2::aes(x = umbral, y = valor, linetype = metrica)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(title = "Métricas del candidato por umbral", x = "Umbral", y = "Valor", linetype = "Métrica")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_02b_metricas_por_umbral_candidato.png"), grafico_umbral, width = 9, height = 6, dpi = 300)
  
  # Preparar variables importantes para graficarlas en orden ascendente.
  datos_importancia_grafico <- importancia_top |>
    dplyr::mutate(importancia = suppressWarnings(as.numeric(importancia))) |>
    dplyr::filter(!is.na(variable), is.finite(importancia)) |>
    dplyr::arrange(importancia) |>
    dplyr::mutate(variable = factor(variable, levels = variable))
  
  grafico_importancia <- datos_importancia_grafico |>
    ggplot2::ggplot(ggplot2::aes(x = variable, y = importancia)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Variables importantes del random forest", x = "Variable", y = "Importancia")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_03_importancia_variables.png"), grafico_importancia, width = 10, height = 8, dpi = 300)
  
  # Graficar la distribución transformada de costos N20.
  grafico_costos <- utilizaciones_N20_costos |>
    dplyr::filter(is.finite(costo_total_positivo_utilizacion), costo_total_positivo_utilizacion >= 0) |>
    ggplot2::ggplot(ggplot2::aes(x = log1p(costo_total_positivo_utilizacion))) +
    ggplot2::geom_histogram(bins = 50) +
    ggplot2::labs(title = "Distribución de costos N20", x = "log1p(costo positivo)", y = "Utilizaciones")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_04_distribucion_costos_n20.png"), grafico_costos, width = 9, height = 6, dpi = 300)
  
  # Comparar costo observado y estimado para perfiles de referencia.
  grafico_perfiles <- perfiles_costo_esperado |>
    dplyr::select(perfil, costo_observado_mediano_referencia, costo_observado_promedio_referencia, costo_esperado_recomendado) |>
    tidyr::pivot_longer(cols = -perfil, names_to = "tipo_valor", values_to = "costo") |>
    dplyr::filter(is.finite(costo)) |>
    ggplot2::ggplot(ggplot2::aes(x = perfil, y = costo, fill = tipo_valor)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = "Costo observado y esperado por perfiles N20", x = "Perfil", y = "Costo", fill = "Valor")
  ggplot2::ggsave(file.path(carpeta_figuras, "compacto_05_perfiles_costo_esperado.png"), grafico_perfiles, width = 10, height = 6, dpi = 300)
}

#------------------------------------------------------------
# 9. Síntesis de resultados
#------------------------------------------------------------

# Consolidar métricas finales para consulta de resultados.
metricas_finales_consolidadas <- dplyr::bind_rows(  # Integra métricas finales de clasificación y costos.
  metricas_candidato_final |>
    tidyr::pivot_longer(
      cols = c(roc_auc, pr_auc, accuracy, sens, spec, precision, f_meas),
      names_to = "indicador",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      bloque = "Clasificación N20",
      valor = as.character(valor),
      fuente = "compacto_05_metricas_candidato_final.csv"
    ) |>
    dplyr::select(bloque, indicador, valor, fuente),
  resumen_errores |>
    dplyr::transmute(
      bloque = "Errores de clasificación",
      indicador = tipo_resultado,
      valor = as.character(beneficiarios),
      fuente = "compacto_05_resumen_errores_candidato.csv"
    ),
  tibble::tibble(
    bloque = "Costos N20",
    indicador = c(
      "registros_n20", "utilizaciones_n20_costos", "beneficiarios_n20", "costo_total_observado",
      "costo_promedio_observado", "costo_mediano_observado", "costo_p95_observado",
      "modelo_costo_recomendado", "perfiles_estimados"
    ),
    valor = as.character(c(
      nrow(base_n20),
      nrow(utilizaciones_N20_costos),
      dplyr::n_distinct(utilizaciones_N20_costos$CHAVE_FUNCIONAL),
      sum(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE),
      mean(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE),
      median(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE),
      as.numeric(stats::quantile(utilizaciones_N20_costos$costo_total_positivo_utilizacion, 0.95, na.rm = TRUE)),
      modelo_costo_recomendado,
      nrow(perfiles_costo_esperado)
    )),
    fuente = "compacto_07_resumen_costos_observados.csv"
  )
)

decisiones_metodologicas <- tibble::tribble(  # Resume decisiones metodológicas principales del flujo.
  ~dimension, ~decision, ~justificacion,
  "Enfermedad objetivo", "Usar CID_NORM LIKE 'N20%'", "La enfermedad se define por diagnóstico CID; CD_PROCEDIMENTO no etiqueta enfermedad.",
  "Unidad de clasificación", "Modelar a nivel CHAVE_FUNCIONAL", "El objetivo del taller es clasificar beneficiarios.",
  "Unidad de costo", "Analizar costo por utilización N20 aproximada", "El costo esperado se estima sobre CHAVE_FUNCIONAL + fecha_utilizacion en registros N20.",
  "Variables prohibidas", "Excluir CID, flag_n20 y derivados directos", "Evita fuga de información diagnóstica.",
  "Desbalance", "Priorizar PR-AUC, sensibilidad, precisión y F1", "La clase N20 es rara y accuracy puede ser engañosa.",
  "Modelos", "Comparar regresión logística, árbol y Random Forest", "Sigue la secuencia de clases: modelo base, árbol interpretable y ensamble.",
  "Interpretación", "Usar importancia de variables y lectura no causal", "La importancia predice, pero no demuestra causalidad clínica."
)

mapa_resultados_codigo <- tibble::tribble(  # Relaciona cada componente del análisis con sus archivos de salida.
  ~componente, ~archivo_principal_generado,
  "Preparación de datos", "compacto_01_control_preparacion_cid_objetivo.csv; compacto_02b_control_calidad.csv",
  "Variable objetivo", "compacto_01_control_preparacion_cid_objetivo.csv; objetivo_beneficiario en SQLite si crear_sqlite=TRUE",
  "Análisis descriptivo", "compacto_03_resumen_descriptivo_general.csv y tablas descriptivas sexo/edad/tipo/estado/especialidad",
  "Entrenamiento de modelos", "compacto_05_diagnostico_modelos.csv y objetos RDS si guardar_modelos=TRUE",
  "Evaluación y comparación", "compacto_05_metricas_modelos.csv; compacto_05_matriz_confusion_candidato.csv; compacto_05_metricas_por_umbral_candidato.csv",
  "Interpretación", "compacto_06_importancia_variables_top.csv; compacto_06_discusion_sesgos_limitaciones.csv",
  "Estimación de costo esperado", "compacto_07_resumen_costos_observados.csv; compacto_07_tabla_modelos_costo.csv; compacto_07_perfiles_costo_esperado.csv"
)

# Crear un resumen ejecutivo con resultados principales y fecha de procesamiento.
resumen_flujo_compacto <- tibble::tibble(
  elemento = c(
    "flujo", "enfermedad", "codigo_cid", "beneficiarios", "positivos_n20", "prevalencia_pct",
    "modelos_entrenados", "modelos_con_metricas_finitas", "modelo_candidato", "umbral_candidato",
    "roc_auc", "pr_auc", "accuracy", "sensibilidad", "especificidad", "precision", "f1",
    "utilizaciones_n20_costos", "costo_total_n20", "costo_promedio_n20", "costo_mediano_n20",
    "modelo_costo_recomendado", "fecha_procesamiento"
  ),
  valor = c(
    "Análisis Taller 3 Minería de Datos",
    nombre_enfermedad,
    codigo_cid,
    as.character(nrow(analitica_beneficiario_auditada)),
    as.character(sum(analitica_beneficiario_auditada$tiene_calculo_renal == 1, na.rm = TRUE)),
    as.character(100 * mean(analitica_beneficiario_auditada$tiene_calculo_renal == 1, na.rm = TRUE)),
    as.character(nrow(metricas_modelos)),
    as.character(modelos_con_metricas_finitas),
    modelo_candidato_nombre,
    as.character(umbral_candidato),
    as.character(metricas_candidato_final$roc_auc[1]),
    as.character(metricas_candidato_final$pr_auc[1]),
    as.character(metricas_candidato_final$accuracy[1]),
    as.character(metricas_candidato_final$sens[1]),
    as.character(metricas_candidato_final$spec[1]),
    as.character(metricas_candidato_final$precision[1]),
    as.character(metricas_candidato_final$f_meas[1]),
    as.character(nrow(utilizaciones_N20_costos)),
    as.character(sum(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE)),
    as.character(mean(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE)),
    as.character(median(utilizaciones_N20_costos$costo_total_positivo_utilizacion, na.rm = TRUE)),
    modelo_costo_recomendado,
    as.character(Sys.time())
  )
)

guardar_csv(metricas_finales_consolidadas, "compacto_08_metricas_finales_consolidadas.csv")
guardar_csv(decisiones_metodologicas, "compacto_08_decisiones_metodologicas.csv")
guardar_csv(mapa_resultados_codigo, "compacto_08_mapa_resultados_codigo.csv")
guardar_csv(resumen_flujo_compacto, "compacto_08_resumen_flujo_compacto.csv")

guardar_tabla_sqlite(metricas_finales_consolidadas, "compacto_metricas_finales_consolidadas", overwrite = TRUE)
guardar_tabla_sqlite(decisiones_metodologicas, "compacto_decisiones_metodologicas", overwrite = TRUE)
guardar_tabla_sqlite(mapa_resultados_codigo, "compacto_mapa_resultados_codigo", overwrite = TRUE)
guardar_tabla_sqlite(resumen_flujo_compacto, "compacto_resumen_flujo", overwrite = TRUE)

guardar_rds(list(
  receta = receta_preparada,
  modelo_logistico = modelo_logistico,
  modelo_arbol = modelo_arbol,
  modelo_rf = modelo_rf,
  modelo_costo_loglineal = modelo_costo_loglineal,
  modelo_costo_gamma = modelo_costo_gamma,
  resumen = resumen_flujo_compacto
), "compacto_objetos_finales.rds")

#------------------------------------------------------------
# 10. Cierre del procesamiento
#------------------------------------------------------------

# Comprobar que estén disponibles las tablas principales del análisis.
salidas_criticas <- c(
  "compacto_01_control_preparacion_cid_objetivo.csv",
  "compacto_02b_control_calidad.csv",
  "compacto_03_resumen_descriptivo_general.csv",
  "compacto_03_tabla_descriptiva_edad.csv",
  "compacto_03_tabla_descriptiva_estado_prestador.csv",
  "compacto_05_metricas_modelos.csv",
  "compacto_05_metricas_candidato_final.csv",
  "compacto_05_matriz_confusion_candidato.csv",
  "compacto_06_importancia_variables_top.csv",
  "compacto_07_resumen_costos_observados.csv",
  "compacto_07_perfiles_costo_esperado.csv",
  "compacto_08_resumen_flujo_compacto.csv"
)

salidas_faltantes <- salidas_criticas[!file.exists(file.path(carpeta_salida, salidas_criticas))]
if (length(salidas_faltantes) > 0) {
  stop("Faltan salidas principales del análisis: ", paste(salidas_faltantes, collapse = ", "))
}

if (mostrar_resumen_consola) {  # Muestra un cierre breve de la ejecución.
  print(resumen_flujo_compacto)
  message("Implementación del taller 3 terminada correctamente.")
  message("Modelo candidato: ", modelo_candidato_nombre)
  message("Modelos con métricas finitas: ", modelos_con_metricas_finitas, " de ", nrow(metricas_modelos))
  message("Resultados guardados en: ", carpeta_salida)
  if (crear_sqlite) message("SQLite de resultados: ", ruta_sqlite_taller3)
}

