# ==============================================================================
# TALLER 3 - MINERÍA DE DATOS
# ETAPA 2: CONSTRUCCIÓN DE VARIABLES K80 POR BLOQUES - VERSIÓN FINAL
# ==============================================================================
#
# Esta versión:
#   - NO usa DuckDB.
#   - NO carga los 9,3 millones de filas completas en memoria.
#   - Lee el CSV en bloques pequeños con readr.
#   - Acumula resultados en una base SQLite alojada en disco.
#   - Conserva una sola fila final por beneficiario.
#
# Requisitos previos en resultados/:
#   - target_k80_beneficiarios.rds
#   - particiones_k80_principal.rds
#
# Salidas principales:
#   data/processed/taller3_k80.sqlite
#   resultados/resultados_etapa_2.rds
#   tablas CSV descriptivas
#   figuras PNG
# ==============================================================================


# 0. CONFIGURACIÓN -------------------------------------------------------------

rm(list = ls())
invisible(gc(full = TRUE))

options(
  scipen = 999,
  stringsAsFactors = FALSE,
  readr.show_progress = FALSE,
  dplyr.summarise.inform = FALSE
)

set.seed(2016325)

paquetes <- c(
  "DBI",
  "RSQLite",
  "readr",
  "data.table",
  "stringi",
  "ggplot2",
  "here"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Faltan estos paquetes:\n",
    paste(faltantes, collapse = ", "),
    "\n\nInstálelos con:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', faltantes), collapse = ", "),
    "))"
  )
}

library(DBI)
library(RSQLite)
library(readr)
library(data.table)
library(stringi)
library(ggplot2)
library(here)


# 1. PARÁMETROS ---------------------------------------------------------------

# Mantener TRUE al ejecutar por primera vez.
# Si se vuelve a ejecutar, borra la base SQLite anterior y reconstruye todo.
reiniciar_proceso <- TRUE

# Si la sesión se vuelve inestable, reducir a 10000.
tamano_bloque <- 10000L

ruta_base <- here("data", "raw", "db_2026.csv")

ruta_target <- here(
  "resultados",
  "target_k80_beneficiarios.rds"
)

ruta_particiones <- here(
  "resultados",
  "particiones_k80_principal.rds"
)

ruta_sqlite <- here(
  "data",
  "processed",
  "taller3_k80.sqlite"
)

carpetas <- c(
  here("data", "processed"),
  here("resultados"),
  here("figuras"),
  here("modelos")
)

invisible(lapply(
  carpetas,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

archivos_requeridos <- c(
  ruta_base,
  ruta_target,
  ruta_particiones
)

faltan_archivos <- archivos_requeridos[
  !file.exists(archivos_requeridos)
]

if (length(faltan_archivos) > 0) {
  stop(
    "No se encontraron estos archivos:\n",
    paste(faltan_archivos, collapse = "\n")
  )
}

if (
  reiniciar_proceso &&
  file.exists(ruta_sqlite)
) {
  file.remove(ruta_sqlite)
}


# 2. FUNCIONES DE LIMPIEZA -----------------------------------------------------

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- stri_trim_both(x)
  x <- stri_trans_toupper(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- stri_replace_all_regex(x, "\\s+", " ")

  x[
    is.na(x) |
      x %in% c(
        "",
        "NA",
        "N/A",
        "NULL",
        "-",
        "NAO INFORMADO",
        "NAO INFORMADA"
      )
  ] <- NA_character_

  x
}

normalizar_sexo <- function(x) {
  x <- normalizar_texto(x)

  fcase(
    x %chin% c("M", "MASCULINO"), "M",
    x %chin% c("F", "FEMININO"), "F",
    default = NA_character_
  )
}

normalizar_tipo_beneficiario <- function(x) {
  x <- normalizar_texto(x)

  fcase(
    stri_detect_regex(x, "TITULAR"), "TITULAR",
    stri_detect_regex(
      x,
      "DEPENDENTE|CONJUGE|FILHO|FILHA|MAE|PAI"
    ), "DEPENDIENTE",
    is.na(x), NA_character_,
    default = "OTROS"
  )
}

normalizar_servicio <- function(x) {
  x <- normalizar_texto(x)

  fcase(
    stri_detect_regex(x, "^CONSULT"), "CONSULTA",
    stri_detect_regex(x, "^EXAM"), "EXAMEN",
    stri_detect_regex(x, "^TERAP"), "TERAPIA",
    stri_detect_regex(x, "^INTERN"), "INTERNACION",
    stri_detect_regex(
      x,
      "PRONTO|SOCORRO|URGEN"
    ), "URGENCIAS",
    is.na(x), "SIN INFORMACION",
    default = "OTROS"
  )
}

normalizar_unidad <- function(x) {
  x <- normalizar_texto(x)

  fcase(
    stri_detect_regex(
      x,
      "PRONTO|SOCORRO|URGEN"
    ), "URGENCIAS",

    stri_detect_regex(
      x,
      "HOSPITAL"
    ), "HOSPITAL",

    stri_detect_regex(
      x,
      "LABORATORIO|DIAGNOSE|DIAGNOST|SADT"
    ), "DIAGNOSTICO",

    stri_detect_regex(
      x,
      "CONSULTORIO|CLINICA|POLICLINICA|ESPECIALIDADE"
    ), "ATENCION_ESPECIALIZADA",

    stri_detect_regex(
      x,
      "BASICA|POSTO|CENTRO DE SAUDE"
    ), "ATENCION_PRIMARIA",

    stri_detect_regex(
      x,
      "DOMICILIAR|HOME CARE"
    ), "DOMICILIARIO",

    stri_detect_regex(
      x,
      "MOVEL"
    ), "MOVIL",

    is.na(x), NA_character_,
    default = "OTROS"
  )
}

normalizar_binaria <- function(x) {
  x_txt <- normalizar_texto(x)
  x_num <- suppressWarnings(
    as.numeric(
      stri_replace_all_fixed(
        as.character(x),
        ",",
        "."
      )
    )
  )

  fcase(
    !is.na(x_num) & x_num > 0, 1L,
    !is.na(x_num) & x_num == 0, 0L,
    x_txt %chin% c("SIM", "S", "TRUE"), 1L,
    x_txt %chin% c("NAO", "N", "FALSE"), 0L,
    default = NA_integer_
  )
}

normalizar_numero <- function(x) {
  suppressWarnings(
    as.numeric(
      stri_replace_all_fixed(
        stri_trim_both(as.character(x)),
        ",",
        "."
      )
    )
  )
}

normalizar_fecha <- function(x) {
  x <- stri_trim_both(as.character(x))

  salida <- suppressWarnings(
    as.Date(x)
  )

  as.character(salida)
}

min_fecha_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  min(x)
}

max_fecha_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  max(x)
}

max_num_seguro <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

sum_num_seguro <- function(x) {
  sum(x, na.rm = TRUE)
}


# 3. CONEXIÓN SQLITE -----------------------------------------------------------

# Se abre una conexión corta por bloque. Esto evita conservar durante horas
# una conexión que pueda quedar inválida después de una interrupción.

abrir_conexion <- function() {

  con_nueva <- dbConnect(
    RSQLite::SQLite(),
    dbname = ruta_sqlite
  )

  dbExecute(
    con_nueva,
    "PRAGMA journal_mode = DELETE"
  )

  dbExecute(
    con_nueva,
    "PRAGMA synchronous = NORMAL"
  )

  dbExecute(
    con_nueva,
    "PRAGMA temp_store = FILE"
  )

  dbExecute(
    con_nueva,
    "PRAGMA cache_size = -75000"
  )

  dbExecute(
    con_nueva,
    "PRAGMA busy_timeout = 60000"
  )

  con_nueva
}

con <- abrir_conexion()

asegurar_conexion <- function() {

  if (
    !exists("con", inherits = TRUE) ||
    !DBI::dbIsValid(con)
  ) {
    con <<- abrir_conexion()
  }

  invisible(con)
}

cerrar_conexion_segura <- function() {

  if (
    exists("con", inherits = TRUE) &&
    DBI::dbIsValid(con)
  ) {
    DBI::dbDisconnect(con)
  }

  invisible(NULL)
}


# 4. TARGET Y PARTICIONES ------------------------------------------------------

cat("\nCargando target y particiones...\n")

target <- as.data.table(
  readRDS(ruta_target)
)

particiones <- as.data.table(
  readRDS(ruta_particiones)
)

setnames(
  target,
  old = "CHAVE_FUNCIONAL",
  new = "chave_funcional"
)

setnames(
  particiones,
  old = "CHAVE_FUNCIONAL",
  new = "chave_funcional"
)

target[, chave_funcional :=
  stri_trim_both(
    as.character(chave_funcional)
  )
]

particiones[, chave_funcional :=
  stri_trim_both(
    as.character(chave_funcional)
  )
]

particiones[, particion :=
  as.character(particion)
]

if (anyDuplicated(target$chave_funcional) > 0L) {
  stop("El target contiene beneficiarios duplicados.")
}

if (anyDuplicated(particiones$chave_funcional) > 0L) {
  stop("Las particiones contienen beneficiarios duplicados.")
}

lookup <- merge(
  target,
  particiones[
    ,
    .(
      chave_funcional,
      particion
    )
  ],
  by = "chave_funcional",
  all.x = TRUE,
  sort = FALSE
)

if (anyNA(lookup$particion)) {
  stop(
    "Hay beneficiarios del target sin partición asignada."
  )
}

setkey(
  lookup,
  chave_funcional
)

dbWriteTable(
  con,
  "target_particiones",
  lookup,
  overwrite = TRUE
)

dbExecute(
  con,
"
CREATE UNIQUE INDEX IF NOT EXISTS
idx_target_id
ON target_particiones(chave_funcional)
"
)


# 5. TABLAS ACUMULADORAS -------------------------------------------------------

dbExecute(
  con,
"
CREATE TABLE IF NOT EXISTS agg_beneficiario (
  chave_funcional TEXT PRIMARY KEY,

  n_procedimientos INTEGER NOT NULL DEFAULT 0,

  n_consultas INTEGER NOT NULL DEFAULT 0,
  n_examenes INTEGER NOT NULL DEFAULT 0,
  n_terapias INTEGER NOT NULL DEFAULT 0,
  n_internaciones_registro INTEGER NOT NULL DEFAULT 0,
  n_urgencias INTEGER NOT NULL DEFAULT 0,
  n_otros INTEGER NOT NULL DEFAULT 0,
  n_sin_tipo_servicio INTEGER NOT NULL DEFAULT 0,

  n_registros_uci INTEGER NOT NULL DEFAULT 0,
  n_registros_internado INTEGER NOT NULL DEFAULT 0,
  n_registros_anestesia INTEGER NOT NULL DEFAULT 0,

  costo_total_positivo REAL NOT NULL DEFAULT 0,
  n_costos_positivos INTEGER NOT NULL DEFAULT 0,
  n_costos_cero INTEGER NOT NULL DEFAULT 0,
  n_costos_negativos INTEGER NOT NULL DEFAULT 0,
  n_costos_faltantes INTEGER NOT NULL DEFAULT 0,
  costo_maximo_positivo REAL,

  n_unidad_faltante INTEGER NOT NULL DEFAULT 0,
  n_especialidad_faltante INTEGER NOT NULL DEFAULT 0,

  fecha_primera_utilizacion TEXT,
  fecha_ultima_utilizacion TEXT,

  tuvo_gastroenterologia INTEGER NOT NULL DEFAULT 0,
  tuvo_cirugia INTEGER NOT NULL DEFAULT 0,
  tuvo_imagenologia INTEGER NOT NULL DEFAULT 0,
  tuvo_ecografia_abdominal INTEGER NOT NULL DEFAULT 0,
  tuvo_procedimiento_biliar INTEGER NOT NULL DEFAULT 0
)
"
)

crear_tabla_conteos <- function(nombre) {
  dbExecute(
    con,
    sprintf(
"
CREATE TABLE IF NOT EXISTS %s (
  chave_funcional TEXT NOT NULL,
  valor TEXT NOT NULL,
  n INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (chave_funcional, valor)
)
",
      nombre
    )
  )
}

crear_tabla_conteos("conteo_sexo")
crear_tabla_conteos("conteo_tipo_beneficiario")
crear_tabla_conteos("conteo_estado")
crear_tabla_conteos("conteo_unidad")
crear_tabla_conteos("conteo_nacimiento")

dbExecute(
  con,
"
CREATE TABLE IF NOT EXISTS resumen_especialidades (
  target_k80 INTEGER NOT NULL,
  valor TEXT NOT NULL,
  n_registros INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (target_k80, valor)
)
"
)

dbExecute(
  con,
"
CREATE TABLE IF NOT EXISTS resumen_procedimientos (
  target_k80 INTEGER NOT NULL,
  codigo TEXT NOT NULL,
  descripcion TEXT NOT NULL,
  n_registros INTEGER NOT NULL DEFAULT 0,
  costo_total_positivo REAL NOT NULL DEFAULT 0,
  PRIMARY KEY (target_k80, codigo, descripcion)
)
"
)


# 6. FUNCIONES DE ACTUALIZACIÓN SQLITE ----------------------------------------

actualizar_agg_beneficiario <- function(tabla) {

  if (nrow(tabla) == 0L) {
    return(invisible(NULL))
  }

  dbWriteTable(
    con,
    "stage_agg",
    tabla,
    overwrite = TRUE
  )

  dbExecute(
    con,
"
INSERT INTO agg_beneficiario (
  chave_funcional,
  n_procedimientos,
  n_consultas,
  n_examenes,
  n_terapias,
  n_internaciones_registro,
  n_urgencias,
  n_otros,
  n_sin_tipo_servicio,
  n_registros_uci,
  n_registros_internado,
  n_registros_anestesia,
  costo_total_positivo,
  n_costos_positivos,
  n_costos_cero,
  n_costos_negativos,
  n_costos_faltantes,
  costo_maximo_positivo,
  n_unidad_faltante,
  n_especialidad_faltante,
  fecha_primera_utilizacion,
  fecha_ultima_utilizacion,
  tuvo_gastroenterologia,
  tuvo_cirugia,
  tuvo_imagenologia,
  tuvo_ecografia_abdominal,
  tuvo_procedimiento_biliar
)

SELECT
  chave_funcional,
  n_procedimientos,
  n_consultas,
  n_examenes,
  n_terapias,
  n_internaciones_registro,
  n_urgencias,
  n_otros,
  n_sin_tipo_servicio,
  n_registros_uci,
  n_registros_internado,
  n_registros_anestesia,
  costo_total_positivo,
  n_costos_positivos,
  n_costos_cero,
  n_costos_negativos,
  n_costos_faltantes,
  costo_maximo_positivo,
  n_unidad_faltante,
  n_especialidad_faltante,
  fecha_primera_utilizacion,
  fecha_ultima_utilizacion,
  tuvo_gastroenterologia,
  tuvo_cirugia,
  tuvo_imagenologia,
  tuvo_ecografia_abdominal,
  tuvo_procedimiento_biliar

FROM stage_agg

WHERE 1

ON CONFLICT(chave_funcional) DO UPDATE SET

  n_procedimientos =
    agg_beneficiario.n_procedimientos +
    excluded.n_procedimientos,

  n_consultas =
    agg_beneficiario.n_consultas +
    excluded.n_consultas,

  n_examenes =
    agg_beneficiario.n_examenes +
    excluded.n_examenes,

  n_terapias =
    agg_beneficiario.n_terapias +
    excluded.n_terapias,

  n_internaciones_registro =
    agg_beneficiario.n_internaciones_registro +
    excluded.n_internaciones_registro,

  n_urgencias =
    agg_beneficiario.n_urgencias +
    excluded.n_urgencias,

  n_otros =
    agg_beneficiario.n_otros +
    excluded.n_otros,

  n_sin_tipo_servicio =
    agg_beneficiario.n_sin_tipo_servicio +
    excluded.n_sin_tipo_servicio,

  n_registros_uci =
    agg_beneficiario.n_registros_uci +
    excluded.n_registros_uci,

  n_registros_internado =
    agg_beneficiario.n_registros_internado +
    excluded.n_registros_internado,

  n_registros_anestesia =
    agg_beneficiario.n_registros_anestesia +
    excluded.n_registros_anestesia,

  costo_total_positivo =
    agg_beneficiario.costo_total_positivo +
    excluded.costo_total_positivo,

  n_costos_positivos =
    agg_beneficiario.n_costos_positivos +
    excluded.n_costos_positivos,

  n_costos_cero =
    agg_beneficiario.n_costos_cero +
    excluded.n_costos_cero,

  n_costos_negativos =
    agg_beneficiario.n_costos_negativos +
    excluded.n_costos_negativos,

  n_costos_faltantes =
    agg_beneficiario.n_costos_faltantes +
    excluded.n_costos_faltantes,

  costo_maximo_positivo =
    CASE
      WHEN agg_beneficiario.costo_maximo_positivo IS NULL
        THEN excluded.costo_maximo_positivo

      WHEN excluded.costo_maximo_positivo IS NULL
        THEN agg_beneficiario.costo_maximo_positivo

      WHEN excluded.costo_maximo_positivo >
        agg_beneficiario.costo_maximo_positivo
        THEN excluded.costo_maximo_positivo

      ELSE agg_beneficiario.costo_maximo_positivo
    END,

  n_unidad_faltante =
    agg_beneficiario.n_unidad_faltante +
    excluded.n_unidad_faltante,

  n_especialidad_faltante =
    agg_beneficiario.n_especialidad_faltante +
    excluded.n_especialidad_faltante,

  fecha_primera_utilizacion =
    CASE
      WHEN agg_beneficiario.fecha_primera_utilizacion IS NULL
        THEN excluded.fecha_primera_utilizacion

      WHEN excluded.fecha_primera_utilizacion IS NULL
        THEN agg_beneficiario.fecha_primera_utilizacion

      WHEN excluded.fecha_primera_utilizacion <
        agg_beneficiario.fecha_primera_utilizacion
        THEN excluded.fecha_primera_utilizacion

      ELSE agg_beneficiario.fecha_primera_utilizacion
    END,

  fecha_ultima_utilizacion =
    CASE
      WHEN agg_beneficiario.fecha_ultima_utilizacion IS NULL
        THEN excluded.fecha_ultima_utilizacion

      WHEN excluded.fecha_ultima_utilizacion IS NULL
        THEN agg_beneficiario.fecha_ultima_utilizacion

      WHEN excluded.fecha_ultima_utilizacion >
        agg_beneficiario.fecha_ultima_utilizacion
        THEN excluded.fecha_ultima_utilizacion

      ELSE agg_beneficiario.fecha_ultima_utilizacion
    END,

  tuvo_gastroenterologia =
    MAX(
      agg_beneficiario.tuvo_gastroenterologia,
      excluded.tuvo_gastroenterologia
    ),

  tuvo_cirugia =
    MAX(
      agg_beneficiario.tuvo_cirugia,
      excluded.tuvo_cirugia
    ),

  tuvo_imagenologia =
    MAX(
      agg_beneficiario.tuvo_imagenologia,
      excluded.tuvo_imagenologia
    ),

  tuvo_ecografia_abdominal =
    MAX(
      agg_beneficiario.tuvo_ecografia_abdominal,
      excluded.tuvo_ecografia_abdominal
    ),

  tuvo_procedimiento_biliar =
    MAX(
      agg_beneficiario.tuvo_procedimiento_biliar,
      excluded.tuvo_procedimiento_biliar
    )
"
  )

  dbExecute(
    con,
    "DROP TABLE IF EXISTS stage_agg"
  )

  invisible(NULL)
}

actualizar_conteos <- function(
    tabla_destino,
    tabla,
    columnas_clave = c(
      "chave_funcional",
      "valor"
    )) {

  if (nrow(tabla) == 0L) {
    return(invisible(NULL))
  }

  dbWriteTable(
    con,
    "stage_conteos",
    tabla,
    overwrite = TRUE
  )

  sql <- sprintf(
"
INSERT INTO %s (
  %s,
  n
)

SELECT
  %s,
  n

FROM stage_conteos

WHERE 1

ON CONFLICT(%s) DO UPDATE SET
  n = %s.n + excluded.n
",
    tabla_destino,
    paste(columnas_clave, collapse = ", "),
    paste(columnas_clave, collapse = ", "),
    paste(columnas_clave, collapse = ", "),
    tabla_destino
  )

  dbExecute(
    con,
    sql
  )

  dbExecute(
    con,
    "DROP TABLE IF EXISTS stage_conteos"
  )

  invisible(NULL)
}

actualizar_especialidades <- function(tabla) {

  if (nrow(tabla) == 0L) {
    return(invisible(NULL))
  }

  dbWriteTable(
    con,
    "stage_especialidades",
    tabla,
    overwrite = TRUE
  )

  dbExecute(
    con,
"
INSERT INTO resumen_especialidades (
  target_k80,
  valor,
  n_registros
)

SELECT
  target_k80,
  valor,
  n_registros

FROM stage_especialidades

WHERE 1

ON CONFLICT(target_k80, valor) DO UPDATE SET
  n_registros =
    resumen_especialidades.n_registros +
    excluded.n_registros
"
  )

  dbExecute(
    con,
    "DROP TABLE IF EXISTS stage_especialidades"
  )

  invisible(NULL)
}

actualizar_procedimientos <- function(tabla) {

  if (nrow(tabla) == 0L) {
    return(invisible(NULL))
  }

  dbWriteTable(
    con,
    "stage_procedimientos",
    tabla,
    overwrite = TRUE
  )

  dbExecute(
    con,
"
INSERT INTO resumen_procedimientos (
  target_k80,
  codigo,
  descripcion,
  n_registros,
  costo_total_positivo
)

SELECT
  target_k80,
  codigo,
  descripcion,
  n_registros,
  costo_total_positivo

FROM stage_procedimientos

WHERE 1

ON CONFLICT(
  target_k80,
  codigo,
  descripcion
) DO UPDATE SET

  n_registros =
    resumen_procedimientos.n_registros +
    excluded.n_registros,

  costo_total_positivo =
    resumen_procedimientos.costo_total_positivo +
    excluded.costo_total_positivo
"
  )

  dbExecute(
    con,
    "DROP TABLE IF EXISTS stage_procedimientos"
  )

  invisible(NULL)
}


# 7. PROCESAMIENTO POR BLOQUES -------------------------------------------------

contador_bloques <- 0L
contador_filas <- 0

procesar_bloque <- function(x, pos) {

  asegurar_conexion()

  contador_bloques <<- contador_bloques + 1L
  contador_filas <<- contador_filas + nrow(x)

  dt <- as.data.table(x)

  setnames(
    dt,
    tolower(names(dt))
  )

  dt[, chave_funcional :=
    stri_trim_both(
      as.character(chave_funcional)
    )
  ]

  dt[
    is.na(chave_funcional) |
      chave_funcional %in% c(
        "",
        "NA",
        "N/A",
        "NULL",
        "-"
      ),
    chave_funcional := NA_character_
  ]

  dt <- dt[
    !is.na(chave_funcional)
  ]

  if (nrow(dt) == 0L) {
    return(invisible(NULL))
  }

  # Unir target y partición sin copiar toda la base.
  dt <- lookup[
    dt,
    on = "chave_funcional",
    nomatch = 0L
  ]

  if (nrow(dt) == 0L) {
    return(invisible(NULL))
  }

  # Variables normalizadas.
  dt[, sexo_norm :=
    normalizar_sexo(
      sexo_beneficiario
    )
  ]

  dt[, tipo_benef_norm :=
    normalizar_tipo_beneficiario(
      tipo_beneficiario
    )
  ]

  dt[, servicio_norm :=
    normalizar_servicio(
      cetipo
    )
  ]

  dt[, unidad_norm :=
    normalizar_unidad(
      tipo_unidade_prest_hospitalar
    )
  ]

  dt[, estado_norm :=
    normalizar_texto(
      uf_cnes_prest_hospitalar
    )
  ]

  dt[, especialidad_norm :=
    normalizar_texto(
      desc_especialidade
    )
  ]

  dt[, descripcion_norm :=
    normalizar_texto(
      descricao_procedimento
    )
  ]

  dt[, codigo_norm :=
    normalizar_texto(
      cd_procedimento
    )
  ]

  dt[, fecha_utilizacion :=
    normalizar_fecha(
      dt_utilizacao
    )
  ]

  dt[, fecha_nacimiento :=
    normalizar_fecha(
      dt_nascimento_beneficiario
    )
  ]

  dt[, uti_bin :=
    normalizar_binaria(
      uti
    )
  ]

  dt[, internado_bin :=
    normalizar_binaria(
      internado
    )
  ]

  dt[, porte_anestesico_num :=
    normalizar_numero(
      porte_anestesico
    )
  ]

  dt[, costo :=
    normalizar_numero(
      valor_utilizacao
    )
  ]

  dt[, costo_positivo :=
    fifelse(
      !is.na(costo) & costo > 0,
      costo,
      NA_real_
    )
  ]

  dt[, anestesia_bin :=
    as.integer(
      !is.na(porte_anestesico_num) &
        porte_anestesico_num > 0
    )
  ]

  # Indicadores clínicos directos.
  dt[, flag_gastro :=
    as.integer(
      !is.na(especialidad_norm) &
        stri_detect_regex(
          especialidad_norm,
          "GASTRO"
        )
    )
  ]

  dt[, flag_cirugia :=
    as.integer(
      (
        !is.na(especialidad_norm) &
          stri_detect_regex(
            especialidad_norm,
            "CIRURG|CIRUG"
          )
      ) |
        (
          !is.na(descripcion_norm) &
            stri_detect_regex(
              descripcion_norm,
              "CIRURG|CIRUG"
            )
        )
    )
  ]

  dt[, flag_imagen :=
    as.integer(
      (
        !is.na(especialidad_norm) &
          stri_detect_regex(
            especialidad_norm,
            "RADIOLOG|IMAGEM|DIAGNOSTICO POR IMAGEM"
          )
      ) |
        (
          !is.na(descripcion_norm) &
            stri_detect_regex(
              descripcion_norm,
              "ULTRASSON|ULTRASON|ECOGRAF|TOMOGRAF|RESSONAN|RESONAN"
            )
        )
    )
  ]

  dt[, flag_ecografia_abdominal :=
    as.integer(
      !is.na(descripcion_norm) &
        stri_detect_regex(
          descripcion_norm,
          "ULTRASSON|ULTRASON|ECOGRAF"
        ) &
        stri_detect_regex(
          descripcion_norm,
          "ABDOM"
        )
    )
  ]

  dt[, flag_biliar :=
    as.integer(
      !is.na(descripcion_norm) &
        stri_detect_regex(
          descripcion_norm,
          "COLECIST|VESICULA|BILIAR|COLANGI|CPRE|ERCP"
        )
    )
  ]

  # Agregación numérica del bloque.
  agg <- dt[
    ,
    .(
      n_procedimientos = .N,

      n_consultas =
        sum(
          servicio_norm == "CONSULTA",
          na.rm = TRUE
        ),

      n_examenes =
        sum(
          servicio_norm == "EXAMEN",
          na.rm = TRUE
        ),

      n_terapias =
        sum(
          servicio_norm == "TERAPIA",
          na.rm = TRUE
        ),

      n_internaciones_registro =
        sum(
          servicio_norm == "INTERNACION",
          na.rm = TRUE
        ),

      n_urgencias =
        sum(
          servicio_norm == "URGENCIAS",
          na.rm = TRUE
        ),

      n_otros =
        sum(
          servicio_norm == "OTROS",
          na.rm = TRUE
        ),

      n_sin_tipo_servicio =
        sum(
          servicio_norm == "SIN INFORMACION",
          na.rm = TRUE
        ),

      n_registros_uci =
        sum(
          uti_bin == 1L,
          na.rm = TRUE
        ),

      n_registros_internado =
        sum(
          internado_bin == 1L,
          na.rm = TRUE
        ),

      n_registros_anestesia =
        sum(
          anestesia_bin == 1L,
          na.rm = TRUE
        ),

      costo_total_positivo =
        sum_num_seguro(
          costo_positivo
        ),

      n_costos_positivos =
        sum(
          !is.na(costo) &
            costo > 0
        ),

      n_costos_cero =
        sum(
          !is.na(costo) &
            costo == 0
        ),

      n_costos_negativos =
        sum(
          !is.na(costo) &
            costo < 0
        ),

      n_costos_faltantes =
        sum(
          is.na(costo)
        ),

      costo_maximo_positivo =
        max_num_seguro(
          costo_positivo
        ),

      n_unidad_faltante =
        sum(
          is.na(unidad_norm)
        ),

      n_especialidad_faltante =
        sum(
          is.na(especialidad_norm)
        ),

      fecha_primera_utilizacion =
        min_fecha_segura(
          fecha_utilizacion
        ),

      fecha_ultima_utilizacion =
        max_fecha_segura(
          fecha_utilizacion
        ),

      tuvo_gastroenterologia =
        as.integer(
          any(
            flag_gastro == 1L,
            na.rm = TRUE
          )
        ),

      tuvo_cirugia =
        as.integer(
          any(
            flag_cirugia == 1L,
            na.rm = TRUE
          )
        ),

      tuvo_imagenologia =
        as.integer(
          any(
            flag_imagen == 1L,
            na.rm = TRUE
          )
        ),

      tuvo_ecografia_abdominal =
        as.integer(
          any(
            flag_ecografia_abdominal == 1L,
            na.rm = TRUE
          )
        ),

      tuvo_procedimiento_biliar =
        as.integer(
          any(
            flag_biliar == 1L,
            na.rm = TRUE
          )
        )
    ),
    by = chave_funcional
  ]

  conteo_sexo <- dt[
    !is.na(sexo_norm),
    .(n = .N),
    by = .(
      chave_funcional,
      valor = sexo_norm
    )
  ]

  conteo_tipo <- dt[
    !is.na(tipo_benef_norm),
    .(n = .N),
    by = .(
      chave_funcional,
      valor = tipo_benef_norm
    )
  ]

  conteo_estado <- dt[
    !is.na(estado_norm),
    .(n = .N),
    by = .(
      chave_funcional,
      valor = estado_norm
    )
  ]

  conteo_unidad <- dt[
    !is.na(unidad_norm),
    .(n = .N),
    by = .(
      chave_funcional,
      valor = unidad_norm
    )
  ]

  conteo_nacimiento <- dt[
    !is.na(fecha_nacimiento),
    .(n = .N),
    by = .(
      chave_funcional,
      valor = fecha_nacimiento
    )
  ]

  resumen_especialidades <- dt[
    !is.na(especialidad_norm),
    .(
      n_registros = .N
    ),
    by = .(
      target_k80,
      valor = especialidad_norm
    )
  ]

  resumen_procedimientos <- dt[
    !is.na(codigo_norm) |
      !is.na(descripcion_norm),
    .(
      n_registros = .N,
      costo_total_positivo =
        sum_num_seguro(
          costo_positivo
        )
    ),
    by = .(
      target_k80,
      codigo = fifelse(
        is.na(codigo_norm),
        "SIN CODIGO",
        codigo_norm
      ),
      descripcion = fifelse(
        is.na(descripcion_norm),
        "SIN DESCRIPCION",
        descripcion_norm
      )
    )
  ]

  # No se mantiene una transacción manual alrededor de dbWriteTable().
  # Cada actualización se completa antes de pasar a la siguiente. Si ocurre
  # un error, el script se detiene mostrando el bloque exacto y la base se
  # reconstruye desde cero en la siguiente ejecución.

  resultado_transaccion <- tryCatch(
    {

      actualizar_agg_beneficiario(
        agg
      )

      actualizar_conteos(
        "conteo_sexo",
        conteo_sexo
      )

      actualizar_conteos(
        "conteo_tipo_beneficiario",
        conteo_tipo
      )

      actualizar_conteos(
        "conteo_estado",
        conteo_estado
      )

      actualizar_conteos(
        "conteo_unidad",
        conteo_unidad
      )

      actualizar_conteos(
        "conteo_nacimiento",
        conteo_nacimiento
      )

      actualizar_especialidades(
        resumen_especialidades
      )

      actualizar_procedimientos(
        resumen_procedimientos
      )

      TRUE
    },
    error = function(e) {

      cat(
        "\n\nERROR EN EL BLOQUE ",
        contador_bloques,
        " (filas acumuladas: ",
        format(
          contador_filas,
          big.mark = ".",
          scientific = FALSE
        ),
        "):\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )

      cerrar_conexion_segura()
      stop(e)
    }
  )

  rm(
    dt,
    agg,
    conteo_sexo,
    conteo_tipo,
    conteo_estado,
    conteo_unidad,
    conteo_nacimiento,
    resumen_especialidades,
    resumen_procedimientos
  )

  # Cerrar al terminar cada bloque fuerza el guardado y evita conexiones
  # persistentes inválidas. El siguiente bloque abre una conexión nueva.
  cerrar_conexion_segura()

  invisible(gc())

  cat(
    sprintf(
      paste0(
        "\rBloques: %d | ",
        "Filas procesadas: %s"
      ),
      contador_bloques,
      format(
        contador_filas,
        big.mark = ".",
        scientific = FALSE
      )
    )
  )

  invisible(resultado_transaccion)
}


# 8. LECTURA DEL CSV -----------------------------------------------------------

cat("\n============================================================\n")
cat("INICIANDO CONSTRUCCIÓN DE VARIABLES POR BLOQUES\n")
cat("============================================================\n")
cat("Tamaño del bloque:", tamano_bloque, "\n")
cat("Base SQLite:", ruta_sqlite, "\n\n")

columnas_lectura <- cols_only(
  CHAVE_FUNCIONAL = col_character(),
  DT_UTILIZACAO = col_character(),
  DT_NASCIMENTO_BENEFICIARIO = col_character(),
  SEXO_BENEFICIARIO = col_character(),
  TIPO_BENEFICIARIO = col_character(),
  CETIPO = col_character(),
  UTI = col_character(),
  INTERNADO = col_character(),
  PORTE_ANESTESICO = col_character(),
  VALOR_UTILIZACAO = col_character(),
  DESC_ESPECIALIDADE = col_character(),
  TIPO_UNIDADE_PREST_HOSPITALAR = col_character(),
  UF_CNES_PREST_HOSPITALAR = col_character(),
  CD_PROCEDIMENTO = col_character(),
  DESCRICAO_PROCEDIMENTO = col_character()
)

callback <- SideEffectChunkCallback$new(
  procesar_bloque
)

read_csv_chunked(
  file = ruta_base,
  callback = callback,
  chunk_size = tamano_bloque,
  col_types = columnas_lectura,
  na = c(
    "",
    "NA",
    "N/A",
    "NULL",
    "-"
  ),
  progress = FALSE,
  show_col_types = FALSE
)

cat("\n\nLectura completa finalizada.\n")

asegurar_conexion()


# 9. MODAS E INCONSISTENCIAS ---------------------------------------------------

cat("\nConstruyendo categorías modales...\n")

crear_tabla_moda <- function(
    tabla_conteos,
    tabla_salida,
    nombre_variable) {

  dbExecute(
    con,
    sprintf(
"
DROP TABLE IF EXISTS %s
",
      tabla_salida
    )
  )

  dbExecute(
    con,
    sprintf(
"
CREATE TABLE %s AS

SELECT
  chave_funcional,
  valor AS %s,
  n_categorias

FROM (
  SELECT
    chave_funcional,
    valor,
    n,

    COUNT(*) OVER (
      PARTITION BY chave_funcional
    ) AS n_categorias,

    ROW_NUMBER() OVER (
      PARTITION BY chave_funcional
      ORDER BY
        n DESC,
        valor ASC
    ) AS orden

  FROM %s
)

WHERE orden = 1
",
      tabla_salida,
      nombre_variable,
      tabla_conteos
    )
  )

  dbExecute(
    con,
    sprintf(
"
CREATE UNIQUE INDEX IF NOT EXISTS
idx_%s_id
ON %s(chave_funcional)
",
      tabla_salida,
      tabla_salida
    )
  )
}

crear_tabla_moda(
  "conteo_sexo",
  "moda_sexo",
  "sexo"
)

crear_tabla_moda(
  "conteo_tipo_beneficiario",
  "moda_tipo_beneficiario",
  "tipo_beneficiario"
)

crear_tabla_moda(
  "conteo_estado",
  "moda_estado",
  "estado_predominante"
)

crear_tabla_moda(
  "conteo_unidad",
  "moda_unidad",
  "unidad_predominante"
)

crear_tabla_moda(
  "conteo_nacimiento",
  "moda_nacimiento",
  "fecha_nacimiento"
)


# 10. TABLA FINAL POR BENEFICIARIO --------------------------------------------

cat("Construyendo tabla final por beneficiario...\n")

dbExecute(
  con,
  "DROP TABLE IF EXISTS variables_beneficiario_k80"
)

dbExecute(
  con,
"
CREATE TABLE variables_beneficiario_k80 AS

SELECT
  tp.chave_funcional,
  tp.target_k80,
  tp.tiene_cid_registrado,
  tp.n_registros_con_cid,
  tp.n_registros_k80,
  tp.particion,

  COALESCE(ms.sexo, 'SIN INFORMACION')
    AS sexo,

  COALESCE(
    mt.tipo_beneficiario,
    'SIN INFORMACION'
  ) AS tipo_beneficiario,

  COALESCE(
    me.estado_predominante,
    'SIN INFORMACION'
  ) AS estado_predominante,

  COALESCE(
    mu.unidad_predominante,
    'SIN INFORMACION'
  ) AS unidad_predominante,

  mn.fecha_nacimiento,

  CASE
    WHEN mn.fecha_nacimiento IS NULL
      OR ab.fecha_ultima_utilizacion IS NULL
    THEN NULL

    WHEN (
      JULIANDAY(ab.fecha_ultima_utilizacion) -
      JULIANDAY(mn.fecha_nacimiento)
    ) / 365.25 BETWEEN 0 AND 110

    THEN CAST(
      (
        JULIANDAY(ab.fecha_ultima_utilizacion) -
        JULIANDAY(mn.fecha_nacimiento)
      ) / 365.25
      AS INTEGER
    )

    ELSE NULL
  END AS edad,

  CASE
    WHEN COALESCE(ms.n_categorias, 0) > 1
    THEN 1 ELSE 0
  END AS sexo_inconsistente,

  CASE
    WHEN COALESCE(mn.n_categorias, 0) > 1
    THEN 1 ELSE 0
  END AS nacimiento_inconsistente,

  CASE
    WHEN COALESCE(mt.n_categorias, 0) > 1
    THEN 1 ELSE 0
  END AS tipo_beneficiario_inconsistente,

  COALESCE(
    ab.n_procedimientos,
    0
  ) AS n_procedimientos,

  COALESCE(
    ab.n_consultas,
    0
  ) AS n_consultas,

  COALESCE(
    ab.n_examenes,
    0
  ) AS n_examenes,

  COALESCE(
    ab.n_terapias,
    0
  ) AS n_terapias,

  COALESCE(
    ab.n_internaciones_registro,
    0
  ) AS n_internaciones_registro,

  COALESCE(
    ab.n_urgencias,
    0
  ) AS n_urgencias,

  COALESCE(
    ab.n_otros,
    0
  ) AS n_otros,

  COALESCE(
    ab.n_sin_tipo_servicio,
    0
  ) AS n_sin_tipo_servicio,

  COALESCE(
    ab.n_registros_uci,
    0
  ) AS n_registros_uci,

  COALESCE(
    ab.n_registros_internado,
    0
  ) AS n_registros_internado,

  COALESCE(
    ab.n_registros_anestesia,
    0
  ) AS n_registros_anestesia,

  COALESCE(
    ab.costo_total_positivo,
    0
  ) AS costo_total_positivo,

  COALESCE(
    ab.n_costos_positivos,
    0
  ) AS n_costos_positivos,

  COALESCE(
    ab.n_costos_cero,
    0
  ) AS n_costos_cero,

  COALESCE(
    ab.n_costos_negativos,
    0
  ) AS n_costos_negativos,

  COALESCE(
    ab.n_costos_faltantes,
    0
  ) AS n_costos_faltantes,

  ab.costo_maximo_positivo,

  COALESCE(
    ab.n_unidad_faltante,
    0
  ) AS n_unidad_faltante,

  COALESCE(
    ab.n_especialidad_faltante,
    0
  ) AS n_especialidad_faltante,

  ab.fecha_primera_utilizacion,
  ab.fecha_ultima_utilizacion,

  CASE
    WHEN ab.fecha_primera_utilizacion IS NULL
      OR ab.fecha_ultima_utilizacion IS NULL
    THEN NULL

    ELSE CAST(
      JULIANDAY(ab.fecha_ultima_utilizacion) -
      JULIANDAY(ab.fecha_primera_utilizacion)
      AS INTEGER
    )
  END AS dias_observacion,

  COALESCE(
    ab.tuvo_gastroenterologia,
    0
  ) AS tuvo_gastroenterologia,

  COALESCE(
    ab.tuvo_cirugia,
    0
  ) AS tuvo_cirugia,

  COALESCE(
    ab.tuvo_imagenologia,
    0
  ) AS tuvo_imagenologia,

  COALESCE(
    ab.tuvo_ecografia_abdominal,
    0
  ) AS tuvo_ecografia_abdominal,

  COALESCE(
    ab.tuvo_procedimiento_biliar,
    0
  ) AS tuvo_procedimiento_biliar,

  CASE
    WHEN COALESCE(ab.n_procedimientos, 0) > 0
    THEN
      1.0 * ab.n_consultas /
      ab.n_procedimientos
    ELSE 0
  END AS prop_consultas,

  CASE
    WHEN COALESCE(ab.n_procedimientos, 0) > 0
    THEN
      1.0 * ab.n_examenes /
      ab.n_procedimientos
    ELSE 0
  END AS prop_examenes,

  CASE
    WHEN COALESCE(ab.n_procedimientos, 0) > 0
    THEN
      1.0 * ab.n_terapias /
      ab.n_procedimientos
    ELSE 0
  END AS prop_terapias,

  CASE
    WHEN COALESCE(ab.n_procedimientos, 0) > 0
    THEN
      1.0 * ab.n_internaciones_registro /
      ab.n_procedimientos
    ELSE 0
  END AS prop_internaciones,

  CASE
    WHEN COALESCE(ab.n_procedimientos, 0) > 0
    THEN
      1.0 * ab.n_urgencias /
      ab.n_procedimientos
    ELSE 0
  END AS prop_urgencias

FROM target_particiones tp

LEFT JOIN agg_beneficiario ab
  ON tp.chave_funcional =
    ab.chave_funcional

LEFT JOIN moda_sexo ms
  ON tp.chave_funcional =
    ms.chave_funcional

LEFT JOIN moda_tipo_beneficiario mt
  ON tp.chave_funcional =
    mt.chave_funcional

LEFT JOIN moda_estado me
  ON tp.chave_funcional =
    me.chave_funcional

LEFT JOIN moda_unidad mu
  ON tp.chave_funcional =
    mu.chave_funcional

LEFT JOIN moda_nacimiento mn
  ON tp.chave_funcional =
    mn.chave_funcional
"
)

dbExecute(
  con,
"
CREATE UNIQUE INDEX IF NOT EXISTS
idx_variables_id
ON variables_beneficiario_k80(chave_funcional)
"
)

dbExecute(
  con,
"
CREATE INDEX IF NOT EXISTS
idx_variables_target
ON variables_beneficiario_k80(target_k80)
"
)

dbExecute(
  con,
"
CREATE INDEX IF NOT EXISTS
idx_variables_particion
ON variables_beneficiario_k80(particion)
"
)


# 11. VALIDACIONES -------------------------------------------------------------

cat("Validando tabla final...\n")

validacion_final <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  COUNT(*) AS n_beneficiarios,
  COUNT(DISTINCT chave_funcional)
    AS n_ids_unicos,
  SUM(target_k80)
    AS n_k80,
  SUM(tiene_cid_registrado)
    AS n_con_cid,
  SUM(n_procedimientos)
    AS n_registros_reconstruidos

FROM variables_beneficiario_k80
"
  )
)

if (
  validacion_final$n_beneficiarios[[1]] !=
    nrow(lookup)
) {
  stop(
    "La tabla final no contiene todos los beneficiarios."
  )
}

if (
  validacion_final$n_ids_unicos[[1]] !=
    nrow(lookup)
) {
  stop(
    "La tabla final contiene identificadores duplicados."
  )
}

if (
  validacion_final$n_k80[[1]] != 306L
) {
  stop(
    "La tabla final no conserva los 306 positivos K80."
  )
}

if (
  validacion_final$n_registros_reconstruidos[[1]] !=
    contador_filas
) {
  warning(
    "La suma de registros por beneficiario no coincide ",
    "con las filas leídas. Revise identificadores faltantes."
  )
}


# 12. TABLAS DESCRIPTIVAS ------------------------------------------------------

cat("Generando tablas descriptivas...\n")

resumen_general <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  COUNT(*) AS n_beneficiarios,
  SUM(target_k80) AS n_k80,
  SUM(tiene_cid_registrado) AS n_con_cid,
  SUM(n_procedimientos) AS n_procedimientos,
  SUM(n_consultas) AS n_consultas,
  SUM(n_examenes) AS n_examenes,
  SUM(n_terapias) AS n_terapias,
  SUM(n_internaciones_registro) AS n_internaciones,
  SUM(n_urgencias) AS n_urgencias,
  SUM(n_costos_negativos) AS n_costos_negativos,
  SUM(n_costos_cero) AS n_costos_cero,
  SUM(n_costos_faltantes) AS n_costos_faltantes,
  SUM(costo_total_positivo) AS costo_total_positivo

FROM variables_beneficiario_k80
"
  )
)

resumen_por_target <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  COUNT(*) AS n_beneficiarios,
  AVG(edad) AS edad_media,
  AVG(n_procedimientos) AS procedimientos_promedio,
  AVG(n_consultas) AS consultas_promedio,
  AVG(n_examenes) AS examenes_promedio,
  AVG(n_terapias) AS terapias_promedio,
  AVG(n_internaciones_registro)
    AS internaciones_promedio,
  AVG(n_urgencias) AS urgencias_promedio,
  AVG(costo_total_positivo)
    AS costo_total_promedio,
  AVG(n_registros_uci > 0) * 100
    AS porcentaje_con_uci,
  AVG(n_registros_internado > 0) * 100
    AS porcentaje_internado,
  AVG(tuvo_cirugia) * 100
    AS porcentaje_cirugia,
  AVG(tuvo_imagenologia) * 100
    AS porcentaje_imagenologia,
  AVG(tuvo_ecografia_abdominal) * 100
    AS porcentaje_ecografia_abdominal,
  AVG(tuvo_procedimiento_biliar) * 100
    AS porcentaje_procedimiento_biliar

FROM variables_beneficiario_k80

GROUP BY target_k80

ORDER BY target_k80
"
  )
)

distribucion_sexo <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  sexo,
  COUNT(*) AS n

FROM variables_beneficiario_k80

GROUP BY
  target_k80,
  sexo

ORDER BY
  target_k80,
  n DESC
"
  )
)

distribucion_tipo_beneficiario <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  tipo_beneficiario,
  COUNT(*) AS n

FROM variables_beneficiario_k80

GROUP BY
  target_k80,
  tipo_beneficiario

ORDER BY
  target_k80,
  n DESC
"
  )
)

distribucion_estado <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  estado_predominante,
  COUNT(*) AS n

FROM variables_beneficiario_k80

GROUP BY
  target_k80,
  estado_predominante

ORDER BY
  target_k80,
  n DESC
"
  )
)

distribucion_unidad <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  unidad_predominante,
  COUNT(*) AS n

FROM variables_beneficiario_k80

GROUP BY
  target_k80,
  unidad_predominante

ORDER BY
  target_k80,
  n DESC
"
  )
)

top_especialidades <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  valor AS especialidad,
  n_registros

FROM (
  SELECT
    target_k80,
    valor,
    n_registros,

    ROW_NUMBER() OVER (
      PARTITION BY target_k80
      ORDER BY n_registros DESC
    ) AS orden

  FROM resumen_especialidades
)

WHERE orden <= 25

ORDER BY
  target_k80,
  orden
"
  )
)

top_procedimientos <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  codigo,
  descripcion,
  n_registros,
  costo_total_positivo

FROM (
  SELECT
    target_k80,
    codigo,
    descripcion,
    n_registros,
    costo_total_positivo,

    ROW_NUMBER() OVER (
      PARTITION BY target_k80
      ORDER BY n_registros DESC
    ) AS orden

  FROM resumen_procedimientos
)

WHERE orden <= 30

ORDER BY
  target_k80,
  orden
"
  )
)

faltantes_beneficiario <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  SUM(edad IS NULL) AS edad,
  SUM(sexo = 'SIN INFORMACION') AS sexo,
  SUM(tipo_beneficiario = 'SIN INFORMACION')
    AS tipo_beneficiario,
  SUM(estado_predominante = 'SIN INFORMACION')
    AS estado_predominante,
  SUM(unidad_predominante = 'SIN INFORMACION')
    AS unidad_predominante,
  SUM(costo_maximo_positivo IS NULL)
    AS costo_maximo_positivo,
  SUM(fecha_primera_utilizacion IS NULL)
    AS fecha_primera_utilizacion,
  SUM(fecha_ultima_utilizacion IS NULL)
    AS fecha_ultima_utilizacion

FROM variables_beneficiario_k80
"
  )
)

faltantes_beneficiario <- melt(
  faltantes_beneficiario,
  measure.vars = names(
    faltantes_beneficiario
  ),
  variable.name = "variable",
  value.name = "n_faltantes"
)

faltantes_beneficiario[
  ,
  porcentaje :=
    100 * n_faltantes /
    validacion_final$n_beneficiarios[[1]]
]

inconsistencias <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  SUM(sexo_inconsistente)
    AS sexo_inconsistente,

  SUM(nacimiento_inconsistente)
    AS nacimiento_inconsistente,

  SUM(tipo_beneficiario_inconsistente)
    AS tipo_beneficiario_inconsistente

FROM variables_beneficiario_k80
"
  )
)

positivos_k80 <- as.data.table(
  dbGetQuery(
    con,
"
SELECT *
FROM variables_beneficiario_k80
WHERE target_k80 = 1
ORDER BY chave_funcional
"
  )
)


# 13. CUANTILES DE EDAD Y COSTO -----------------------------------------------

datos_cuantiles <- as.data.table(
  dbGetQuery(
    con,
"
SELECT
  target_k80,
  edad,
  costo_total_positivo,
  n_procedimientos

FROM variables_beneficiario_k80
"
  )
)

mediana_numerica <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  as.numeric(median(x))
}

cuantil_numerico <- function(x, probabilidad) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  as.numeric(
    quantile(
      x,
      probs = probabilidad,
      names = FALSE,
      type = 7
    )
  )
}

cuantiles_por_target <- datos_cuantiles[
  ,
  .(
    edad_mediana =
      mediana_numerica(edad),

    edad_q1 =
      cuantil_numerico(edad, 0.25),

    edad_q3 =
      cuantil_numerico(edad, 0.75),

    costo_mediano =
      mediana_numerica(costo_total_positivo),

    costo_q1 =
      cuantil_numerico(
        costo_total_positivo,
        0.25
      ),

    costo_q3 =
      cuantil_numerico(
        costo_total_positivo,
        0.75
      ),

    procedimientos_mediana =
      mediana_numerica(n_procedimientos)
  ),
  by = target_k80
]


# 14. FIGURAS -----------------------------------------------------------------

grafico_edad <- ggplot(
  datos_cuantiles[
    !is.na(edad) &
      edad >= 0 &
      edad <= 110
  ],
  aes(
    x = edad,
    fill = factor(target_k80)
  )
) +
  geom_density(
    alpha = 0.45,
    adjust = 1.1
  ) +
  labs(
    title = "Distribución de edad según presencia de K80",
    x = "Edad",
    y = "Densidad",
    fill = "K80"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here(
    "figuras",
    "04_edad_por_target_k80.png"
  ),
  grafico_edad,
  width = 8,
  height = 5,
  dpi = 300
)

grafico_costos <- ggplot(
  datos_cuantiles[
    costo_total_positivo > 0
  ],
  aes(
    x = factor(
      target_k80,
      levels = c(0, 1),
      labels = c("Sin K80", "Con K80")
    ),
    y = costo_total_positivo,
    fill = factor(target_k80)
  )
) +
  geom_boxplot(
    outlier.alpha = 0.08,
    show.legend = FALSE
  ) +
  scale_y_log10() +
  labs(
    title = "Costo acumulado por beneficiario",
    subtitle = "Escala logarítmica",
    x = NULL,
    y = "Costo total positivo"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here(
    "figuras",
    "05_costos_por_target_k80.png"
  ),
  grafico_costos,
  width = 8,
  height = 5,
  dpi = 300
)

grafico_procedimientos <- ggplot(
  datos_cuantiles[
    n_procedimientos > 0
  ],
  aes(
    x = factor(
      target_k80,
      levels = c(0, 1),
      labels = c("Sin K80", "Con K80")
    ),
    y = n_procedimientos,
    fill = factor(target_k80)
  )
) +
  geom_boxplot(
    outlier.alpha = 0.08,
    show.legend = FALSE
  ) +
  scale_y_log10() +
  labs(
    title = "Procedimientos acumulados por beneficiario",
    subtitle = "Escala logarítmica",
    x = NULL,
    y = "Número de procedimientos"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here(
    "figuras",
    "06_procedimientos_por_target_k80.png"
  ),
  grafico_procedimientos,
  width = 8,
  height = 5,
  dpi = 300
)


# 15. EXPORTACIÓN --------------------------------------------------------------

fwrite(
  validacion_final,
  here(
    "resultados",
    "10_validacion_variables_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_general,
  here(
    "resultados",
    "11_resumen_general_variables_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_por_target,
  here(
    "resultados",
    "12_resumen_por_target_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  cuantiles_por_target,
  here(
    "resultados",
    "13_cuantiles_por_target_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  distribucion_sexo,
  here(
    "resultados",
    "14_distribucion_sexo_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  distribucion_tipo_beneficiario,
  here(
    "resultados",
    "15_distribucion_tipo_beneficiario_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  distribucion_estado,
  here(
    "resultados",
    "16_distribucion_estado_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  distribucion_unidad,
  here(
    "resultados",
    "17_distribucion_unidad_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  top_especialidades,
  here(
    "resultados",
    "18_top_especialidades_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  top_procedimientos,
  here(
    "resultados",
    "19_top_procedimientos_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  faltantes_beneficiario,
  here(
    "resultados",
    "20_faltantes_variables_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  inconsistencias,
  here(
    "resultados",
    "21_inconsistencias_variables_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  positivos_k80,
  here(
    "resultados",
    "22_beneficiarios_positivos_k80.csv"
  ),
  bom = TRUE
)

resultados_etapa_2 <- list(
  enfermedad = list(
    nombre = "Cholelithiasis (Colelitiasis)",
    codigo = "K80"
  ),
  validacion_final = validacion_final,
  resumen_general = resumen_general,
  resumen_por_target = resumen_por_target,
  cuantiles_por_target = cuantiles_por_target,
  distribucion_sexo = distribucion_sexo,
  distribucion_tipo_beneficiario =
    distribucion_tipo_beneficiario,
  distribucion_estado = distribucion_estado,
  distribucion_unidad = distribucion_unidad,
  top_especialidades = top_especialidades,
  top_procedimientos = top_procedimientos,
  faltantes_beneficiario =
    faltantes_beneficiario,
  inconsistencias = inconsistencias,
  archivo_sqlite =
    "data/processed/taller3_k80.sqlite",
  informacion_sesion =
    sessionInfo()
)

saveRDS(
  resultados_etapa_2,
  here(
    "resultados",
    "resultados_etapa_2.rds"
  ),
  compress = "xz"
)

rm(
  datos_cuantiles,
  positivos_k80
)

invisible(gc(full = TRUE))


# 16. CIERRE ------------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 2 FINALIZADA CORRECTAMENTE\n")
cat("============================================================\n")
cat(
  "Filas procesadas:",
  format(
    contador_filas,
    big.mark = ".",
    scientific = FALSE
  ),
  "\n"
)
cat(
  "Beneficiarios:",
  format(
    validacion_final$n_beneficiarios[[1]],
    big.mark = ".",
    scientific = FALSE
  ),
  "\n"
)
cat(
  "Positivos K80:",
  validacion_final$n_k80[[1]],
  "\n"
)
cat(
  "Base analítica SQLite:",
  "data/processed/taller3_k80.sqlite\n"
)
cat(
  "Resumen:",
  "resultados/resultados_etapa_2.rds\n"
)
cat("============================================================\n")

cerrar_conexion_segura()
