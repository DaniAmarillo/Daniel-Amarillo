

RUTA <- "C:/Users/juanj/OneDrive/Desktop/base de datos/db_2026.csv"
OUT  <- "salidas_taller3"
REF  <- as.Date("2026-06-30")
PNEU_REGEX <- "^J1[2-8]" 

dir.create(OUT, showWarnings = FALSE)
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)
setDTthreads(percent = 100)


moda <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

max0 <- function(x) { x <- x[is.finite(x)]; if (!length(x)) 0 else as.numeric(max(x)) }


group_uf <- function(x) {
  u <- toupper(trimws(x)); u[is.na(u)] <- "NI"
  u[u %in% c("-", "")] <- "NI"
  keep <- c("SP","PE","MT","RJ","PR","SC","BA","MG")
  u[!(u %in% c(keep, "NI"))] <- "Otros"
  u
}
group_tb <- function(x) {
  u <- toupper(trimws(x)); out <- rep("Otros", length(u))
  out[u == "TITULAR"]    <- "TITULAR"
  out[u == "DEPENDENTE"] <- "DEPENDENTE"
  out[grepl("INFORMAD|IGNORAD", u)] <- "NI"
  out[is.na(u)] <- "NI"
  out
}
group_un <- function(x) {
  u <- toupper(trimws(x)); out <- rep("Otros", length(u))
  out[is.na(u) | u == "-" | u == ""]        <- "NI"
  out[grepl("SADT", u)]                     <- "SADT"
  out[grepl("HOSPITAL GERAL", u)]           <- "HOSP_GERAL"
  out[grepl("CENTRO DE ESPECIALIDADE|CLINICA/CENTRO", u)] <- "CLINICA_ESP"
  out[grepl("CONSULTORIO", u)]              <- "CONSULTORIO"
  out[grepl("HOSPITAL ESPECIALIZADO", u)]   <- "HOSP_ESP"
  out[grepl("PRONTO", u)]                   <- "PRONTO"
  out
}


if (!exists("dt") || !is.data.table(dt)) {
  cat("Leyendo CSV...\n"); t0 <- Sys.time()
  dt <- fread(RUTA, na.strings = c("", "NA", "NULL", "null", "NaN"),
              showProgress = TRUE)
  cat("Tiempo de carga: "); print(Sys.time() - t0)
} else {
  cat("Reusando 'dt' de la memoria (", nrow(dt), "filas ).\n")
}


cat("Construyendo variables a nivel fila...\n")

dt[, CID_norm  := toupper(trimws(as.character(CID)))]
dt[, pneu_flag := as.integer(grepl(PNEU_REGEX, CID_norm))]  # grepl(NA)=FALSE

dt[, sexo_clean := fifelse(SEXO_BENEFICIARIO %in% c("M", "MASCULINO"), "M",
                    fifelse(SEXO_BENEFICIARIO == "F", "F", "NI"))]

dt[, dob := as.Date(DT_NASCIMENTO_BENEFICIARIO)]
dt[, idade_row := as.numeric(REF - dob) / 365.25]
dt[idade_row < 0 | idade_row > 110, idade_row := NA_real_]

dt[, CET_U := toupper(CETIPO)]
dt[, `:=`(
  cet_exame    = CET_U == "EXAME",
  cet_intern   = grepl("^INTERNA", CET_U),
  cet_consulta = CET_U == "CONSULTA",
  cet_pronto   = grepl("PRONTO", CET_U),
  cet_terapia  = CET_U == "TERAPIA",
  cet_outros   = CET_U == "OUTROS"
)]

dt[, resp_flag := as.integer(grepl(
  "PNEUMO|PULMON|TISIOLOG|RESPIRAT|INFECTOLOG|TORAX|TÓRAX",
  toupper(DESC_ESPECIALIDADE)))]

cat("Agregando a nivel beneficiario (puede tardar)...\n"); t1 <- Sys.time()
benef <- dt[, .(
  # --- objetivo ---
  y = max(pneu_flag),
  # --- demográficas ---
  sexo           = moda(sexo_clean),
  idade          = as.numeric(median(idade_row, na.rm = TRUE)),
  tipo_benef_raw = moda(TIPO_BENEFICIARIO),
  uf_raw         = moda(UF_CNES_PREST_HOSPITALAR),
  unidade_raw    = moda(TIPO_UNIDADE_PREST_HOSPITALAR),
  # --- intensidad de uso ---
  n_filas          = .N,
  n_util           = uniqueN(DT_UTILIZACAO),
  n_proc_distintos = uniqueN(CD_PROCEDIMENTO),
  n_especialidades = uniqueN(DESC_ESPECIALIDADE),
  # --- tipos de utilización ---
  n_exame      = sum(cet_exame),
  n_intern_cet = sum(cet_intern),
  n_consulta   = sum(cet_consulta),
  n_pronto     = sum(cet_pronto),
  n_terapia    = sum(cet_terapia),
  n_outros     = sum(cet_outros),
  # --- clínicas ---
  any_uti       = max(UTI),
  n_uti         = sum(UTI),
  any_internado = max(INTERNADO),
  n_internado   = sum(INTERNADO),
  flag_esp_resp = max(resp_flag),
  porte_max     = max0(PORTE_ANESTESICO),
  # --- costos ---
  valor_total = sum(VALOR_UTILIZACAO),
  valor_medio = mean(VALOR_UTILIZACAO),
  valor_max   = max(VALOR_UTILIZACAO),
  valor_sd    = sd(VALOR_UTILIZACAO)
), by = CHAVE_FUNCIONAL]
cat("Tiempo de agregación: "); print(Sys.time() - t1)

benef[, sexo       := factor(fifelse(is.na(sexo), "NI", sexo))]
benef[, uf         := factor(group_uf(uf_raw))]
benef[, tipo_benef := factor(group_tb(tipo_benef_raw))]
benef[, unidade    := factor(group_un(unidade_raw))]
benef[, c("uf_raw", "tipo_benef_raw", "unidade_raw") := NULL]

benef[is.na(valor_sd), valor_sd := 0]

med_idade <- median(benef$idade, na.rm = TRUE)
benef[is.na(idade) | !is.finite(idade), idade := med_idade]
benef[, idade := round(idade, 1)]

cat("\n============================================================\n")
cat("==== TABLA A NIVEL BENEFICIARIO ====\n")
cat("Dimensiones: "); print(dim(benef))

cat("\n==== VARIABLE OBJETIVO ====\n")
print(table(benef$y))
cat("Prevalencia:", round(100 * mean(benef$y), 3), "%\n")

cat("\n==== ¿QUEDAN NA EN ALGUNA COLUMNA? ====\n")
print(colSums(is.na(benef)))

cat("\n==== CATEGÓRICAS AGRUPADAS ====\n")
cat("-- sexo --\n");       print(table(benef$sexo))
cat("-- tipo_benef --\n"); print(table(benef$tipo_benef))
cat("-- uf --\n");         print(table(benef$uf))
cat("-- unidade --\n");    print(table(benef$unidade))

cat("\n==== RESUMEN DE VARIABLES NUMÉRICAS ====\n")
num_cols <- c("idade","n_filas","n_util","n_proc_distintos","n_especialidades",
              "n_internado","n_uti","porte_max","valor_total","valor_medio","valor_max")
print(summary(benef[, ..num_cols]))

# Evidencia de leakage: comparar perfiles y=1 vs y=0
cat("\n==== PROMEDIOS POR CLASE (evidencia de leakage) ====\n")
comp_cols <- c("idade","n_filas","n_util","any_internado","any_uti",
               "flag_esp_resp","porte_max","valor_total","valor_medio")
print(benef[, lapply(.SD, function(z) round(mean(z), 2)), by = y, .SDcols = comp_cols])

cat("\n==== PRIMERAS FILAS ====\n")
print(head(benef, 5))

saveRDS(benef, file.path(OUT, "datos_beneficiario.rds"))

