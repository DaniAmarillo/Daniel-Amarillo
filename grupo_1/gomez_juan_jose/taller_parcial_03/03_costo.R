
RUTA <- "C:/Users/juanj/OneDrive/Desktop/base de datos/db_2026.csv"
OUT  <- "C:/Users/juanj/OneDrive/Desktop/salidas_taller3"
REF  <- as.Date("2026-06-30")
PNEU_REGEX <- "^J1[2-8]"

if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table); setDTthreads(percent = 100)

if (!exists("dt") || !is.data.table(dt)) {
  cat("Leyendo CSV...\n")
  dt <- fread(RUTA, na.strings = c("", "NA", "NULL", "null", "NaN"),
              showProgress = TRUE)
} else cat("Reusando 'dt' de memoria.\n")

dt[, CID_norm  := toupper(trimws(as.character(CID)))]
dt[, pneu_flag := as.integer(grepl(PNEU_REGEX, CID_norm))]
dt[, dob := as.Date(DT_NASCIMENTO_BENEFICIARIO)]
dt[, idade_row := as.numeric(REF - dob) / 365.25]
dt[idade_row < 0 | idade_row > 110, idade_row := NA_real_]

n <- nrow(dt)
desc <- list()

desc$granularidad <- data.table(
  metrica = c("Beneficiarios","Utilizaciones (CHAVE+fecha)","Filas/procedimientos","Procedimientos distintos"),
  valor = c(uniqueN(dt$CHAVE_FUNCIONAL),
            nrow(unique(dt[, .(CHAVE_FUNCIONAL, DT_UTILIZACAO)])),
            n, uniqueN(dt$CD_PROCEDIMENTO)))

desc$rango_fechas <- range(as.Date(dt$DT_UTILIZACAO), na.rm = TRUE)

dt[, cid_ausente := is.na(CID) | CID_norm %in% c("N/A", "-", "", ".")]
desc$cid_ausente_pct <- round(100 * mean(dt$cid_ausente), 2)

desc$top_cid <- dt[cid_ausente == FALSE, .N, by = CID_norm][order(-N)][1:20]
desc$pneu_codigos <- dt[pneu_flag == 1, .N, by = CID_norm][order(-N)]
desc$filas_pneu <- dt[pneu_flag == 1, .N]

desc$faltantes <- data.table(
  columna = names(dt),
  pct_NA = round(100 * sapply(dt, function(x) sum(is.na(x))) / n, 2)
)[order(-pct_NA)][pct_NA > 0]

desc$cetipo <- dt[, .N, by = CETIPO][order(-N)]

val <- dt$VALOR_UTILIZACAO
desc$valor_transaccion <- data.table(
  estad = c("min","p25","mediana","media","p75","p99","max","negativos","ceros"),
  valor = c(min(val), quantile(val,.25), median(val), mean(val),
            quantile(val,.75), quantile(val,.99), max(val),
            sum(val < 0), sum(val == 0)))

cat("\n==== DESCRIPTIVO ====\n")
cat("CID ausente:", desc$cid_ausente_pct, "%\n")
cat("Rango fechas:", as.character(desc$rango_fechas), "\n")
print(desc$granularidad)

pneu_benef <- unique(dt[pneu_flag == 1, CHAVE_FUNCIONAL])
cat("\nBeneficiarios con neumonía:", length(pneu_benef), "\n")

costo <- list()

costo$directo <- dt[pneu_flag == 1, .(
  n = .N, media = mean(VALOR_UTILIZACAO), mediana = median(VALOR_UTILIZACAO),
  p25 = quantile(VALOR_UTILIZACAO,.25), p75 = quantile(VALOR_UTILIZACAO,.75))]

sub <- dt[CHAVE_FUNCIONAL %in% pneu_benef]
cx_proc  <- sub$VALOR_UTILIZACAO                                              # por procedimiento (fila)
cx_util  <- sub[, .(c = sum(VALOR_UTILIZACAO)), by = .(CHAVE_FUNCIONAL, DT_UTILIZACAO)]$c  # por utilización
cx_benef <- sub[, .(c = sum(VALOR_UTILIZACAO)), by = CHAVE_FUNCIONAL]$c       # por beneficiario
resumir <- function(x) c(n = length(x), media = mean(x), mediana = median(x),
                         p25 = quantile(x,.25), p75 = quantile(x,.75),
                         p90 = quantile(x,.90), max = max(x))
costo$granularidad <- rbind(
  procedimiento = resumir(cx_proc),
  utilizacion   = resumir(cx_util),
  beneficiario  = resumir(cx_benef))
costo$granularidad <- as.data.table(round(costo$granularidad, 0), keep.rownames = "nivel")

bc <- sub[, .(costo = sum(VALOR_UTILIZACAO),
              any_uti = max(UTI), any_internado = max(INTERNADO),
              idade = median(idade_row, na.rm = TRUE)), by = CHAVE_FUNCIONAL]
bc[, edad_grp := cut(idade, c(0,40,60,75,200), labels = c("<40","40-60","60-75","75+"))]

costo$por_uti <- bc[, .(n = .N, media = round(mean(costo)),
                        mediana = round(median(costo))), by = any_uti][order(any_uti)]
costo$por_internado <- bc[, .(n = .N, media = round(mean(costo)),
                              mediana = round(median(costo))), by = any_internado][order(any_internado)]
costo$por_edad <- bc[, .(n = .N, media = round(mean(costo)),
                         mediana = round(median(costo))), by = edad_grp][order(edad_grp)]

bc[, costo_pos := pmax(costo, 1)]
mcost <- lm(log(costo_pos) ~ edad_grp + factor(any_uti) + factor(any_internado), data = bc)
costo$modelo_coef <- data.table(
  termino = names(coef(mcost)),
  coef = round(coef(mcost), 3),
  mult_costo = round(exp(coef(mcost)), 2))   # efecto multiplicativo sobre el costo
costo$modelo_r2 <- summary(mcost)$r.squared

cat("\n==== COSTO POR GRANULARIDAD ====\n"); print(costo$granularidad)
cat("\n==== COSTO POR UCI (0/1) ====\n"); print(costo$por_uti)
cat("\n==== COSTO POR INTERNACIÓN (0/1) ====\n"); print(costo$por_internado)
cat("\n==== MODELO log(costo) ~ perfil  (R2 =", round(costo$modelo_r2,3), ") ====\n")
print(costo$modelo_coef)

png(file.path(OUT, "costo_distribucion.png"), width = 800, height = 600, res = 110)
hist(log10(pmax(cx_benef, 1)), breaks = 40, col = "#6A1B9A", border = "white",
     main = "Costo total por beneficiario con neumonía (log10)",
     xlab = "log10(costo en $)")
dev.off()

png(file.path(OUT, "costo_por_uti.png"), width = 800, height = 600, res = 110)
boxplot(log10(pmax(costo, 1)) ~ any_uti, data = bc, col = c("#F57C00","#6A1B9A"),
        names = c("Sin UCI","Con UCI"), ylab = "log10(costo)",
        main = "Costo por beneficiario según paso por UCI")
dev.off()

saveRDS(desc,  file.path(OUT, "descriptivo_raw.rds"))
saveRDS(costo, file.path(OUT, "costo_resultados.rds"))
fwrite(costo$granularidad, file.path(OUT, "costo_granularidad.csv"))
fwrite(costo$modelo_coef,  file.path(OUT, "costo_modelo.csv"))

