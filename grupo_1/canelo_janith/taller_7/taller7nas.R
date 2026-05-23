install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)

set.seed(42)
n <- 200
datos <- data.frame(
  edad    = c(sample(18:65, 160, replace=TRUE), rep(NA, 40)),       # 20% NA
  ingreso = c(rnorm(100, 3e6, 5e5), rep(NA, 100)),    # 50% NA
  ciudad  = sample(c("Bogotá","Medellín","Cali", NA), n, replace = TRUE,
                   prob = c(0.4, 0.3, 0.2, 0.1)),     # ~10% NA
  compra  = rbinom(n, 1, 0.5)                         # 0% NA
)


(colMeans(is.na(datos)))

(sum(!complete.cases(datos)))

naniar::gg_miss_var(datos)
visdat::vis_miss(datos)

# el la edad los 40 se ponen al azar, ya que se ponen a los ultimos 40 regustristros, 
# por lo tanto es MCAR

# ingreso  MNAR debido a que es una pregunta sensible