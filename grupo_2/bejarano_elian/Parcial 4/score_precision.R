library(dplyr)

ev <- read.csv("consultas_evaluacion.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
ev$is_relevant <- 0L

mark <- function(q, strategy, ranks) {
  ev$is_relevant[ev$query_id == q & ev$strategy == strategy & ev$rank %in% ranks] <<- 1L
}

mark("Q1_directa", "TF-IDF + coseno", c(1, 2, 3, 5))
mark("Q1_directa", "LSA/SVD + coseno", c(3))
mark("Q2_sinonimos", "TF-IDF + coseno", 1:5)
mark("Q2_sinonimos", "LSA/SVD + coseno", 1:5)
mark("Q3_general", "TF-IDF + coseno", 1:5)
mark("Q3_general", "LSA/SVD + coseno", 1:5)
mark("Q4_especifica", "TF-IDF + coseno", c(1, 2))
mark("Q4_especifica", "LSA/SVD + coseno", c(2))
mark("Q5_dificil", "TF-IDF + coseno", c(2))
mark("Q5_dificil", "LSA/SVD + coseno", c(4))

write.csv(ev, "consultas_evaluacion.csv", row.names = FALSE, fileEncoding = "UTF-8")

summary <- ev |>
  group_by(query_id, query_type, query, strategy) |>
  summarise(
    relevantes_top5 = sum(is_relevant),
    precision_at_5 = round(sum(is_relevant) / 5, 2),
    .groups = "drop"
  )

write.csv(summary, "precision_at_5.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(summary)
