install.packages("DescTools")
install.packages("psych")
library(psych)
library(DescTools)
library(psych)

## Ejemplo: Datos de evaluación docente en una universidad colombiana

# Se tienen 400 evaluaciones de estudiantes a docentes, clasificadas por modalidad (presencial, virtual) y calificación 
# (Excelente, Bueno, Regular). El mismo docente fue evaluado en ambas modalidades.

# Tareas:

# 1. Identificar el esquema de muestreo.

# como se fijó solo el total n = 400 es Multinomial. Se salió a recolectar 400 evaluaciones en total y 
# conteos por modalidad quedaron como resultaron. Esto es lo que describe el enunciado: "se tienen 400 
# evaluaciones", sin mencionar que se fijaron cuántas por modalidad.

# 2. Probar si hay diferencia en la distribución de calificaciones entre modalidades.

set.seed(42)
n_docentes <- 400
niveles <- c("Excelente", "Bueno", "Regular")

presencial <- sample(niveles, n_docentes, replace = TRUE, prob = c(0.45, 0.40, 0.15))
virtual    <- sample(niveles, n_docentes, replace = TRUE, prob = c(0.30, 0.45, 0.25))

tabla <- table(
  Modalidad    = c(rep("Presencial", n_docentes), rep("Virtual", n_docentes)),
  Calificacion = c(presencial, virtual)
)[, niveles]

print(tabla)
chisq.test(tabla)

# Hay evidencia estadística suficiente para concluir que la distribución de calificaciones 
# sí es diferente entre modalidad presencial y virtual. Los docentes tienden a recibir 
# mejores calificaciones en presencial (45% Excelente) que en virtual (30% Excelente).


# 3. Si el docente es la unidad de análisis, ¿qué prueba es la apropiada?

tabla_pareada <- table(
  Presencial = presencial,
  Virtual    = virtual
)[niveles, niveles]

print(tabla_pareada)

StuartMaxwellTest(tabla_pareada)

# Hay evidencia suficiente para concluir que la distribución de calificaciones cambia según la 
# modalidad, incluso cuando se controla por docente. Es decir, el mismo docente tiende a ser 
# calificado de forma distinta en presencial que en virtual.

# 4. Calcular Kappa si las calificaciones se reducen a Aprueba/No aprueba.

# Recodificar: Excelente y Bueno = "Aprueba", Regular = "No aprueba"

presencial_bin <- ifelse(presencial == "Regular", "No aprueba", "Aprueba")
virtual_bin    <- ifelse(virtual    == "Regular", "No aprueba", "Aprueba")

tabla_kappa <- table(
  Presencial = presencial_bin,
  Virtual    = virtual_bin
)

print(tabla_kappa)
cohen.kappa(tabla_kappa)

# El Kappa es 0.023, prácticamente cero, lo que indica que el acuerdo entre las dos modalidades 
# es apenas superior al azar.Además el intervalo de confianza incluye el cero (-0.072 , 0.12), 
# lo que significa que no hay evidencia de acuerdo real entre las calificaciones en presencial y virtual.


