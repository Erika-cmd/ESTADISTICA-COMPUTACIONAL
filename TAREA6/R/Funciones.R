# Generar números aleatorios de una distribución normal
generar_normal <- function(n, mean = 0, sd = 1) {
  rnorm(n, mean, sd)
}
