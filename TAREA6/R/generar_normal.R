#' Genera números aleatorios de una distribución normal
#'
#' Esta función genera números aleatorios de una distribución normal con parámetros definidos por el usuario.
#'
#' @param n Número de valores a generar.
#' @param mean Media de la distribución normal. Valor predeterminado es 0.
#' @param sd Desviación estándar de la distribución normal. Valor predeterminado es 1.
#'
#' @return Un vector de números aleatorios de la distribución normal.
#' @export
generar_normal <- function(n, mean = 0, sd = 1) {
  rnorm(n, mean, sd)
}
