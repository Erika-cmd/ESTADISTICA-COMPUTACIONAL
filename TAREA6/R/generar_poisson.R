#' Genera números aleatorios de una distribución Poisson
#'
#' Esta función genera números aleatorios de una distribución de Poisson con un parámetro lambda definido por el usuario.
#'
#' @param n Número de valores a generar.
#' @param lambda Parámetro de tasa de la distribución de Poisson. Debe ser positivo.
#'
#' @return Un vector de números aleatorios de la distribución de Poisson.
#' @export
#'
#' @examples
#' generar_poisson(10, lambda = 3)
generar_poisson <- function(n, lambda = 1) {
  rpois(n, lambda)
}
