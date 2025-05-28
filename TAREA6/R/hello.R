#' Generar números binomiales aleatorios
#'
#' Esta función genera una muestra de números aleatorios desde una distribución binomial.
#'
#' @param n Número de observaciones
#' @param size Número de ensayos por observación
#' @param prob Probabilidad de éxito en cada ensayo
#' @return Vector con números binomiales generados
#' @export
#'
#' @examples
#' generar_binomial(10, size = 5, prob = 0.3)
generar_binomial <- function(n, size, prob) {
  rbinom(n, size, prob)
}
