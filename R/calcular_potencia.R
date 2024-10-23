#' Calcular a potência em MW
#'
#' @param energia Energia em MWh.
#' @param fator_disp Fator de disponibilidade.
#'
#' @return vetor de tamanho 1
#' @export
#'
calcular_potencia <- function(energia, fator_disp) {
  (energia / fator_disp) * (1 / (24 * 365))
}
