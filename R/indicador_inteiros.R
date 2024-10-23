gerar_indicador_inteiros <- function(rotas, indices, inputs) {
  indicadores <- purrr::map(
    rotas,
    function(x) {
      if (x != "cofiring") {
        n <- pegar_num_distintos(inputs$matriz_distancias_saldos[[x]], muni_cod_destino)
      } else {
        n <- 0
      }
      c(
        rep(FALSE, length(indices[[x]])),
        rep(TRUE, n)
      )
    }
  )

  c(
    indicadores[["dens"]],
    indicadores[["biocomb"]],
    indicadores[["bioeletr"]],
    indicadores[["biocomb_pec"]],
    indicadores[["bioeletr_pec"]],
    indicadores[["cofiring"]]
  )
}