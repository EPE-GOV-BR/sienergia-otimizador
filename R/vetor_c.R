gerar_vetor_c <- function(inputs, indices, rotas) {
  lista_c <- list(
    "dens" = NULL,
    "biocomb" = NULL,
    "bioeletr" = NULL,
    "biocomb_pec" = NULL,
    "bioeletr_pec" = NULL,
    "cofiring" = NULL
  )

  for (r in rotas) {
    if (r == "cofiring") {
      n <- 0
    } else {
      n <- inputs$matriz_distancias_saldos[[r]] |>
        pegar_num_distintos(muni_cod_destino)
    }

    v <- c(
      dplyr::pull(inputs$matriz_distancias_saldos[[r]], saldo)[indices[[r]]],
      rep(inputs$custo_fixo_usina[[r]], n)
    )

    lista_c[[r]] <- v
  }

  vetor_c <- purrr::list_c(lista_c)

  return(vetor_c)
}
