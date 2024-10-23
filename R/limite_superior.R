
# limita a quantidade de usinas por sedes candidata
gerar_limite_superior_vars_decisao <- function(inputs, indices,
                                               num_max_usinas_sede, rotas) {
  n_possiveis_sedes <- purrr::map(
    rotas,
    ~ if (.x != "cofiring") {
      inputs$matriz_distancias_saldos[[.x]] |>
        pegar_num_distintos(muni_cod_destino)
    } else {
      0
    }
  )

  limites <- list()

  if ("dens" %in% rotas | "bioeletr" %in% rotas | "bioeletr_pec" %in% rotas) {
    limites <- purrr::map(
      rotas_eletricas(rotas),
      ~ calcular_limites_superiores(inputs, indices, num_max_usinas_sede, .x)
    )
  } else {
    limites[["dens"]] <- NULL
    limites[["bioeletr"]] <- NULL
    limites[["bioeletr_pec"]] <- NULL
  }

  for (r in rotas[!rotas %in% rotas_eletricas(rotas)]) {
    limites[[r]] <- rep(1, length(indices[[r]]) + n_possiveis_sedes[[r]])
  }

  c(
    limites[["dens"]], limites[["biocomb"]], limites[["bioeletr"]],
    limites[["biocomb_pec"]], limites[["bioeletr_pec"]], limites[["cofiring"]]
  )
}

calcular_limites_superiores <- function(inputs, indices, num_max_usinas_sede, .rota) {
  tab_num_max_usinas <- inputs$matriz_distancias_saldos[[.rota]] |>
    dplyr::mutate(rowid = dplyr::row_number()) |>
    dplyr::filter(rowid %in% !!indices[[.rota]]) |>
    dplyr::group_by(muni_cod_destino) |>
    dplyr::summarise(
      soma_energia = sum(energia_mj, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      potencia = (soma_energia / !!inputs$fator_disponibilidade[[.rota]]) * (1 / (24 * 365 * 3600)),
      max_usinas_possivel_dbl = potencia / !!inputs$limite_tamanho_usina[[.rota]],
      max_usinas_possivel = ceiling(max_usinas_possivel_dbl),
      max_usinas_geral = !!num_max_usinas_sede
    ) |>
    dplyr::mutate(
      num_max_usinas = ifelse(
        max_usinas_possivel < max_usinas_geral,
        max_usinas_possivel,
        max_usinas_geral
      )
    ) |>
    dplyr::select(muni_cod_destino, num_max_usinas)

  vetor_num_max_usinas <- inputs$matriz_distancias_saldos[[.rota]] |>
    dplyr::distinct(muni_cod_destino) |>
    dplyr::left_join(tab_num_max_usinas, by = "muni_cod_destino") |>
    tidyr::replace_na(list(num_max_usinas = 0)) |>
    dplyr::pull(num_max_usinas)

  c(rep(1, length(indices[[.rota]])), vetor_num_max_usinas)
}