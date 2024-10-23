gerar_vetor_restricoes <- function(inputs, limitar_tamanho_usina,
                                   limitar_por_demanda, rotas) {
  # Restrição 1: Uma sede pode ser abastecida por, no máximo, n municípios.
  # Se um município não é uma sede, então ele não pode ser abastecido por
  # nenhum município (aij <= Nqi)

  restricao_1 <- purrr::map(
    rotas,
    function(x) {
      if (x == "cofiring") {
        n_possiveis_sedes <- 0
      } else {
        n_possiveis_sedes <- inputs$matriz_distancias_saldos[[x]] |>
          pegar_num_distintos(muni_cod_destino)
      }

      rep(0, n_possiveis_sedes)
    }
  )

  # Restrição 2: A potência máxima por usina elétrica é de 5MW
  # (mas cada sede pode ter mais de uma usina).

  tipo <- separar_rotas_por_tipo(rotas)
  rotas_ele <- rotas_eletricas(rotas)

  if (limitar_tamanho_usina & length(rotas_ele) != 0) {
    restricao_2 <- purrr::map(
      rotas_ele,
      ~ restricao_1[[.x]]
    )
  } else {
    restricao_2 <- list(NULL)
  }

  # Restrição 3: a porcentagem de resíduos fornecidos por um município não
  # pode ultrapassar 100%  (aij + bij + fij <= 1).

  origem <- separar_rotas_por_origem(rotas)

  n_municipios <- purrr::map(
    origem,
    function(x) {
      purrr::map(
        x,
        ~ inputs$matriz_distancias_saldos[[.x]] |>
          dplyr::pull(muni_cod_origem) |>
          unique()
      ) |>
        purrr::list_c() |>
        dplyr::n_distinct()
    }
  )

  restricao_3 <- purrr::map(
    n_municipios,
    ~ rep(1, .x)
  )

  # Restrição 4: A energia elétrica produzida por todas as usinas
  # elétricas sobre a cobertura de uma distribuidora tem que ser
  # menor que a demanda atual ((aij + fij) ej <= Lk).

  if (limitar_por_demanda) {
    demandas_rotas <- purrr::map(
      rotas,
      ~ inputs$matriz_distancias_saldos[[.x]] |>
        dplyr::distinct(grupo_demanda, demanda)
    )

    if (inputs$backend == "disco") {
      demandas_rotas <- purrr::map(
        demandas_rotas,
        dplyr::collect
      )
    }

    restricao_4 <- purrr::map(
      tipo,
      function(x) {
        if (length(x) != 0) {
          purrr::map_dfr(
            x,
            ~ demandas_rotas[[.x]]
          ) |>
            dplyr::distinct() |>
            dplyr::arrange(grupo_demanda) |>
            dplyr::pull(demanda)
        } else {
          NULL
        }
      }
    )
  } else {
    restricao_4 <- list(NULL)
  }

  # Criando vetor de restrições

  c(
    restricao_1[["dens"]],
    restricao_1[["biocomb"]],
    restricao_1[["bioeletr"]],
    restricao_1[["biocomb_pec"]],
    restricao_1[["bioeletr_pec"]],
    restricao_2[["dens"]],
    restricao_2[["bioeletr"]],
    restricao_2[["bioeletr_pec"]],
    restricao_3[["rotas_agricolas"]],
    restricao_3[["rotas_pecuarias"]],
    restricao_4[["rotas_eletricas"]],
    restricao_4[["rotas_combustivel"]],
    restricao_4[["rotas_cofiring"]]
  )
}

gerar_matriz_restricoes <- function(inputs, indices,
                                    limitar_tamanho_usina,
                                    limitar_por_demanda,
                                    rotas) {
  # Valores úteis
  possiveis_sedes <- purrr::map(
    rotas,
    ~ inputs$matriz_distancias_saldos[[.x]] |>
      dplyr::pull(muni_cod_destino) |>
      unique()
  )

  n_possiveis_sedes <- purrr::map(
    rotas,
    ~ possiveis_sedes[[.x]] |>
      length()
  )

  origem <- separar_rotas_por_origem(rotas)
  tipo <- separar_rotas_por_tipo(rotas)

  municipios_origem <- purrr::map(
    origem,
    function(x) {
      purrr::map(
        x,
        ~ inputs$matriz_distancias_saldos[[.x]] |>
          dplyr::pull(muni_cod_origem) |>
          unique()
      ) |>
        purrr::list_c() |>
        unique()
    }
  )

  n_municipios_origem <- purrr::map(
    municipios_origem,
    ~ length(.x)
  )

  num_municipios_sedes <- purrr::map(
    rotas,
    ~ inputs$matriz_distancias_saldos[[.x]] |>
      dplyr::count(muni_cod_destino) |>
      dplyr::pull(n)
  )

  # Restrição 1: Uma sede pode ser abastecida por, no máximo, n municípios.
  # Se um município não é uma sede, então ele não pode ser abastecido por
  # nenhum município (aij <= Nqi)

  M1_a <- purrr::map(
    rotas,
    ~ Matrix::sparseMatrix(
      i = match(
        dplyr::pull(inputs$matriz_distancias_saldos[[.x]], muni_cod_destino),
        possiveis_sedes[[.x]]
      ),
      j = 1:calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]]),
      dims = c(
        n_possiveis_sedes[[.x]],
        calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]])
      )
    )
  )

  M1_b <- purrr::map(
    rotas,
    ~ if (.x != "cofiring") {
      Matrix::sparseMatrix(
        i = 1:length(num_municipios_sedes[[.x]]),
        j = 1:length(num_municipios_sedes[[.x]]),
        x = -num_municipios_sedes[[.x]]
      )
    } else {
      NULL
    }
  )

  M1 <- purrr::map(
    rotas,
    ~ cbind(
      M1_a[[.x]][, indices[[.x]], drop = FALSE],
      M1_b[[.x]]
    )
  )

  # Restrição 2: A potência máxima por usina termoelétrica é de 5MW
  # (mas cada sede pode ter mais de uma usina).

  if (limitar_tamanho_usina & length(rotas_eletricas(rotas)) != 0) {
    rotas_ele <- rotas_eletricas(rotas)
    M2_a <- purrr::map(
      rotas_ele,
      ~ Matrix::sparseMatrix(
        i = match(
          dplyr::pull(inputs$matriz_distancias_saldos[[.x]], muni_cod_destino),
          possiveis_sedes[[.x]]
        ),
        j = 1:calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]]),
        x = dplyr::pull(inputs$matriz_distancias_saldos[[.x]], energia_mj),
        dims = c(
          n_possiveis_sedes[[.x]],
          calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]])
        )
      )
    )

    limite_energia_potencia <- purrr::map(
      rotas_ele,
      function(x) {
        inputs$limite_tamanho_usina[[x]] * 24 * 365 *
          inputs$fator_disponibilidade[[x]] * 3600 # convertendo pra mj
      }
    )

    limite <- purrr::map(
      rotas_ele,
      ~ rep(limite_energia_potencia[[.x]], n_possiveis_sedes[[.x]])
    )

    n_limite <- purrr::map(
      rotas_ele,
      ~ length(limite[[.x]])
    )

    M2_b <- purrr::map(
      rotas_ele,
      ~ Matrix::sparseMatrix(
        i = 1:n_limite[[.x]],
        j = 1:n_limite[[.x]],
        x = -limite[[.x]],
        dims = c(n_limite[[.x]], n_limite[[.x]])
      )
    )

    M2 <- purrr::map(
      rotas,
      ~ cbind(
        M2_a[[.x]][, indices[[.x]], drop = FALSE],
        M2_b[[.x]]
      )
    )
  } else {
    M2 <- NULL
  }

  # Restrição 3: a porcentagem de resíduos fornecidos por um município não
  # pode ultrapassar 100% (aij + bij + fij <= 1).

  M3 <- purrr::map2(
    origem,
    names(origem),
    function(x, y) {
      purrr::map(
        x,
        ~ Matrix::sparseMatrix(
          i = match(
            dplyr::pull(inputs$matriz_distancias_saldos[[.x]], muni_cod_origem),
            municipios_origem[[y]]
          ),
          j = 1:calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]]),
          dims = c(
            n_municipios_origem[[y]],
            calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]])
          )
        )
      )
    }
  )

  M3 <- purrr::map2(
    M3,
    origem,
    function(x, y) {
      purrr::map2(
        x,
        y,
        ~ .x[, indices[[.y]], drop = FALSE]
      )
    }
  )

  # Restrição 4: A energia elétrica produzida por todas as usinas
  # termoelétricas sobre a cobertura de uma distribuidora tem que ser
  # menor que a demanda atual ((aij + fij) ej <= Lk).

  if (limitar_por_demanda) {
    demandas_rotas <- purrr::map(
      rotas,
      function(x) {
        inputs$matriz_distancias_saldos[[x]] |>
          dplyr::distinct(grupo_demanda, demanda)
      }
    )

    if (inputs$backend == "disco") {
      demandas_rotas <- purrr::map(
        demandas_rotas,
        dplyr::collect
      )
    }

    grupos_tipo_demanda <- purrr::map(
      tipo,
      function(x) {
        if (length(x) != 0) {
          purrr::map_dfr(
            x,
            ~ demandas_rotas[[.x]]
          ) |>
            dplyr::distinct() |>
            dplyr::arrange(grupo_demanda) |>
            dplyr::pull(grupo_demanda)
        } else {
          NULL
        }
      }
    )

    grupos <- purrr::map(
      rotas,
      function(x) {
        if (x %in% rotas_eletricas(rotas)) {
          grupos_tipo_demanda$rotas_eletricas
        } else if (x %in% rotas_combustivel(rotas)) {
          grupos_tipo_demanda$rotas_combustivel
        } else {
          grupos_tipo_demanda$rotas_cofiring
        }
      }
    )

    M4 <- purrr::map(
      rotas,
      ~ Matrix::sparseMatrix(
        i = match(
          dplyr::pull(inputs$matriz_distancias_saldos[[.x]], grupo_demanda),
          grupos[[.x]]
        ),
        j = 1:calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]]),
        x = dplyr::pull(inputs$matriz_distancias_saldos[[.x]], energia_mj),
        dims = c(
          length(grupos[[.x]]),
          calcular_num_linhas(inputs$matriz_distancias_saldos[[.x]])
        )
      )
    )
  } else {
    M4 <- NULL
  }

  M4 <- purrr::map(
    rotas,
    ~ M4[[.x]][, indices[[.x]], drop = FALSE]
  )


  # Montando matriz de restrições -------------------------------------------

  # Esquema
  # os 0s são matrizes esparças nulas, criadas com a função esparssa_nula()

  #   a  | q |   b  | s |   f   | w |   g   | o |    m   | u | alf

  #    M1_d  |   0  | 0 |   0   | 0 |   0   | 0 |    0   | 0 |  0
  #   0  | 0 |    M1_b  |   0   | 0 |   0   | 0 |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |    M1_be  |   0   | 0 |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |   0   | 0 |   M1_bp   |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |   0   | 0 |   0   | 0 |    M1_bep  |  0
  #    M2_d  |   0  | 0 |   0   | 0 |   0   | 0 |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |    M2_be  |   0   | 0 |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |   0   | 0 |   0   | 0 |    M2_bep  |  0
  # M3_d | 0 | M3_b | 0 | M3_be | 0 |   0   | 0 |    0   | 0 |M3_cof
  #   0  | 0 |   0  | 0 |   0   | 0 | M3_bp | 0 | M3_bep | 0 |  0
  # M4_d | 0 |   0  | 0 | M4_be | 0 |   0   | 0 | M3_bep | 0 |  0
  #   0  | 0 | M4_b | 0 |   0   | 0 | M4_bp | 0 |    0   | 0 |  0
  #   0  | 0 |   0  | 0 |   0   | 0 |   0   | 0 |    0   | 0 |M4_cof

  possiveis_rotas <- rotas_possiveis()

  n_col_M1 <- purrr::map(
    possiveis_rotas,
    function(x) {
      if (!is.null(M1[[x]])) {
        ncol(M1[[x]])
      } else {
        0
      }
    }
  )

  n_col_M1_b <- purrr::map(
    possiveis_rotas,
    function(x) {
      if (!is.null(M1_b[[x]]) & x != "cofiring") {
        ncol(M1_b[[x]])
      } else {
        0
      }
    }
  )

  n_colunas <- purrr::map_dbl(rotas, ~ ncol(M1[[.x]])) |> sum()

  rbind(
    # M1_dens
    cbind(
      M1[["dens"]],
      esparssa_nula(m = nrow(M1[["dens"]]), n = n_colunas - ncol(M1[["dens"]]))
    ),
    # M1_biocomb
    cbind(
      esparssa_nula(m = nrow(M1[["biocomb"]]), n = n_col_M1[["dens"]]),
      M1[["biocomb"]],
      esparssa_nula(m = nrow(M1[["biocomb"]]), n = n_col_M1[["bioeletr"]]),
      esparssa_nula(m = nrow(M1[["biocomb"]]), n = n_col_M1[["biocomb_pec"]]),
      esparssa_nula(m = nrow(M1[["biocomb"]]), n = n_col_M1[["bioeletr_pec"]]),
      esparssa_nula(m = nrow(M1[["biocomb"]]), n = n_col_M1[["cofiring"]])
    ),
    # M1_bioeletr
    cbind(
      esparssa_nula(m = nrow(M1[["bioeletr"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M1[["bioeletr"]]), n = n_col_M1[["biocomb"]]),
      M1[["bioeletr"]],
      esparssa_nula(m = nrow(M1[["bioeletr"]]), n = n_col_M1[["biocomb_pec"]]),
      esparssa_nula(m = nrow(M1[["bioeletr"]]), n = n_col_M1[["bioeletr_pec"]]),
      esparssa_nula(m = nrow(M1[["bioeletr"]]), n = n_col_M1[["cofiring"]])
    ),
    # M1_biocomb_pec
    cbind(
      esparssa_nula(m = nrow(M1[["biocomb_pec"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M1[["biocomb_pec"]]), n = n_col_M1[["biocomb"]]),
      esparssa_nula(m = nrow(M1[["biocomb_pec"]]), n = n_col_M1[["bioeletr"]]),
      M1[["biocomb_pec"]],
      esparssa_nula(m = nrow(M1[["biocomb_pec"]]), n = n_col_M1[["bioeletr_pec"]]),
      esparssa_nula(m = nrow(M1[["biocomb_pec"]]), n = n_col_M1[["cofiring"]])
    ),
    # M1_bioeletr_pec
    cbind(
      esparssa_nula(m = nrow(M1[["bioeletr_pec"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M1[["bioeletr_pec"]]), n = n_col_M1[["biocomb"]]),
      esparssa_nula(m = nrow(M1[["bioeletr_pec"]]), n = n_col_M1[["bioeletr"]]),
      esparssa_nula(m = nrow(M1[["bioeletr_pec"]]), n = n_col_M1[["biocomb_pec"]]),
      M1[["bioeletr_pec"]],
      esparssa_nula(m = nrow(M1[["bioeletr_pec"]]), n = n_col_M1[["cofiring"]])
    ),
    # M2_dens
    cbind(
      M2[["dens"]],
      esparssa_nula(m = nrow(M2[["dens"]]), n = n_colunas - ncol(M2[["dens"]]))
    ),
    # M2_bioeletr
    cbind(
      esparssa_nula(m = nrow(M2[["bioeletr"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M2[["bioeletr"]]), n = n_col_M1[["biocomb"]]),
      M2[["bioeletr"]],
      esparssa_nula(m = nrow(M2[["bioeletr"]]), n = n_col_M1[["biocomb_pec"]]),
      esparssa_nula(m = nrow(M2[["bioeletr"]]), n = n_col_M1[["bioeletr_pec"]]),
      esparssa_nula(m = nrow(M2[["bioeletr"]]), n = n_col_M1[["cofiring"]])
    ),
    # M2_bioeletr_pec
    cbind(
      esparssa_nula(m = nrow(M2[["bioeletr_pec"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M2[["bioeletr_pec"]]), n = n_col_M1[["biocomb"]]),
      esparssa_nula(m = nrow(M2[["bioeletr_pec"]]), n = n_col_M1[["bioeletr"]]),
      esparssa_nula(m = nrow(M2[["bioeletr_pec"]]), n = n_col_M1[["biocomb_pec"]]),
      M2[["bioeletr_pec"]],
      esparssa_nula(m = nrow(M2[["bioeletr_pec"]]), n = n_col_M1[["cofiring"]])
    ),
    # M3 agro
    cbind(
      M3$rotas_agricolas[["dens"]],
      esparssa_nula(m = nrow(M3$rotas_agricolas[["dens"]]), n = n_col_M1_b[["dens"]]),
      M3$rotas_agricolas[["biocomb"]],
      esparssa_nula(m = nrow(M3$rotas_agricolas[["biocomb"]]), n = n_col_M1_b[["biocomb"]]),
      M3$rotas_agricolas[["bioeletr"]],
      esparssa_nula(m = nrow(M3$rotas_agricolas[["bioeletr"]]), n = n_col_M1_b[["bioeletr"]]),
      esparssa_nula(m = calcula_n_linhas_M3_agro(M3), n = n_col_M1[["biocomb_pec"]]),
      esparssa_nula(m = calcula_n_linhas_M3_agro(M3), n = n_col_M1[["bioeletr_pec"]]),
      M3$rotas_agricolas[["cofiring"]]
    ),
    # M3 pec
    cbind(
      esparssa_nula(m = calcula_n_linhas_M3_pec(M3), n = n_col_M1[["dens"]]),
      esparssa_nula(m = calcula_n_linhas_M3_pec(M3), n = n_col_M1[["biocomb"]]),
      esparssa_nula(m = calcula_n_linhas_M3_pec(M3), n = n_col_M1[["bioeletr"]]),
      M3$rotas_pecuarias[["biocomb_pec"]],
      esparssa_nula(m = nrow(M3$rotas_pecuarias[["biocomb_pec"]]), n = n_col_M1_b[["biocomb_pec"]]),
      M3$rotas_pecuarias[["bioeletr_pec"]],
      esparssa_nula(m = nrow(M3$rotas_pecuarias[["bioeletr_pec"]]), n = n_col_M1_b[["bioeletr_pec"]]),
      esparssa_nula(m = calcula_n_linhas_M3_pec(M3), n = n_col_M1[["cofiring"]])
    ),
    # M4_dens + M4_bioeletr + M4_bioeletr_pec
    cbind(
      M4[["dens"]],
      esparssa_nula(m = nrow(M4[["dens"]]), n = n_col_M1_b[["dens"]]),
      esparssa_nula(m = calcula_n_linhas_M4_eletr(M4), n = n_col_M1[["biocomb"]]),
      M4[["bioeletr"]],
      esparssa_nula(m = nrow(M4[["bioeletr"]]), n = n_col_M1_b[["bioeletr"]]),
      esparssa_nula(m = calcula_n_linhas_M4_eletr(M4), n = n_col_M1[["biocomb_pec"]]),
      M4[["bioeletr_pec"]],
      esparssa_nula(m = nrow(M4[["bioeletr_pec"]]), n = n_col_M1_b[["bioeletr_pec"]]),
      esparssa_nula(m = calcula_n_linhas_M4_eletr(M4), n = n_col_M1[["cofiring"]])
    ),
    # M4_biocomb + M4_biocomb_pec
    cbind(
      esparssa_nula(m = calcula_n_linhas_M4_comb(M4), n = n_col_M1[["dens"]]),
      M4[["biocomb"]],
      esparssa_nula(m = nrow(M4[["biocomb"]]), n = n_col_M1_b[["biocomb"]]),
      esparssa_nula(m = calcula_n_linhas_M4_comb(M4), n = n_col_M1[["bioeletr"]]),
      M4[["biocomb_pec"]],
      esparssa_nula(m = nrow(M4[["biocomb_pec"]]), n = n_col_M1_b[["biocomb_pec"]]),
      esparssa_nula(m = calcula_n_linhas_M4_comb(M4), n = n_col_M1[["bioeletr_pec"]]),
      esparssa_nula(m = calcula_n_linhas_M4_comb(M4), n = n_col_M1[["cofiring"]])
    ),
    # M4_cofiring
    cbind(
      esparssa_nula(m = nrow(M4[["cofiring"]]), n = n_col_M1[["dens"]]),
      esparssa_nula(m = nrow(M4[["cofiring"]]), n = n_col_M1[["biocomb"]]),
      esparssa_nula(m = nrow(M4[["cofiring"]]), n = n_col_M1[["bioeletr"]]),
      esparssa_nula(m = nrow(M4[["cofiring"]]), n = n_col_M1[["biocomb_pec"]]),
      esparssa_nula(m = nrow(M4[["cofiring"]]), n = n_col_M1[["bioeletr_pec"]]),
      M4[["cofiring"]]
    )
  )
}


calcula_n_linhas_M4_eletr <- function(x) {
  if (!is.null(x[["dens"]])) {
    nrow(x[["dens"]])
  } else if (!is.null(x[["bioeletr"]])) {
    nrow(x[["bioeletr"]])
  } else if (!is.null(x[["bioeletr_pec"]])) {
    nrow(x[["bioeletr_pec"]])
  } else {
    NULL
  }
}

calcula_n_linhas_M4_comb <- function(x) {
  if (!is.null(x[["biocomb"]])) {
    nrow(x[["biocomb"]])
  } else if (!is.null(x[["biocomb_pec"]])) {
    nrow(x[["biocomb_pec"]])
  } else {
    NULL
  }
}

calcula_n_linhas_M3_agro <- function(x) {
  if (!is.null(x$rotas_agricolas[["dens"]])) {
    nrow(x$rotas_agricolas[["dens"]])
  } else if (!is.null(x$rotas_agricolas[["biocomb"]])) {
    nrow(x$rotas_agricolas[["biocomb"]])
  } else if (!is.null(x$rotas_agricolas[["bioeletr"]])) {
    nrow(x$rotas_agricolas[["bioeletr"]])
  } else if (!is.null(x$rotas_agricolas[["cofiring"]])) {
    nrow(x$rotas_agricolas[["cofiring"]])
  } else {
    NULL
  }
}

calcula_n_linhas_M3_pec <- function(x) {
  if (!is.null(x$rotas_pecuarias[["biocomb_pec"]])) {
    nrow(x$rotas_pecuarias[["biocomb_pec"]])
  } else if (!is.null(x$rotas_pecuarias[["bioeletr_pec"]])) {
    nrow(x$rotas_pecuarias[["bioeletr_pec"]])
  } else {
    NULL
  }
}

esparssa_nula <- function(m, n) {
  if (is.null(m) | is.null(n)) {
    return(NULL)
  }
  Matrix::sparseMatrix(
    i = NULL,
    j = NULL,
    dims = c(m, n)
  )
}


