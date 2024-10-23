calcular_saldo <- function(.rota, parametros, matriz_distancias,
                           carga_energia_brasil,
                           produtos_selecionados = NULL,
                           fatores_sazonalidade) {
  parametros_r <- parametros |>
    dplyr::filter(rota == .rota)

  custo_aquisicao <- calcular_custo_aquisicao(parametros_r) # $/t
  multiplicador_vp <- calcular_mult_valor_presente(parametros_r) # adimensional
  capex_opex_adic_por_mj <- calcular_custo_por_mj(parametros_r) # $/MJ
  custo_armazenamento <- parametros_r$custo_armazenamento_por_t

  eficiencia_usina <- parametros_r$efic_usina # adimensional
  receita <- parametros_r$receita # $/MWh ou $/m3 ou $/ton

  if (!is.null(produtos_selecionados)) {
    carga_energia_brasil <- carga_energia_brasil |>
      dplyr::filter(produto_codigo %in% produtos_selecionados)
  }

  tab_energetico <- arrumar_tab_energetico(carga_energia_brasil, .rota) |>
    dplyr::left_join(
      fatores_sazonalidade,
      by = c("muni_cod", "producao")
    )

  tab_distancias <- matriz_distancias |>
    dplyr::mutate(
      custo_transporte = dplyr::case_when(
        distancia == 0 ~ 0,
        TRUE ~ !!parametros_r$custo_fixo_transporte +
          !!parametros_r$custo_transporte_por_t_km * distancia
      )
    )

  if (parametros_r$unidade_receita == "mwh") {
    tab_saldo <- tab_distancias |>
      dplyr::left_join(
        dplyr::select(
          tab_energetico,
          muni_cod,
          energetico,
          fator_proporcional,
          combustivel_mj = glue::glue("{.rota}_combustivel_mj")
        ),
        by = c("muni_cod_origem" = "muni_cod")
      ) |>
      dplyr::filter(!is.na(energetico)) |>
      dplyr::mutate(
        saldo = receita * combustivel_mj / 3600 * eficiencia_usina * multiplicador_vp -
          capex_opex_adic_por_mj * combustivel_mj * eficiencia_usina -
          custo_aquisicao * energetico * multiplicador_vp -
          custo_transporte * energetico * multiplicador_vp -
          energetico * fator_proporcional * custo_armazenamento * multiplicador_vp,
        energia_mj = combustivel_mj * eficiencia_usina
      )
  } else if (parametros_r$unidade_receita == "m3") {
    tab_saldo <- tab_distancias |>
      dplyr::left_join(
        dplyr::select(
          tab_energetico,
          muni_cod,
          energetico,
          fator_combustivel_proporcional,
          combustivel_mj = glue::glue("{.rota}_combustivel_mj"),
          combustivel_m3 = glue::glue("{.rota}_combustivel_m3")
        ),
        by = c("muni_cod_origem" = "muni_cod"),
        copy = TRUE
      ) |>
      dplyr::filter(!is.na(energetico)) |>
      dplyr::mutate(
        saldo = receita * combustivel_m3 * multiplicador_vp -
          capex_opex_adic_por_mj * combustivel_mj * eficiencia_usina -
          custo_aquisicao * energetico * multiplicador_vp -
          custo_transporte * energetico * multiplicador_vp -
          energetico * fator_combustivel_proporcional * custo_armazenamento * multiplicador_vp,
        energia_mj = combustivel_mj
      )
  } else if (parametros_r$unidade_receita == "ton") {
    tab_saldo <- tab_distancias |>
      dplyr::left_join(
        dplyr::select(
          tab_energetico,
          muni_cod,
          energetico,
          fator_proporcional,
          combustivel_mj = glue::glue("{.rota}_combustivel_mj")
        ),
        by = c("muni_cod_origem" = "muni_cod"),
        copy = TRUE
      ) |>
      dplyr::filter(!is.na(energetico)) |>
      dplyr::mutate(
        saldo = receita * energetico * multiplicador_vp -
          custo_aquisicao * energetico * multiplicador_vp -
          custo_transporte * energetico * multiplicador_vp -
          energetico * fator_proporcional * custo_armazenamento * multiplicador_vp,
        energia_mj = combustivel_mj
      )
  }

  tab_saldo |>
    dplyr::select(
      muni_cod_origem,
      muni_cod_destino,
      energetico,
      distancia,
      combustivel_mj,
      tidyselect::any_of("combustivel_m3"),
      energia_mj,
      saldo,
      dplyr::starts_with("fator")
    )
}

arrumar_tab_energetico <- function(carga_energia_brasil, .rota) {
  if (stringr::str_detect(.rota, "pec")) {
    tab <- carga_energia_brasil |>
      dplyr::select(
        uf, muni_cod, muni_nome, produto_codigo, produto_nome, producao,
        dplyr::contains("pec")
      ) |>
      dplyr::filter(
        dplyr::if_all(
          .cols = dplyr::everything(),
          .fns = ~ !is.na(.)
        )
      ) |>
      dplyr::rename(
        # coproduto = pec_coproduto,
        energetico = biocomb_pec_energetico
      ) |>
      dplyr::select(-bioeletr_pec_energetico)
  } else {
    tab <- carga_energia_brasil |>
      dplyr::select(
        uf,
        muni_cod,
        muni_nome,
        produto_codigo,
        produto_nome,
        producao,
        !dplyr::contains("pec")
      ) |>
      dplyr::filter(
        dplyr::if_all(
          .cols = dplyr::everything(),
          .fns = ~ !is.na(.)
        )
      )
  }

  tab |>
    dplyr::group_by(muni_cod, muni_nome, producao) |>
    dplyr::summarise(dplyr::across(
      c(
        # coproduto,
        energetico,
        dplyr::contains("combustivel")
      ),
      .fns = function(x) {
        sum(x, na.rm = TRUE)
      }
    ), .groups = "drop")
}
