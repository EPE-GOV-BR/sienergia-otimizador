calcular_fatores_sazonalidade <- function(con, produtos_selecionados = NULL,
                                          sazonalidade, carga_energia_brasil,
                                          backend) {
  if (sazonalidade) {
    producoes_agricolas <- dplyr::tbl(
      con,
      "sazonalidade_producao_agricola"
    ) |>
      dplyr::collect()
    sazonalidade_venda_combustiveis <- dplyr::tbl(
      con,
      "sazonalidade_venda_combustiveis"
    ) |>
      dplyr::collect()

    sazonalidade_combustiveis <- sazonalidade_venda_combustiveis |>
      dplyr::mutate(
        ano = as.character(ano),
        mes_numero = stringr::str_pad(mes_numero,
          width = 2,
          pad = "0",
          side = "left"
        )
      ) |>
      dplyr::group_by(uf_sigla, ano, mes_numero) |>
      dplyr::summarise(
        prop_vendas = sum(vendas_m3)
      ) |>
      dplyr::group_by(uf_sigla, ano) |>
      dplyr::mutate(
        prop_vendas = prop_vendas / sum(prop_vendas, na.rm = TRUE)
      ) |>
      dplyr::group_by(uf_sigla, mes_numero) |>
      dplyr::summarise(
        prop_vendas = mean(prop_vendas)
      )

    if (!is.null(produtos_selecionados)) {
      carga_energia_brasil <- carga_energia_brasil |>
        dplyr::filter(produto_codigo %in% produtos_selecionados)
    }

    fatores <- producoes_agricolas |>
      dplyr::rename(uf_sigla = uf_nome) |>
      dplyr::left_join(
        sazonalidade_combustiveis,
        by = c("uf_sigla", "mes_numero")
      ) |>
      dplyr::group_by(uf_sigla, produto_nome, produto_codigo) |>
      dplyr::summarise(
        minimo_auxiliar = max(-min(cumsum(prop_producao - 1 / 12), 0)),
        mimimo_auxiliar_combustivel = max(-min(cumsum(prop_producao - prop_vendas), 0)),
        fator = sum(cumsum(prop_producao - 1 / 12)) + 12 * minimo_auxiliar,
        fator_combustivel = sum(cumsum(prop_producao - prop_vendas)) + 12 * mimimo_auxiliar_combustivel
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-produto_nome)

    fatores_finais <- carga_energia_brasil |>
      dplyr::left_join(fatores, by = c("uf" = "uf_sigla", "produto_codigo"), copy = TRUE) |>
      tidyr::replace_na(list(fator = 5.5, fator_combustivel = 5.5)) |>
      dplyr::group_by(uf, muni_cod, producao) |>
      dplyr::summarise(
        fator_proporcional = sum(energetico * fator) / sum(energetico),
        fator_combustivel_proporcional = sum(energetico * fator_combustivel) / sum(energetico)
      ) |>
      tidyr::replace_na(list(fator_proporcional = 0, fator_combustivel_proporcional = 0))
  } else {
    t1 <- carga_energia_brasil |>
      dplyr::distinct(muni_cod) |>
      dplyr::mutate(
        producao = "Agrícola",
        fator_proporcional = 0,
        fator_combustivel_proporcional = 0
      )

    t2 <- carga_energia_brasil |>
      dplyr::distinct(muni_cod) |>
      dplyr::mutate(
        producao = "Pecuária",
        fator_proporcional = 0,
        fator_combustivel_proporcional = 0
      )

    dplyr::union_all(t1, t2)
  }
}
