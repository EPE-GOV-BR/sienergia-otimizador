pegar_num_distintos <- function(tab, var) {
  tab |>
    dplyr::distinct({{ var }}) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(n)
}

calcular_num_linhas <- function(tab) {
  tab |>
    dplyr::summarise(
      num_linhas = dplyr::n()
    ) |>
    dplyr::pull(num_linhas)
}

computar_matriz_distancias_saldos <- function(matriz_distancias_saldos) {
  purrr::map(
    matriz_distancias_saldos,
    dplyr::compute
  )
}

pegar_msg_erro <- function(erro) {
  text <- readLines(system.file(
    glue::glue("error_messages/{erro}.txt"),
    package = "otimizadorLinear"
  ))

  paste(text, collapse = "\n")
}

verificar_instalacao <- function(pacote) {
  system.file(package = pacote) != ""
}

pegar_nome_tab_demanda <- function(.rota, parametros) {
  parametros |>
    dplyr::filter(rota == .rota) |>
    dplyr::pull(tab_demanda)
}

pegar_muni_uf <- function(con, regiao_escopo) {
   dplyr::tbl(con, "distancias_BRASIL") |>
    dplyr::filter(uf_o %in% regiao_escopo) |>
    dplyr::distinct(muni_cod_origem) |>
    dplyr::pull(muni_cod_origem)
}


utils::globalVariables(
  c(".sed_cod", "ano", "biocomb_pec_energetico", "bioeletr_pec_energetico",
    "biomassa_aproveitada", "biomassa_disponivel", "calc_num_linhas", "code_muni",
    "combustivel_m3", "combustivel_mj", "combustivel_mj_total",
    "combustivel_mj_x_valor", "consumo_agua_m3", "consumo_agua_m3_por_un",
    "coproduto", "custo_transporte", "demanda", "demanda_existente",
    "demanda_satisfeita", "distancia", "efic_usina", "empregos_por_un", "energetico",
    "energetico_x_valor", "energia", "energia_mj", "energia_ofertada", "fator",
    "fator_combustivel", "fator_combustivel_proporcional", "fator_proporcional",
    "fonte", "fonte_energia", "grupo_demanda", "litros_diesel", "max_usinas_geral",
    "max_usinas_possivel", "max_usinas_possivel_dbl", "mes_numero",
    "milhoes_toneladas_co2_eq", "milhoes_toneladas_co2_eq_por_un",
    "mimimo_auxiliar_combustivel", "minimo_auxiliar", "muni_cod", "muni_cod_destino",
    "muni_cod_origem", "muni_nome", "muni_origem_nome", "muni_sede_nome", "n", "name",
    "name_muni", "num_caminhoes", "num_empregos", "num_linhas", "num_max_usinas",
    "num_usinas", "numero_usinas", "param_f_disp", "pec_coproduto", "populacao",
    "potencia", "producao", "produto_codigo", "produto_nome", "prop_producao",
    "prop_vendas", "ref", "regiao", "rota", "rowid", "saldo", "saldo_regiao", "sed_cod", "sede_cod",
    "sede_name", "sede_nome", "soma_combustivel_mj_x_valor_por_rota",
    "soma_energetico_x_valor_por_rota", "soma_energia", "soma_energia_mj",
    "soma_energia_mwh", "tab_demanda", "total_soma_combustivel_mj_disponivel",
    "total_soma_energetico_disponivel", "uf", "uf_d", "uf_nome", "uf_o", "uf_origem",
    "uf_sede", "uf_sigla", "valor", "vendas_m3", "alfa_custo_usina", "beta_custo_usina",
    "capex", "comb_gerado_m3", "custo_capex",
    "fator_disponibilidade", "fator_pessoal_x_ute", "inv_aquisicao",
    "inv_capex_alfa", "inv_custo_fixo_usina", "inv_opex_adicional", "inv_receita",
    "inv_sazonalidade", "inv_transporte", "nivel_remuneracao_mensal",
    "num_empregos_rota", "percentual_opex", "setNames", "toneladas_co2_eq",
    "toneladas_co2_eq_por_un", "total", "total_inv_aquisicao", "total_inv_capex",
    "total_inv_capex_alfa", "total_inv_capex_beta", "total_inv_custo_fixo_usina",
    "total_inv_opex", "total_inv_opex_alfa", "total_inv_opex_beta",
    "total_inv_opex_capex", "total_inv_receita", "total_inv_sazonalidade",
    "total_inv_transporte", "total_potencia", "total_usinas", "ute")
)
