#' Calcular inputs do modelo
#'
#' @param con Conexão com o banco de dados do otimizador.
#' @param parametros Tabela de parâmetros.
#' @param regiao_escopo Vetor com siglas de estados brasileiros ou a string
#' "Brasil" para considerar todo o país como regiao escopo.
#' @param populacao_minima População mínima que um município deve possuir
#' para que ele possa ser uma sede.
#' @param rotas Vetor de rotas.
#' @param produtos_selecionados Vetor com o id dos produtos que vão entrar no
#' cálculo do energético.
#' @param backend As manipulações da base devem ser feitas em disco, usando
#' SQL, ou em RAM, usando dplyr? Opções: 'disco' (padrão) ou 'ram'.
#' @param sazonalidade A sazonalidade deve ser considerada na otimização?
#' Opções: TRUE (sim, argumento padrão) ou FALSE (não)
#'
#' @return Uma lista com os inputs de cada rota.
#' @export
calcular_inputs <- function(con, parametros, regiao_escopo,
                            populacao_minima = 10000,
                            rotas = c(
                              "dens", "biocomb", "bioeletr",
                              "bioeletr_pec", "biocomb_pec", "cofiring"
                            ),
                            produtos_selecionados = NULL,
                            backend = c("disco", "ram"),
                            sazonalidade = TRUE) {
  if (length(regiao_escopo) == 1) {
    if (regiao_escopo == "Brasil") {
      regiao_escopo <- otimizadorLinear::estados_br
    } else if (!regiao_escopo %in% otimizadorLinear::estados_br) {
      stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
    }
  } else if (!all(regiao_escopo %in% otimizadorLinear::estados_br)) {
    stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
  }

  # Ordenando rotas para ficar na mesma ordem da matriz de restrições
  rotas_possiveis <- c(
    "dens",
    "biocomb",
    "bioeletr",
    "biocomb_pec",
    "bioeletr_pec",
    "cofiring"
  )

  rotas <- rotas_possiveis[rotas_possiveis %in% rotas]
  rotas <- purrr::set_names(rotas, rotas)

  matriz_distancias <- dplyr::tbl(con, "distancias_BRASIL") |>
    dplyr::filter(uf_o %in% regiao_escopo & uf_d %in% regiao_escopo)

  carga_energia_brasil <- dplyr::tbl(con, "carga_energia_brasil")

  if (populacao_minima > 0) {
    municipios_com_pop <- dplyr::tbl(con, "tabela_pop_muni") |>
      dplyr::filter(populacao > !!populacao_minima) |>
      dplyr::pull(muni_cod)

    matriz_distancias <- matriz_distancias |>
      dplyr::filter(muni_cod_destino %in% !!municipios_com_pop)
  }

  if (backend[1] == "ram") {
    matriz_distancias <- dplyr::collect(matriz_distancias)
    carga_energia_brasil <- dplyr::collect(carga_energia_brasil)
  }

  fatores_sazonalidade <- calcular_fatores_sazonalidade(
    con,
    produtos_selecionados,
    sazonalidade,
    carga_energia_brasil,
    backend = backend[1]
  )

  # 3.774 sec elapsed
  matriz_distancias_saldos <- purrr::map(
    rotas,
    ~ calcular_saldo(
      .rota = .x,
      parametros,
      matriz_distancias,
      carga_energia_brasil,
      produtos_selecionados,
      fatores_sazonalidade
    )
  )

  custo_fixo_usina <- purrr::map(
    rotas,
    ~ calcular_custo_fixo_usina(parametros, .rota = .x)
  )

  limite_tamanho_usina <- purrr::map(
    rotas,
    ~ parametros$limite_tamanho_usina_mw[parametros$rota == .x]
  )

  fator_disponibilidade <- purrr::map(
    rotas,
    ~ parametros$fator_disponibilidade[parametros$rota == .x]
  )

  # Incluindo tabela de demandas
  matriz_distancias_saldos <- purrr::map(
    rotas,
    ~ juntar_tab_demanda(
      con,
      parametros = parametros,
      matriz_distancias_saldos = matriz_distancias_saldos,
      .rota = .x,
      backend = backend[1]
    ) |>
      dplyr::filter(demanda > 0)
  )

  # Computando SQL
  if (backend[1] == "disco") {
    matriz_distancias_saldos <- computar_matriz_distancias_saldos(
      matriz_distancias_saldos
    )
  }

  list(
    matriz_distancias_saldos = matriz_distancias_saldos,
    custo_fixo_usina = custo_fixo_usina,
    limite_tamanho_usina = limite_tamanho_usina,
    fator_disponibilidade = fator_disponibilidade,
    backend = backend[1]
    # demanda = demanda,
    # energetico = energetico,
    # combustivel = combustivel
  )
}


calcular_custo_fixo_usina <- function(parametros, .rota = NULL) {
  if (!is.null(.rota)) {
    parametros <- parametros[parametros$rota == .rota, ]
  }
  mult_valor_presente <- calcular_mult_valor_presente(parametros)
  alfa <- parametros$alfa_custo_usina
  perc_opex <- parametros$percentual_opex

  -alfa * (1 + perc_opex * mult_valor_presente)
}

juntar_tab_demanda <- function(con, parametros, matriz_distancias_saldos, .rota,
                               backend) {
  nome_tabela <- pegar_nome_tab_demanda(.rota, parametros)

  tab <- dplyr::tbl(con, nome_tabela) |>
    dplyr::select(
      muni_cod,
      grupo_demanda,
      demanda,
      dplyr::any_of("tipo_usina")
    )

  if (backend == "ram") {
    tab <- dplyr::collect(tab)
  }

  if (.rota == "cofiring") {
    tab <- ajustar_demanda_cofiring(tab, parametros)
  }

  matriz_distancias_saldos[[.rota]] |>
    dplyr::left_join(
      tab,
      by = c("muni_cod_destino" = "muni_cod")
    )
}

ajustar_demanda_cofiring <- function(tab, parametros) {
  porc_demanda_cimenteira <- parametros |>
    dplyr::filter(rota == "cofiring") |>
    dplyr::pull(porc_demanda_cimenteira)

  porc_demanda_siderurgica <- parametros |>
    dplyr::filter(rota == "cofiring") |>
    dplyr::pull(porc_demanda_siderurgica)

  porc_demanda_termeletrica <- parametros |>
    dplyr::filter(rota == "cofiring") |>
    dplyr::pull(porc_demanda_termeletrica)

  tab |>
    dplyr::mutate(
      demanda = dplyr::case_when(
        tipo_usina == "cimenteira" ~ demanda * porc_demanda_cimenteira,
        tipo_usina == "termeletrica" ~ demanda * porc_demanda_termeletrica,
        tipo_usina == "siderurgica" ~ demanda * porc_demanda_siderurgica
      )
    )
}
