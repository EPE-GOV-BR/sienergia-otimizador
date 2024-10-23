preparar_tabelas_resultados <- function(con, resultado, parametros, regiao_escopo, fatores) {

  rotas <- names(resultado$tab_solucao)
  rotas <- stats::setNames(rotas, rotas)


  if (length(regiao_escopo) == 1) {
    if (regiao_escopo == "Brasil") {
      regiao_escopo <- otimizadorLinear::estados_br
    } else if (!regiao_escopo %in% otimizadorLinear::estados_br) {
      stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
    }
  } else if (!all(regiao_escopo %in% otimizadorLinear::estados_br)) {
    stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
  }

  municipios_escopo <- pegar_muni_uf(con, regiao_escopo)

  demanda_total <- purrr::map(
    rotas,
    ~ dplyr::tbl(con, pegar_nome_tab_demanda(.x, parametros)) |>
      dplyr::filter(muni_cod %in% municipios_escopo) |>
      dplyr::distinct(grupo_demanda, demanda) |>
      dplyr::summarise(demanda_total = sum(demanda, na.rm = TRUE)) |>
      dplyr::collect()
  )

  carga_energetico <- purrr::map(
    rotas,
    ~ dplyr::tbl(con, "carga_energia_brasil") |>
      dplyr::filter(muni_cod %in% municipios_escopo) |>
      arrumar_tab_energetico(.x)
  )

  energetico_total <- purrr::map(
    carga_energetico,
    ~ .x |>
      dplyr::summarise(energetico_total = sum(energetico, na.rm = TRUE)) |>
      dplyr::collect()
  )

  combustivel_total <- purrr::map2(
    rotas,
    carga_energetico,
    ~ .y |>
      dplyr::select(combustivel_mj = glue::glue("{.x}_combustivel_mj")) |>
      dplyr::summarise(combustivel_mj_total = sum(combustivel_mj, na.rm = TRUE)) |>
      dplyr::collect()
  )

  combustivel_aproveitado <- calcular_porc_combustivel_aproveitado(
    resultado,
    combustivel_total
  )

  biomassa_aproveitada <- calcular_porc_biomassa_aproveitada(
    resultado,
    energetico_total
  )

  demanda_satisfeita <- calcular_porc_demanda_satisfeita(
    resultado,
    demanda_total
  )

  tab_muni <- dplyr::tbl(con, "municipios")

  eficiencia_usina <- parametros |>
    dplyr::select(rota, efic_usina)

  tab_solucao_com_muni <- resultado$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::left_join(tab_muni,
      by = c("muni_cod_origem" = "muni_cod"),
      copy = TRUE
    ) |>
    dplyr::rename(
      muni_origem_nome = muni_nome,
      uf_origem = uf
    ) |>
    dplyr::left_join(tab_muni,
      by = c("sede_cod" = "muni_cod"),
      copy = TRUE
    ) |>
    dplyr::rename(
      muni_sede_nome = muni_nome,
      uf_sede = uf
    ) |>
    dplyr::left_join(eficiencia_usina, by = "rota")

  suppressWarnings({
    if (is.null(tab_solucao_com_muni$combustivel_m3)) {
      tab_solucao_com_muni <- tab_solucao_com_muni |>
        dplyr::mutate(combustivel_m3 = NA_integer_)
    }
  })

  tabela_municipios_fornecedores <- gerar_tabela_municipios_fornecedores(
    tab_solucao_com_muni
  )

  demanda_satisfeita_grupo <- gerar_tabela_demanda_satisfeita(
    tab_solucao_com_muni
  )

  energia_ofertada <- calcular_porc_energia_ofertada(
    tab_solucao_com_muni
  )

  resultados_potencia <- preparar_tabela_resultados_potencia(
    tab_solucao_com_muni,
    parametros
  )

  tabela_lucros <- preparar_tabela_lucros(resultado, parametros)

  tabela_investimentos <- preparar_tabela_investimentos(resultado, parametros, rotas)

  tabela_regiao <- criar_tab_por_regiao(resultado, con)

  total_km_rodados <- calcular_total_km_rodados(resultado, fator_ida_volta = 2)

  litros_combustivel_consumido <- calcular_litros_combustivel_consumido(
    resultado,
    capacidade_caminhao_ton = 30,
    litros_diesel_por_km = 2,
    fator_ida_volta = 2
  )

  producao_rota <- calcula_producao_rota(resultado, parametros)

  agua_emissoes_empregos <- calcula_agua_emissoes_empregos(resultado,
                                 con,
                                 producao_rota,
                                 resultados_potencia,
                                 parametros,
                                 fatores$fator_consumo_mwh,
                                 fatores$fator_consumo_m3,
                                 fatores$fator_consumo_ton,
                                 fatores$fator_emissoes_mwh,
                                 fatores$fator_emissoes_m3,
                                 fatores$fator_emissoes_ton,
                                 fatores$fator_empregabilidade_mwh,
                                 fatores$fator_empregabilidade_m3,
                                 fatores$fator_empregabilidade_ton,
                                 fatores$fator_pessoal)

  list(
    biomassa_aproveitada = biomassa_aproveitada,
    demanda_satisfeita = demanda_satisfeita,
    combustivel_aproveitado = combustivel_aproveitado,
    tabela_municipios_fornecedores = tabela_municipios_fornecedores,
    resultados_potencia = resultados_potencia,
    demanda_satisfeita_grupo = demanda_satisfeita_grupo,
    energia_ofertada = energia_ofertada,
    tabela_lucros = tabela_lucros,
    tabela_investimentos = tabela_investimentos,
    tabela_regiao = tabela_regiao,
    total_km_rodados = total_km_rodados,
    litros_combustivel_consumido = litros_combustivel_consumido,
    agua_emissoes_empregos = agua_emissoes_empregos
  )
}



preparar_tabela_resultados_potencia <- function(tab_solucao_com_muni,
                                                parametros) {

  fator_disponibilidade <- parametros |>
    dplyr::select(rota, fator_disponibilidade)

  tabela_soma_energia <- tab_solucao_com_muni |>
    dplyr::group_by(rota, muni_sede_nome, uf_sede) |>
    dplyr::summarise(
      soma_energia_mj = sum(energia_mj * valor),
      numero_municipios = dplyr::n(),
      num_usinas = num_usinas[1],
      comb_gerado_m3 = sum(combustivel_m3*valor),
      comb_gerado_ton = sum(energetico*valor)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(fator_disponibilidade, by = dplyr::join_by(rota))

  tabela_soma_energia |>
    dplyr::mutate(
      soma_energia_mwh = soma_energia_mj * 0.00027777777777778
    ) |>
    dplyr::mutate(
      potencia = otimizadorLinear::calcular_potencia(
        energia = soma_energia_mwh,
        fator_disponibilidade
      ),
      potencia = round(potencia, 2),
      .before = comb_gerado_m3
    ) |>
    dplyr::arrange(dplyr::desc(potencia)) |>
    dplyr::mutate(
      comb_gerado_m3 = dplyr::case_when(
        rota %in% c("dens", "bioeletr", "bioeletr_pec", "cofiring") ~ NA,
        TRUE ~ comb_gerado_m3
      ),
      comb_gerado_ton = dplyr::case_when(
        rota %in% c("dens", "bioeletr", "bioeletr_pec", "biocomb", "biocomb_pec") ~ NA,
        TRUE ~ comb_gerado_ton
      ),
      potencia = dplyr::case_when(
        rota %in% c("biocomb", "biocomb_pec", "cofiring") ~ NA,
        TRUE ~ potencia
      )
    ) |>
    dplyr::select(-soma_energia_mj, -soma_energia_mwh, -fator_disponibilidade)
}

calcular_porc_combustivel_aproveitado <- function(resultado, combustivel_total) {
  tab_combustivel_total <- purrr::list_rbind(
    combustivel_total,
    names_to = "rota"
  )
  resultado$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::left_join(tab_combustivel_total, by = "rota") |>
    dplyr::select(rota, muni_cod_origem, combustivel_mj, combustivel_mj_total, valor) |>
    dplyr::mutate(combustivel_mj_x_valor = valor * combustivel_mj) |>
    dplyr::group_by(rota) |>
    dplyr::summarise(
      soma_combustivel_mj_x_valor_por_rota = sum(combustivel_mj_x_valor),
      total_soma_combustivel_mj_disponivel = combustivel_mj_total[1]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      porc_combustivel_aproveitado = soma_combustivel_mj_x_valor_por_rota /
        total_soma_combustivel_mj_disponivel
    )
}

calcular_porc_biomassa_aproveitada <- function(resultado, energetico_total) {
  tab_energetico_total <- purrr::list_rbind(
    energetico_total,
    names_to = "rota"
  )
  resultado$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::left_join(tab_energetico_total, by = "rota") |>
    dplyr::select(rota, muni_cod_origem, energetico, energetico_total, valor) |>
    dplyr::mutate(energetico_x_valor = valor * energetico) |>
    dplyr::group_by(rota) |>
    dplyr::summarise(
      soma_energetico_x_valor_por_rota = sum(energetico_x_valor),
      total_soma_energetico_disponivel = energetico_total[1]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      porc_biomassa_aproveitada = soma_energetico_x_valor_por_rota /
        total_soma_energetico_disponivel
    )
}

gerar_tabela_municipios_fornecedores <- function(tab_solucao_com_muni) {

  tab_muni_fornecedor <- tab_solucao_com_muni |>
    dplyr::group_by(muni_origem_nome, muni_cod_origem, uf_origem, muni_sede_nome, rota, grupo_demanda) |>
    dplyr::summarise(
      # energia_consumida_mj = sum(energia_mj),
      biomassa_disponivel = sum(energetico),
      biomassa_aproveitada = sum(valor * energetico),
      porc_energetico_aproveitado = biomassa_aproveitada / biomassa_disponivel,
      # energia_primaria_disponivel = dplyr::case_when(
      #   rota == "dens" ~ sum(efic_usina * combustivel_mj * valor),
      #   rota == "biocomb" ~ sum(combustivel_mj * valor)
      # ),
      combustivel_comercializado = sum(combustivel_m3 * valor)
      # incluir o numero de muni atendidos / abastecidos
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(muni_origem_nome) |>
    # Só faz sentido apresentar a distribuidora quando a rota gera
    # energia elétrica
    dplyr::mutate(
      distribuidora_energia_eletrica = dplyr::case_when(
        rota %in% c("dens", "bioeletr", "bioeletr_pec") ~ grupo_demanda,
        TRUE ~ "-"
      )
    )

  tab_muni_fornecedor
}

calcular_porc_energia_ofertada <- function(tab_solucao_com_muni) {
  tab_solucao_com_muni |>
    dplyr::mutate(energia_ofertada = valor * energia_mj) |>
    dplyr::group_by(rota) |>
    dplyr::summarise(soma_energia_ofertada = sum(energia_ofertada))
}

gerar_tabela_demanda_satisfeita <- function(tab_solucao_com_muni) {
  demanda_por_grupo <- tab_solucao_com_muni |>
    dplyr::distinct(rota, grupo_demanda, demanda_existente = demanda)

  tab_solucao_com_muni |>
    dplyr::group_by(rota, grupo_demanda) |>
    dplyr::summarise(demanda_satisfeita = sum(energia_mj * valor)) |>
    dplyr::left_join(demanda_por_grupo, by = c("rota", "grupo_demanda")) |>
    dplyr::mutate(porc_demanda_satisfeita = demanda_satisfeita / demanda_existente) |>
    dplyr::ungroup()
}

#' calcula a porcentagem da demanda satisfeita
#'
#' @param resultado saída da função 'arrumar_solucao'
#' @param demanda_total tabela com a demanda total de cada rota
#'
#' @export
calcular_porc_demanda_satisfeita <- function(resultado, demanda_total) {
  tab_demanda_total <- purrr::list_rbind(
    demanda_total,
    names_to = "rota"
  )
  resultado$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::left_join(tab_demanda_total, by = "rota") |>
    dplyr::group_by(rota) |>
    dplyr::summarise(porc_demanda_satisfeita = sum(energia_mj * valor) / demanda_total[1])
}

preparar_tabela_lucros <- function(resultado, parametros) {
  inputs <- resultado$inputs

  fator_disponibilidade <- setNames(as.list(parametros$fator_disponibilidade), nm = parametros$rota)

  purrr::map2(
    resultado$tab_solucao,
    names(resultado$tab_solucao),
    function(x, y) {
      x |>
        dplyr::group_by(sede_cod) |>
        dplyr::summarise(
          numero_usinas = num_usinas[1],
          energetico = sum(energetico * valor),
          ofertantes_quais = paste0(muni_cod_origem, collapse = ", "),
          ofertantes = dplyr::n(),
          vpl = sum(saldo * valor) + inputs$custo_fixo_usina[[y]] * num_usinas[1],
          energia = sum(valor * energia_mj),
          potencia = energia / (fator_disponibilidade[[y]] * 3600 * 365 * 24),
          valor_medio = mean(valor)
        ) |>
        dplyr::filter(numero_usinas > 0)# |>
        #with(sum(vpl))
    }
  )
}

criar_tab_por_regiao <- function(res, con) {
  depara_regioes <- dplyr::tbl(con, "carga_energia_brasil") |>
    dplyr::select(muni_cod, uf) |>
    dplyr::mutate(
      regiao = dplyr::case_when(
        uf %in% c("AM", "RR", "AP", "PA", "TO", "RO", "AC") ~ "Norte",
        uf %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA") ~ "Nordeste",
        uf %in% c("PR", "RS", "SC") ~ "Sul",
        uf %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
        uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
        muni_cod %in% c(1504752) ~ "Norte",
        muni_cod %in% c(4212650, 4220000, 4314548, 4316733) ~ "Sul",
        muni_cod %in% c(5006275, 5208152) ~ "Centro-Oeste"
      )
    ) |>
    dplyr::distinct() |>
    dplyr::rename(sede_cod = muni_cod) |>
    dplyr::collect()

  energetico_aproveitado_regiao <- res$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::left_join(depara_regioes, by = c("muni_cod_origem" = "sede_cod")) |>
    dplyr::select(muni_cod_origem, energetico, regiao, valor) |>
    dplyr::group_by(regiao) |>
    dplyr::summarise(energetico_aproveitado = sum(valor * energetico))

  energia_eletrica_produzida <- res$tab_solucao |>
    purrr::list_rbind(names_to = "rota") |>
    dplyr::filter(rota %in% c("dens", "bioeletr", "bioeletr_pec")) |>
    dplyr::left_join(depara_regioes, by = dplyr::join_by(sede_cod)) |>
    dplyr::select(muni_cod_origem, energia_mj, valor, regiao) |>
    dplyr::group_by(regiao) |>
    dplyr::summarise(energia_eletrica_produzida = sum(valor * energia_mj))

  energetico_aproveitado_regiao |>
    dplyr::left_join(energia_eletrica_produzida, by = dplyr::join_by(regiao))
}

# gera_tab_regioes <- function(con, inputs, tab_resultado) {
#   depara_regioes <- dplyr::tbl(con, "carga_energia_brasil") |>
#     dplyr::select(uf, muni_cod) |>
#     dplyr::mutate(
#       regiao = dplyr::case_when(
#         uf %in% c("AM", "RR", "AP", "PA", "TO", "RO", "AC") ~ "Norte",
#         uf %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA") ~ "Nordeste",
#         uf %in% c("PR", "RS", "SC") ~ "Sul",
#         uf %in% c("SP", "RJ", "ES", "MG") ~ "Sudeste",
#         uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
#         muni_cod %in% c(1504752) ~ "Norte",
#         muni_cod %in% c(4212650, 4220000, 4314548, 4316733) ~ "Sul",
#         muni_cod %in% c(5006275, 5208152) ~ "Centro-Oeste"
#       )
#     ) |>
#     dplyr::collect()

#   energetico_produzido_por_regiao <- inputs$matriz_lucros$dens |>
#     dplyr::select(muni_cod = muni_cod_origem, energetico) |>
#     dplyr::distinct() |>
#     dplyr::collect() |>
#     dplyr::left_join(depara_regioes, by = dplyr::join_by(muni_cod)) |>
#     dplyr::distinct() |>
#     dplyr::group_by(regiao) |>
#     dplyr::summarise(energetico_produzido = sum(energetico))

#   demanda_energia_por_regiao <- inputs$matriz_lucros$dens |>
#     dplyr::select(muni_cod = muni_cod_destino, demanda, grupo_demanda) |>
#     dplyr::distinct() |>
#     dplyr::left_join(depara_regioes, by = dplyr::join_by(muni_cod)) |>
#     tidyr::drop_na() |>
#     dplyr::select(demanda, grupo_demanda, regiao) |>
#     dplyr::distinct() |>
#     dplyr::group_by(grupo_demanda) |>
#     dplyr::mutate(n = dplyr::n()) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(demanda_real = demanda / n) |>
#     dplyr::group_by(regiao) |>
#     dplyr::summarise(demanda_energia = sum(demanda_real))

#   tab_resultado$tabela_regiao |>
#     dplyr::left_join(energetico_produzido_por_regiao, by = dplyr::join_by(regiao)) |>
#     dplyr::left_join(demanda_energia_por_regiao, by = dplyr::join_by(regiao)) |>
#     dplyr::mutate(
#       porc_energia_produzida = energia_eletrica_produzida / demanda_energia,
#       porc_energetico_aproveitado = energetico_aproveitado / energetico_produzido
#     ) |>
#     dplyr::select(
#       regiao, demanda_energia, energia_eletrica_produzida, porc_energia_produzida,
#       energetico_produzido, energetico_aproveitado, porc_energetico_aproveitado
#     )
# }

calcular_total_km_rodados <- function(resultado, fator_ida_volta = 2) {
  tab <- purrr::map2_dfr(
    resultado$tab_solucao,
    names(resultado$tab_solucao),
    function(x, y) {
      x |>
        dplyr::summarise(
          total_km_rodados = fator_ida_volta * sum(distancia),
          total_energetico = sum(energetico)
        ) |>
        dplyr::mutate(rota = y)
    }
  )

  tab |>
    dplyr::bind_rows(
      data.frame(
        total_km_rodados = sum(tab$total_km_rodados),
        total_energetico = sum(tab$total_energetico),
        rota = "total"
      )
    )
}

calcular_litros_combustivel_consumido <- function(resultado,
                                                 capacidade_caminhao_ton,
                                                 litros_diesel_por_km,
                                                 fator_ida_volta = 2) {
  tab <- purrr::map2_dfr(
    resultado$tab_solucao,
    names(resultado$tab_solucao),
    function(x, y) {
      x |>
        dplyr::mutate(
          num_caminhoes = ceiling(energetico / capacidade_caminhao_ton),
          litros_diesel = num_caminhoes * fator_ida_volta * distancia * litros_diesel_por_km
        ) |>
        dplyr::summarise(total_litros_diesel_consumidos = sum(litros_diesel)) |>
        dplyr::mutate(rota = y)
    }
  )

  tab |>
    dplyr::bind_rows(
      data.frame(
        total_litros_diesel_consumidos = sum(tab$total_litros_diesel_consumidos),
        rota = "total"
      )
    )
}

calcula_producao_rota <- function(resultado,
                                  parametros){

  # rotas possiveis
  # rotas <- unique(parametros$rota)

  # rotas presentes no resultado
  rotas <- names(resultado$tab_solucao) |> unique()

  rotas_eletricas <- rotas[rotas %in% c("dens", "bioeletr", "bioeletr_pec")]
  rotas_combustivel <- rotas[rotas %in% c("biocomb", "biocomb_pec")]
  rotas_cofiring <- rotas[rotas %in% c("cofiring")]

  rotas_eletricas <- stats::setNames(rotas_eletricas, rotas_eletricas)
  rotas_combustivel <- stats::setNames(rotas_combustivel, rotas_combustivel)
  rotas_cofiring <- stats::setNames(rotas_cofiring, rotas_cofiring)

  tipo <-  list(
    rotas_eletricas = rotas_eletricas,
    rotas_combustivel = rotas_combustivel,
    rotas_cofiring = rotas_cofiring
  ) |>
    purrr::compact()

  list(
  producao = purrr::map(
    tipo,
      ~purrr::map(
        .x,
        function(x){
          efic = parametros |>
            dplyr::filter(rota == x) |>
            dplyr::pull(efic_usina)
          if (x %in% rotas_eletricas){
            resultado$tab_solucao[[x]] |>
              dplyr::mutate(mwh = energia_mj*valor*efic/3600) |>
              dplyr::summarise(producao_mwh = sum(mwh))
          } else if (x %in% rotas_combustivel){
            resultado$tab_solucao[[x]] |>
              dplyr::mutate(m3 = combustivel_m3*valor) |>
              dplyr::summarise(producao_m3 = sum(m3))
          } else if (x %in% rotas_cofiring){
            resultado$tab_solucao[[x]] |>
              dplyr::mutate(ton = energetico*valor) |>
              dplyr::summarise(producao_ton = sum(ton))
          }
        }
      ) |>
      purrr::list_rbind(names_to = "rota")
  ),
  tipo_rota = tipo
  )
}

calcula_agua_emissoes_empregos <- function(resultado,
                                 con = con,
                                 producao_rota,
                                 resultados_potencia,
                                 parametros,
                                 fator_consumo_mwh = 2.000000e+00,
                                 fator_consumo_m3 = 1.507866e-05,
                                 fator_consumo_ton = 0,
                                 fator_emissoes_mwh = 1.879909e-08,
                                 fator_emissoes_m3 = 1.417326e-13,
                                 fator_emissoes_ton = 0,
                                 fator_empregabilidade_mwh = 1.341237e-02,
                                 fator_empregabilidade_m3 = 1.011203e-07,
                                 fator_empregabilidade_ton = 0,
                                 fator_pessoal = 0.8){

  dados_capex <- resultados_potencia |>
    dplyr::left_join(dplyr::select(parametros, rota, efic_usina, fator_disponibilidade)) |>
    dplyr::mutate(potencia = ifelse(is.na(potencia), (comb_gerado_m3 * 0.00307032 * efic_usina)/(365*24*fator_disponibilidade), potencia)) |>
    dplyr::group_by(rota) |>
    dplyr::summarise(total_usinas = sum(num_usinas),
                     total_potencia = sum(potencia)) |>
    dplyr::left_join(
      parametros |>
        dplyr::select(rota, alfa_custo_usina, beta_custo_usina, percentual_opex),
      by = dplyr::join_by(rota)
    ) |>
    dplyr::mutate(
      capex = alfa_custo_usina*total_usinas + beta_custo_usina*total_potencia,
      ute = capex*percentual_opex,
      fator_pessoal = fator_pessoal,
      fator_pessoal_x_ute = ute*fator_pessoal
    ) |>
    dplyr::select(rota, fator_pessoal_x_ute)

  dados <- purrr::map2(
    producao_rota$producao,
    producao_rota$tipo_rota,
    function(x, y){
      if (all(y %in% c("dens", "bioeletr", "bioeletr_pec"))){
        fator_consumo <- fator_consumo_mwh
        fator_emissoes <- fator_emissoes_mwh
        fator_empregabilidade <- fator_empregabilidade_mwh
      } else if (all(y %in% c("biocomb", "biocomb_pec"))){
        fator_consumo <- fator_consumo_m3
        fator_emissoes <- fator_emissoes_m3
        fator_empregabilidade <- fator_empregabilidade_m3
      } else if (all(y %in% c("cofiring"))){
        fator_consumo <- fator_consumo_ton
        fator_emissoes <- fator_emissoes_ton
        fator_empregabilidade <- fator_empregabilidade_ton
      }

      x |>
        dplyr::mutate(
          fator_consumo = fator_consumo,
          fator_emissoes = fator_emissoes,
          fator_empregabilidade = fator_empregabilidade
        )
    }
  )

  purrr::map(
    dados,
    function(a, dados_capex){
      producao_por_rota <- a |>
        dplyr::select(dplyr::starts_with("producao")) |>
        dplyr::pull()
      producao_por_rota <- stats::setNames(producao_por_rota, a$rota) |> as.list()

      fator_consumo <-  unique(a$fator_consumo)
      fator_emissoes <- unique(a$fator_emissoes)
      fator_empregabilidade <- unique(a$fator_empregabilidade)

      purrr::map(
        producao_por_rota,
        function(x, fator_consumo, fator_emissoes, fator_empregabilidade){

          data.frame(
            fonte = "rota",
            producao = x,
            consumo_agua_m3_por_un = fator_consumo,
            toneladas_co2_eq_por_un = fator_emissoes,
            empregos_por_un = fator_empregabilidade
          ) |> dplyr::mutate(
            consumo_agua_m3 = x*consumo_agua_m3_por_un,
            toneladas_co2_eq = x*toneladas_co2_eq_por_un,
            num_empregos = x*empregos_por_un
          ) |>
            dplyr::select(
              fonte_energia = fonte,
              producao,
              consumo_agua_m3,
              toneladas_co2_eq,
              num_empregos
            ) |>
            tidyr::pivot_wider(names_from = fonte_energia,
                               values_from = c(consumo_agua_m3,
                                               toneladas_co2_eq,
                                               num_empregos))
        },
        fator_consumo,
        fator_emissoes,
        fator_empregabilidade
      ) |>
        purrr::list_rbind(names_to = "rota") |>
        dplyr::left_join(dados_capex, by = dplyr::join_by(rota)) |>
        dplyr::mutate(
          nivel_remuneracao_mensal = fator_pessoal_x_ute/(num_empregos_rota*12),
          nivel_remuneracao_mensal = dplyr::if_else(nivel_remuneracao_mensal > 20000, 20000, nivel_remuneracao_mensal)
        ) |>
        dplyr::select(-fator_pessoal_x_ute)
    },
    dados_capex
  )

}

preparar_tabela_investimentos <- function(resultado, parametros, rotas){

  tab_inv <- purrr::map(
    rotas,
    ~calcula_investimentos(
      resultado,
      parametros,
      .x
    ),
    resultado,
    parametros
  ) |>
    purrr::list_rbind(names_to = "rota")

  tab_inv |>
    dplyr::mutate(
      total_inv_opex = total_inv_opex_alfa + total_inv_opex_beta,
      total_inv_capex = total_inv_capex_alfa + total_inv_capex_beta
    ) |>
    dplyr::select(
      rota, total_inv_receita, total_inv_opex, total_inv_aquisicao,
      total_inv_transporte, total_inv_sazonalidade, total_inv_capex
    )

}

calcula_investimentos <- function(resultado, parametros, .rota){

  parametros_r <- parametros |>
    dplyr::filter(rota == .rota)

  custo_aquisicao <- calcular_custo_aquisicao(parametros_r) # $/t
  multiplicador_vp <- calcular_mult_valor_presente(parametros_r) # adimensional
  capex_opex_adic_por_mj <- calcular_custo_por_mj(parametros_r) # $/MJ
  custo_armazenamento <- parametros_r$custo_armazenamento_por_t
  p_opex <- parametros_r$percentual_opex
  alfa <- parametros_r$alfa_custo_usina
  custo_fixo_usina <- resultado$inputs$custo_fixo_usina[[.rota]]

  eficiencia_usina <- parametros_r$efic_usina # adimensional
  receita <- parametros_r$receita # $/MWh ou $/m3 ou $/ton

  tab <- resultado$tab_solucao[[.rota]] |>
    dplyr::mutate(
      custo_transporte = dplyr::case_when(
        distancia_aux == 0 ~ 0,
        TRUE ~ !!parametros_r$custo_fixo_transporte +
          !!parametros_r$custo_transporte_por_t_km * distancia_aux
      )
    )

  if (parametros_r$unidade_receita == "mwh") {
    tab_saldo <- tab |>
      dplyr::mutate(
        inv_receita = receita * combustivel_mj / 3600 * eficiencia_usina * multiplicador_vp * valor,
        inv_opex_adicional = capex_opex_adic_por_mj * combustivel_mj * eficiencia_usina * valor,
        inv_aquisicao = custo_aquisicao * energetico * multiplicador_vp * valor,
        inv_transporte = custo_transporte * energetico * multiplicador_vp * valor,
        inv_sazonalidade = energetico * fator_proporcional * custo_armazenamento * multiplicador_vp * valor
      )
  } else if (parametros_r$unidade_receita == "m3") {
    tab_saldo <- tab |>
      dplyr::mutate(
        inv_receita = receita * combustivel_m3 * multiplicador_vp * valor,
        inv_opex_adicional = capex_opex_adic_por_mj * combustivel_mj * eficiencia_usina * valor,
        inv_aquisicao = custo_aquisicao * energetico * multiplicador_vp * valor,
        inv_transporte = custo_transporte * energetico * multiplicador_vp * valor,
        inv_sazonalidade = energetico * fator_combustivel_proporcional * custo_armazenamento * multiplicador_vp * valor
      )
  } else if (parametros_r$unidade_receita == "ton") {
    tab_saldo <- tab |>
      dplyr::mutate(
        inv_receita = receita * energetico * multiplicador_vp * valor,
        inv_opex_adicional = 0,
        inv_aquisicao = custo_aquisicao * energetico * multiplicador_vp * valor,
        inv_transporte = custo_transporte * energetico * multiplicador_vp * valor,
        inv_sazonalidade = energetico * fator_proporcional * custo_armazenamento * multiplicador_vp * valor
      )
  }

  tab_inv <- tab_saldo |>
    dplyr::summarise(
      total_saldo = sum(saldo),
      total_inv_receita = sum(inv_receita),
      total_inv_opex_capex = sum(-inv_opex_adicional),
      total_inv_aquisicao = sum(-inv_aquisicao),
      total_inv_transporte = sum(-inv_transporte),
      total_inv_sazonalidade = sum(-inv_sazonalidade)
    ) |>
    dplyr::mutate(
      total_inv_opex_beta = (total_inv_opex_capex/(1+multiplicador_vp*p_opex))*multiplicador_vp*p_opex,
      total_inv_capex_beta =  total_inv_opex_capex/(1+multiplicador_vp*p_opex)
    )

  tab_capex <- tab_saldo |>
    dplyr::distinct(num_usinas, sede_cod, .keep_all = TRUE) |>
    dplyr::mutate(
      inv_custo_fixo_usina = custo_fixo_usina*num_usinas,
      inv_capex_alfa = num_usinas*alfa
    ) |>
    dplyr::summarise(
      total_inv_custo_fixo_usina = sum(inv_custo_fixo_usina),
      total_inv_capex_alfa = sum(-inv_capex_alfa)
    ) |>
    dplyr::mutate(
      total_inv_opex_alfa = total_inv_custo_fixo_usina - total_inv_capex_alfa
    )

  dplyr::bind_cols(tab_inv, tab_capex)

}
