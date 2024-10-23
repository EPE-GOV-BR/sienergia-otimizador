# testando com num_c alto
# objetivos: descobrir qual a maior distancia que o energetico viaja
#            descobrir qual o "melhor" num_c

# Carregar o pacote
devtools::load_all()

# Definir o estado de escopo
regiao_escopo <- "Brasil"

# Conectar com o SQLite baixado
con <- DBI::dbConnect(RSQLite::SQLite(), "SIEnergia_dados.sqlite")

# Buscar a tabela de parâmetros
parametros <- dplyr::tbl(con, "exemplo_parametros") |>
  dplyr::collect()

# Rodando com parametros default

# Definindo rotas
rotas <- c("dens",
           "biocomb",
           "bioeletr",
           "bioeletr_pec",
           "biocomb_pec",
           "cofiring")

produtos_selecionados <- dplyr::tbl(con, "carga_energia_brasil") |>
  dplyr::distinct(produto_codigo) |>
  dplyr::pull()

  # Calcular os inputs do modelo
  tictoc::tic()
  inputs <- calcular_inputs(
    con,
    parametros,
    regiao_escopo,
    populacao_minima = 100000,
    rotas,
    produtos_selecionados,
    backend = "ram",
    sazonalidade = TRUE
  )
  tictoc::toc()

  distancias_num_c <- purrr::map(
    inputs$matriz_distancias_saldos,
    ~ .x |>
      dplyr::group_by(muni_cod_origem) |>
      dplyr::arrange(desc(saldo)) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::select(muni_cod_origem, muni_cod_destino, distancia, saldo, id)
  )

  # Executar o otimizador
  tictoc::tic()
  res_bruto <- otimizar(
    inputs,
    solver = "gurobi",
    num_c = 250,
    limitar_tamanho_usina = TRUE,
    num_max_usinas_sede = 50,
    limitar_por_demanda = TRUE,
    TimeLimit = 500
  )
  tictoc::toc()

  # Arrumar a solução do otimizador
  res <- arrumar_solucao(res_bruto)

  tab_final <- purrr::map2(
    distancias_num_c,
    res$tab_solucao,
    function(x, y){
      solucao <- y |>
        dplyr::select(muni_cod_origem, muni_cod_destino = sede_cod)

      tabelas_juntas <- solucao |>
        dplyr::left_join(x)

      if(nrow(tabelas_juntas) > 0){
        tabelas_juntas |>
          dplyr::summarise(
            num_c_max = max(id),
            dist_max = max(distancia)
          )
      } else {
        NULL
      }

    }
  )

  readr::write_rds(x = tab_final, file = "data-raw/resultado_dist_num_c_max.rds")
