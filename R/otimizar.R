#' Otimizador linear
#'
#' @param inputs Tabela de parâmetros.
#' @param solver Otimizador a ser utilizado.
#' @param num_c Número de municípios a serem considerados como fornecedores de
#' cada sede.
#' @param limitar_tamanho_usina Se \code{TRUE}, limita o tamanho das usinas.
#' @param num_max_usinas_sede Número máximo de usinas por sede.
#' @param limitar_por_demanda Se \code{TRUE}, limita a energia produzida pela
#' demanda.
#' @param ... Argumentos opcionais para as funções de otimização, a depender do
#' Otimizador a ser utilizado.
#'
#' @export
#'
otimizar <- function(inputs,
                     solver = c("cbc", "gurobi"),
                     num_c = Inf,
                     limitar_tamanho_usina = TRUE,
                     num_max_usinas_sede = 3,
                     limitar_por_demanda = TRUE,
                     ...) {
  rotas <- names(inputs$matriz_distancias_saldos)
  rotas <- purrr::set_names(rotas, rotas)

  sedes_viaveis <- purrr::map(
    rotas,
    ~ listar_usinas_viaveis(
      purrr::pluck(inputs$matriz_distancias_saldos, .x),
      purrr::pluck(inputs$custo_fixo_usina, .x)
    )
  )

  # Devolve solução nula caso não existam sedes viáveis
  if (all(purrr::map_lgl(sedes_viaveis, ~ length(.x) == 0))) {
    return(list(
      matriz_lucros = inputs$matriz_lucros,
      solucao = rep(
        0,
        sum(purrr::map_dbl(inputs$matriz_distancias_saldos, calcular_num_linhas))
      )
    ))
  }

  # Eliminando rotas sem sedes viáveis
  for (r in rotas) {
    if (length(sedes_viaveis[[r]]) == 0) {
      inputs$matriz_distancias_saldos[[r]] <- NULL
    }
  }

  rotas <- rotas[rotas %in% names(inputs$matriz_distancias_saldos)]

  # Pegando apenas sedes viáveis
  inputs$matriz_distancias_saldos <- purrr::map(
    rotas,
    ~ inputs$matriz_distancias_saldos[[.x]] |>
      dplyr::filter(
        muni_cod_destino %in% !!sedes_viaveis[[.x]],
        saldo > 0
      ) |>
      dplyr::arrange(
        muni_cod_destino,
        muni_cod_origem
      )
  )

  if (inputs$backend == "disco") {
    inputs$matriz_distancias_saldos <- computar_matriz_distancias_saldos(
      inputs$matriz_distancias_saldos
    )
  }

  # Para cada rota, gerando vetor com os índices dos melhores
  # municípios de cada possível sede
  indices <- purrr::map(
    rotas,
    ~ gerar_indices(inputs$matriz_distancias_saldos[[.x]], num_c = num_c)
  )

  # Calculando vetor_c concatenado para todas as rotas
  vetor_c <- gerar_vetor_c(inputs, indices, rotas)

  # Calculando vetor de restições
  vetor_restricoes <- gerar_vetor_restricoes(
    inputs,
    limitar_tamanho_usina,
    limitar_por_demanda,
    rotas
  )

  # Calculando matriz de restrições
  matriz_restricoes <- gerar_matriz_restricoes(
    inputs,
    indices,
    limitar_tamanho_usina,
    limitar_por_demanda,
    rotas
  )

  # Gerando limites superiores das variáveis de decisão
  limite_superior_vars_decisao <- gerar_limite_superior_vars_decisao(
    inputs,
    indices,
    num_max_usinas_sede,
    rotas
  )

  # Vetor indicador de números inteiras
  indicador_de_inteiros <- gerar_indicador_inteiros(rotas, indices, inputs)

  # Rodando otimizador
  if (solver[1] == "cbc") {
    if (!verificar_instalacao("rcbc")) {
      stop(pegar_msg_erro("cbc"))
    }

    solucao <- rcbc::cbc_solve(
      max = TRUE,
      is_integer = indicador_de_inteiros,
      row_ub = vetor_restricoes,
      col_ub = limite_superior_vars_decisao,
      col_lb = rep(0, ncol(matriz_restricoes)),
      obj = vetor_c,
      mat = matriz_restricoes,
      ...
    )

    list(
      inputs = inputs,
      matriz_distancias_saldos = inputs$matriz_distancias_saldos,
      indices = indices,
      vetor_c = vetor_c,
      solucao = solucao$column_solution,
      valor = solucao$objective_value,
      custo_fixo_usina = inputs$custo_fixo_usina
    )
  } else if (solver[1] == "gurobi") {
    if (!verificar_instalacao("gurobi")) {
      stop(pegar_msg_erro("gurobi"))
    }

    vtypes <- ifelse(indicador_de_inteiros, "I", "C")

    # create optimization problem
    model <- list()
    model$obj <- vetor_c
    model$modelsense <- "max"
    model$rhs <- vetor_restricoes
    model$vtype <- vtypes
    model$ub <- limite_superior_vars_decisao
    model$lb <- rep(0, ncol(matriz_restricoes))
    model$A <- matriz_restricoes

    # solve the optimization problem using Gurobi
    solucao <- gurobi::gurobi(model, params = list(...))

    list(
      inputs = inputs,
      matriz_distancias_saldos = inputs$matriz_distancias_saldos,
      indices = indices,
      vetor_c = vetor_c,
      solucao = solucao$x,
      valor = solucao$objval,
      custo_fixo_usina = inputs$custo_fixo_usina
    )
  } else if (solver[1] == "cplex") {
    if (!verificar_instalacao("Rcplex")) {
      stop(pegar_msg_erro("cplex"))
    }

    vtypes <- ifelse(indicador_de_inteiros, "I", "C")

    solucao <- Rcplex::Rcplex(
      cvec = vetor_c,
      Amat = matriz_restricoes,
      bvec = vetor_restricoes,
      lb = rep(0, ncol(matriz_restricoes)),
      ub = limite_superior_vars_decisao,
      vtype = vtypes,
      objsense = "max",
      control = list(...)
    )

    list(
      inputs = inputs,
      matriz_distancias_saldos = inputs$matriz_distancias_saldos,
      indices = indices,
      vetor_c = vetor_c,
      solucao = solucao$xopt,
      valor = solucao$obj,
      custo_fixo_usina = inputs$custo_fixo_usina
    )
  } else if (solver[1] == "highs") {
    if (!verificar_instalacao("highs")) {
      stop(pegar_msg_erro("highs"))
    }
    
    vtypes <- ifelse(indicador_de_inteiros, "I", "C")
    
    solucao <- highs::highs_solve(
      L = vetor_c,
      lower = rep(0, ncol(matriz_restricoes)),
      upper = limite_superior_vars_decisao,
      A = matriz_restricoes,
      lhs = rep(-Inf, nrow(matriz_restricoes)),
      rhs = vetor_restricoes,
      types = vtypes,
      maximum = TRUE
    )
    
    list(
      inputs = inputs,
      matriz_distancias_saldos = inputs$matriz_distancias_saldos,
      indices = indices,
      vetor_c = vetor_c,
      solucao = solucao$primal_solution,
      valor = solucao$objective_value,
      custo_fixo_usina = inputs$custo_fixo_usina
    )

  } else if (solver[1] == "glpk") {
    if (!verificar_instalacao("Rglpk")) {
      stop(pegar_msg_erro("glpk"))
    }

    vtypes <- ifelse(indicador_de_inteiros, "I", "C")

    solucao <- Rglpk::Rglpk_solve_LP(
      obj = vetor_c,
      mat = matriz_restricoes,
      rhs = vetor_restricoes,
      dir = rep("<=", nrow(matriz_restricoes)),
      bounds = list(
        lower = rep(0, ncol(matriz_restricoes)),
        upper = limite_superior_vars_decisao
      ),
      types = vtypes,
      max = TRUE,
      control = list(...)
    )

    list(
      inputs = inputs,
      matriz_distancias_saldos = inputs$matriz_distancias_saldos,
      indices = indices,
      vetor_c = vetor_c,
      solucao = solucao$solution,
      valor = solucao$objval,
      custo_fixo_usina = inputs$custo_fixo_usina
    )
  } else {
    stop("'solver' deve ser 'cbc', 'gurobi', 'cplex', 'glpk' ou 'highs'.")
  }
}

listar_usinas_viaveis <- function(tab, custo_usina) {
  tab |>
    dplyr::group_by(muni_cod_destino) |>
    dplyr::summarise(
      saldo_regiao = sum(saldo[saldo > 0], na.rm = TRUE)
    ) |>
    dplyr::filter(saldo_regiao > -!!custo_usina) |>
    dplyr::distinct(muni_cod_destino) |>
    dplyr::pull(muni_cod_destino)
}
