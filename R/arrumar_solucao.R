#' Arrumar solução do otimizador
#'
#' @param res Objeto retornado pela função \code{\link{otimizar}}.
#'
#' @return lista
#' @export
arrumar_solucao <- function(res) {
  rotas <- names(res$matriz_distancias_saldos)
  rotas <- stats::setNames(rotas, rotas)

  possiveis_sedes <- purrr::map(
    rotas,
    ~ res$matriz_distancias_saldos[[.x]] |>
      dplyr::distinct(muni_cod_destino) |>
      dplyr::pull(muni_cod_destino)
  )

  tam_solucao <- purrr::map(
    rotas,
    ~ length(res$indices[[.x]]) + length(possiveis_sedes[[.x]])
  )

  ini_solucao <- cumsum(tam_solucao) - as.numeric(tam_solucao) + 1
  fim_solucao <- cumsum(tam_solucao)

  solucoes <- purrr::map(
    rotas,
    ~ res$solucao[ini_solucao[.x]:fim_solucao[.x]] |>
      tidyr::replace_na(1)
  )

  depara_num_usinas <- purrr::map(
    rotas,
    ~ tibble::tibble(
      sede_cod = possiveis_sedes[[.x]],
      num_usinas = tail(solucoes[[.x]], length(possiveis_sedes[[.x]]))
    )
  )

  tab_solucao <- purrr::map(
    rotas,
    function(.x) {
      tab_valores <- dplyr::tibble(
        rowid = res$indices[[.x]],
        valor = solucoes[[.x]][1:length(res$indices[[.x]])]
      )
      res$matriz_distancias_saldos[[.x]] |>
        dplyr::mutate(
          rowid = dplyr::row_number()
        ) |>
        dplyr::filter(
          rowid %in% !!res$indices[[.x]]
        ) |>
        dplyr::left_join(
          tab_valores,
          by = "rowid",
          copy = TRUE
        ) |>
        dplyr::filter(valor > 0) |>
        dplyr::rename(sede_cod = muni_cod_destino) |>
        dplyr::collect() |>
        dplyr::left_join(depara_num_usinas[[.x]], by = "sede_cod") |>
        dplyr::filter(
          num_usinas > 0
        ) |>
        dplyr::mutate(
          distancia_aux = distancia,
          distancia = dplyr::case_when(
            distancia == 0 ~ 15,
            TRUE ~ distancia
          )
        )
    }
  )

  valor <-  purrr::map2_dfr(
    tab_solucao,
    names(tab_solucao),
    function(x, y) {
      x |>
        dplyr::summarise(total = sum(saldo*valor)) |>
        dplyr::bind_cols(
          x |>
            dplyr::distinct(num_usinas, sede_cod) |>
            dplyr::summarise(custo_capex = sum(num_usinas)*res$custo_fixo_usina[[y]])
        )
    }
  ) |>
    dplyr::summarise(
      sum(total + custo_capex)
    ) |>
    dplyr::pull()

  list(
    inputs = res$inputs,
    tab_solucao = tab_solucao,
    num_sedes = purrr::map(
      rotas,
      ~ dplyr::n_distinct(tab_solucao[[.x]]$sede_cod)
    ),
    valor = valor
  )
}
