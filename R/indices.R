gerar_indices <- function(tab, num_c) {
  if (is.infinite(num_c)) {
    return(1:calcular_num_linhas(tab))
  }
  tab |>
    dplyr::mutate(
      rowid = dplyr::row_number()
    ) |>
    dplyr::group_by(muni_cod_origem) |>
    dplyr::slice_max(order_by = saldo, n = num_c) |>
    dplyr::pull(rowid) |>
    sort()
}