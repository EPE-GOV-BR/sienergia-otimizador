gg_regioes_apresentacao <- function(tab, regiao_escopo, sedes = NULL, title = "") {
  if (length(regiao_escopo) == 1) {
    if (regiao_escopo == "Brasil") {
      regiao_escopo <- otimizadorLinear::estados_br
    } else if (!regiao_escopo %in% otimizadorLinear::estados_br) {
      stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
    }
  } else if (!all(regiao_escopo %in% otimizadorLinear::estados_br)) {
    stop("`regiao_escopo` deve ser 'Brasil' ou um vetor de siglas de estados brasileiros.")
  }

  tab_geo <- regiao_escopo |>
    purrr::map(
      ~ sf::st_as_sf(otimizadorLinear::dados_geo[[.x]])
    ) |>
    dplyr::bind_rows()

  if (is.null(tab) || nrow(tab) == 0) {
    mapa <- tab_geo |>
      gg_mapa_nulo_apresentacao(ref = ref, title = title)
  } else {
    if (!is.null(sedes)) {
      tab <- tab |>
        dplyr::filter(sede_cod %in% sedes)
      texto_sem <- "Outros municípios"
      texto_com <- "Municípios da(s) sede(s)"
    } else {
      texto_sem <- "Sem aproveitamento de resíduo"
      texto_com <- "Com aproveitamento de resíduo"
    }

    if (is.null(title)) {
      sede_nome <- tab_geo |>
        dplyr::filter(code_muni == .sed_cod) |>
        dplyr::pull(name_muni)

      title <- glue::glue("Sede {sede_nome}")
    }
    tab_geo |>
      dplyr::left_join(
        tab,
        by = c("code_muni" = "muni_cod_origem")
      ) |>
      dplyr::mutate(
        regiao = ifelse(
          !is.na(sede_cod),
          texto_com,
          texto_sem
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(ggplot2::aes(fill = regiao), lwd = 0) +
      ggplot2::labs(fill = "", title = title) +
      ggplot2::scale_fill_manual(values = cores_mapa_apresentacao()) +
      tema_mapa_gg_apresentacao()
  }
}

gg_mapa_nulo_apresentacao <- function(tab, ref, title) {
  tab |>
    dplyr::mutate(sede_nome = ref) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = sede_nome), lwd = 0) +
    ggplot2::labs(fill = "Sede", title = title) +
    ggplot2::scale_fill_manual(values = cores_mapa()[2]) +
    tema_mapa_gg()
}

tema_mapa_gg_apresentacao <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

cores_mapa_apresentacao <- function() {
  c("#008000", "#d3d3d3")
}
