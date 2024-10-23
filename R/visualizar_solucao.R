
#' ggplot com mapa das regiões
#'
#' @param tab tabela de solução do otimizador.
#' @param regiao_escopo Região escopo da otimização.
#' @param sedes Sedes específicas a serem mostradas no mapa.
#' @param title Título do mapa.
#' @param ref Referência para municípios sem solução.
#'
#' @return ggplot com o mapa.
#' @export
gg_regioes <- function(tab, regiao_escopo, sedes = NULL, title = "",
                       ref = "Sem aproveitamento de resíduo") {
  
  
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
      gg_mapa_nulo(ref = ref, title = title)
  } else {
    if (!is.null(sedes)) {
      tab <- tab |>
        dplyr::filter(sede_cod %in% sedes)
      texto_sem <- "Sem aproveitamento de biomassa na(s) sede(s) selecionadas"
      texto_com <- "Municípios fornecedores de biomassa"
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
      ggplot2::geom_sf(ggplot2::aes(fill = regiao)) +
      ggplot2::labs(fill = "", title = title) +
      ggplot2::scale_fill_manual(values = cores_mapa()) +
      tema_mapa_gg()
  }
}

#' echart com mapa das regiões
#'
#' @param tab tabela de solução do otimizador.
#' @param regiao_escopo Região escopo da otimização.
#' @param sedes Sedes específicas a serem mostradas no mapa.
#' @param title Título do mapa.
#'
#' @return ggplot com o mapa.
#' @export
ec_regioes <- function(tab, regiao_escopo, sedes = NULL, title = "") {
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
      ec_mapa_nulo(ref = ref, title = title)
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

    tab_mapa <- tab_geo |>
      dplyr::left_join(
        tab,
        by = c("code_muni" = "muni_cod_origem")
      ) |>
      ### Pegar o nome da sede
      dplyr::left_join(
        tab_geo |>
          dplyr::select(code_muni, sede_name = name_muni) |>
          sf::st_drop_geometry(),
        by = c("sede_cod" = "code_muni")
      ) |>
      ###
      dplyr::group_by(code_muni) |>
      dplyr::summarise(
        name_muni = dplyr::first(name_muni),
        sede_name = stringr::str_c(sede_name, collapse = ", ")
      ) |>
      dplyr::mutate(
        regiao = ifelse(!is.na(sede_name), 1, 0),
        sede_name = ifelse(
          is.na(sede_name),
          texto_sem,
          sede_name
        )
      ) |>
      dplyr::rename(name = name_muni)

    echarts4r::e_charts() |>
      echarts4r::e_map_register("regiao", geojsonsf::sf_geojson(tab_mapa)) |>
      echarts4r::e_list(
        list(
          series = list(
            type = "map",
            map = "regiao",
            selectedMode = FALSE,
            emphasis = list(
              disabled = TRUE
            ),
            data = tab_mapa |>
              sf::st_drop_geometry() |>
              dplyr::select(name, value = regiao, sede_name) |>
              purrr::transpose()
          ),
          visualMap = list(
            type = "piecewise",
            splitNumber = 2,
            pieces = list(
              list(
                min = 0,
                max = 0,
                label = texto_sem,
                color = "#808080"
              ),
              list(
                min = 1,
                max = 1,
                label = texto_com,
                color = "orange"
              )
            )
          ),
          title = list(
            text = title,
            left = "center"
          ),
          tooltip = list(
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {
                  text = '<b>' + params.name + '</b><br>';
                  if (params.value == 0) {
                    text += '{{texto_sem}}';
                  } else {
                    text += 'Envia resíduo para: ' + params.data.sede_name;
                  }
                  return text;
                }",
                .open = "{{",
                .close = "}}"
              )
            )
          )
        )
      )
  }
}

gg_mapa_nulo <- function(tab, ref, title) {
  tab |>
    dplyr::mutate(sede_nome = ref) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = sede_nome)) +
    ggplot2::labs(fill = "Sede", title = title) +
    ggplot2::scale_fill_manual(values = cores_mapa()[2]) +
    tema_mapa_gg()
}

ec_mapa_nulo <- function(tab, ref, title) {
  tab_mapa <- tab |>
    dplyr::rename(name = name_muni)
  tab_mapa |>
    sf::st_drop_geometry() |>
    dplyr::select(name) |>
    echarts4r::e_charts(name) |>
    echarts4r::e_map_register("regiao", geojsonsf::sf_geojson(tab_mapa)) |>
    echarts4r::e_map(
      name,
      map = "regiao",
      selectedMode = FALSE,
      emphasis = list(
        itemStyle = list(
          areaColor = "yellow"
        ),
        label = list(
          color = "black"
        )
      )
    ) |>
    echarts4r::e_title(title, left = "center")
}

tema_mapa_gg <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

cores_mapa <- function() {
  c("orange", "#808080")
}
