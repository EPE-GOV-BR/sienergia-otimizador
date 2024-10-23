## code to prepare `geo_mt` dataset goes here

estados <- unique(geobr::grid_state_correspondence_table$abbrev_state)

dados_geo <- purrr::map(
  estados,
  ~ geobr::read_municipality(
    code_muni = .x,
    year = 2017,
    showProgress = FALSE
  )
)

dados_geo <- purrr::set_names(dados_geo, estados)

usethis::use_data(dados_geo, overwrite = TRUE)
