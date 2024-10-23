## code to prepare `estados_br` dataset goes here

estados_br <- unique(geobr::grid_state_correspondence_table$abbrev_state)

usethis::use_data(estados_br, overwrite = TRUE)
