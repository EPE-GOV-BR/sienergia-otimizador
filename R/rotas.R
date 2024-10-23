rotas_possiveis <- function() {
    rotas <- c(
    "dens", "biocomb", "bioeletr",
    "biocomb_pec", "bioeletr_pec", "cofiring"
  )

    stats::setNames(rotas, rotas)
}

rotas_eletricas <- function(rotas) {
  rotas[rotas %in% c("dens", "bioeletr", "bioeletr_pec")]
}

rotas_combustivel <- function(rotas) {
  rotas[rotas %in% c("biocomb", "biocomb_pec")]
}

rotas_cofiring <- function(rotas) {
  rotas[rotas %in% c("cofiring")]
}

rotas_agricolas <- function(rotas) {
  rotas[rotas %in% c("dens", "bioeletr", "biocomb", "cofiring")]
}

rotas_pecuarias <- function(rotas) {
  rotas[rotas %in% c("bioeletr_pec", "biocomb_pec")]
}


separar_rotas_por_origem <- function(rotas) {
  list(
    rotas_agricolas = rotas_agricolas(rotas),
    rotas_pecuarias = rotas_pecuarias(rotas)
  )
}

separar_rotas_por_tipo <- function(rotas) {
  list(
    rotas_eletricas = rotas_eletricas(rotas),
    rotas_combustivel = rotas_combustivel(rotas),
    rotas_cofiring = rotas_cofiring(rotas)
  )
}
