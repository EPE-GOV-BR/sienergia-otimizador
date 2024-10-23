# Retorna $
calcular_custo_aquisicao <- function(parametros) {
  premio <- parametros$premio_produtor
  coleta <- parametros$custo_coleta_por_t
  carga <- parametros$custo_carga_por_t
  armazenamento <- parametros$custo_armazenamento_por_t

  premio + coleta + carga + armazenamento
}

# Retorna um valor adimensional
calcular_mult_valor_presente <- function(parametros) {
  taxa <- parametros$taxa
  anos <- parametros$anos_vida_util_usina

  (1 - 1 / (1 + taxa)^anos) / taxa
}

# Retorna $/MJ
calcular_custo_por_mj <- function(parametros) {
  beta <- parametros$beta_custo_usina # $/MW
  fator_disp <- parametros$fator_disponibilidade # adimensional
  multiplicador_vp <- calcular_mult_valor_presente(parametros) # adimensional
  perc_opex <- parametros$percentual_opex # adimensional

  custo_por_mwh <- beta / (fator_disp * 24 * 365)

  # Dividir por 3600 para converter de $/MWh para $/MJ
  (custo_por_mwh + perc_opex * custo_por_mwh * multiplicador_vp) / 3600
}
