# Carregar o pacote
devtools::load_all()

# Definir o estado de escopo
regiao_escopo <- c("Brasil")

# Conectar com o SQLite baixado
con <- DBI::dbConnect(RSQLite::SQLite(), "SIEnergia_dados.sqlite")

# Buscar a tabela de parâmetros
parametros <- dplyr::tbl(con, "exemplo_parametros") |>
  dplyr::collect()

# Alterar parâmetros se necessário
# parametros$receita[1] <- 900
# parametros$receita[2] <- 5
# parametros$receita[5] <- 2
# parametros$receita[3] <- 900
# parametros$premio_produtor[4] <- -80
# parametros$premio_produtor[5] <- -80
# parametros$unidade_receita[3] <- "mwh"
# parametros$unidade_receita[2] <- "m3"

# Definir quais rotas vamos usar para calcular
rotas <- c(
  "dens",
  "biocomb",
  "biocomb_pec",
  "bioeletr",
  "bioeletr_pec",
  "cofiring"
)

produtos_selecionados <- dplyr::tbl(con, "carga_energia_brasil") |>
  dplyr::distinct(produto_codigo) |>
  dplyr::pull()

# Calcular os inputs do modelo
tictoc::tic()
inputs <- calcular_inputs(
  con,
  parametros,
  regiao_escopo,
  populacao_minima = 0,
  rotas,
  produtos_selecionados,
  backend = "ram",
  sazonalidade = TRUE
)
tictoc::toc()

# Executar o otimizador
tictoc::tic()
res_bruto <- otimizar(
  inputs,
  solver = "gurobi",
  num_c = 2,
  limitar_tamanho_usina = TRUE,
  num_max_usinas_sede = 50,
  limitar_por_demanda = FALSE,
  TimeLimit = 20
)
tictoc::toc()

# Arrumar a solução do otimizador
res <- arrumar_solucao(res_bruto)

# Tabelas de resultados ---------------------------------------------
tab_fatores <- dplyr::tbl(con, "fatores_externalidades") |>
  dplyr::collect() |>
  dplyr::mutate(
    fator_pessoal = 0.8
  )

fatores_mwh <- tab_fatores |> dplyr::filter(unidade == "mwh")
fatores_m3 <- tab_fatores |> dplyr::filter(unidade == "m3")
fatores_ton <- tab_fatores |> dplyr::filter(unidade == "ton")


fatores <- list(
  fator_consumo_mwh = fatores_mwh$consumo_agua_m3_por_un[1],
  fator_consumo_m3 = fatores_m3$consumo_agua_m3_por_un[1],
  fator_consumo_ton = fatores_ton$consumo_agua_m3_por_un[1],
  fator_emissoes_mwh = fatores_mwh$toneladas_co2_eq_por_un[1],
  fator_emissoes_m3 = fatores_m3$toneladas_co2_eq_por_un[1],
  fator_emissoes_ton = fatores_ton$toneladas_co2_eq_por_un[1],
  fator_empregabilidade_mwh = fatores_mwh$empregos_por_un[1],
  fator_empregabilidade_m3 = fatores_m3$empregos_por_un[1],
  fator_empregabilidade_ton = fatores_ton$empregos_por_un[1],
  fator_pessoal = fatores_mwh$fator_pessoal[1],
  fator_ida_volta = 2
)

tab_resultado <- preparar_tabelas_resultados(
  con = con,
  resultado = res,
  parametros = parametros,
  regiao_escopo,
  fatores
)

tab_resultado$demanda_satisfeita |> dplyr::mutate(porc_demanda_satisfeita = round(porc_demanda_satisfeita, 4))
tab_resultado$biomassa_aproveitada
tab_resultado$combustivel_aproveitado


# calculos apresentacao ---------------------------------------------------

### Mapas:

# estão salvos na pasta data-raw/res_apresentacao

### Numero de usinas:

# geral:

tab_resultado$resultados_potencia |>
  dplyr::summarise(total_usinas = sum(num_usinas))

# por rota:

tab_resultado$resultados_potencia |>
  dplyr::group_by(rota) |>
  dplyr::summarise(total_usinas = sum(num_usinas))

### % de biomassa aproveitada por rota:

tab_resultado$biomassa_aproveitada |>
  dplyr::select(rota, porc_biomassa_aproveitada)

### % da demanda atendida por rota:

tab_resultado$demanda_satisfeita

### Valor total do lucro:

scales::dollar(
  res$valor,
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

### Quebra do valor total do lucro por rota

tab_resultado$tabela_lucros

##### Por região:

# tabela que separa municipios em regioes:

gera_tab_regioes(con, inputs, tab_resultado)

gera_tab_regioes(con, inputs, tab_resultado) |>
  dplyr::select(-regiao) |>
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = sum
    )
  )

# Fitness -----------------------------------------------------------------

scales::dollar(
  res$valor,
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

# Mapa --------------------------------------------------------------------

# ggplot

gg_regioes_apresentacao(
  res$tab_solucao$dens,
  regiao_escopo,
  title = "Densificação (Res. Agrícola)"
)

gg_regioes_apresentacao(
  res$tab_solucao$biocomb,
  regiao_escopo,
  title = "Biodigestão - Combustível (Res. Agrícola)"
)

gg_regioes_apresentacao(
  res$tab_solucao$bioeletr,
  regiao_escopo,
  title = "Biodigestão - Energia Elétrica (Res. Agrícola)"
)

gg_regioes_apresentacao(
  res$tab_solucao$biocomb_pec,
  regiao_escopo,
  title = "Biodigestão - Combustível (Res. Pecuário)"
)

gg_regioes_apresentacao(
  res$tab_solucao$bioeletr_pec,
  regiao_escopo,
  title = "Biodigestão - Energia Elétrica (Res. Pecuário)"
)

gg_regioes_apresentacao(
  res$tab_solucao$cofiring,
  regiao_escopo,
  title = "Cofiring (Res. Agrícola)"
)

### Echarts

ec_regioes(
  res$tab_solucao$dens,
  regiao_escopo,
  title = "Densificação (Res. Agrícola)"
)

ec_regioes(
  res$tab_solucao$biocomb,
  regiao_escopo,
  title = "Biodigestão - Combustível (Res. Agrícola)"
)

ec_regioes(
  res$tab_solucao$bioeletr,
  regiao_escopo,
  title = "Biodigestão - Energia Elétrica (Res. Agrícola)"
)

ec_regioes(
  res$tab_solucao$biocomb_pec,
  regiao_escopo,
  title = "Biodigestão - Combustível (Res. Pecuário)"
)

ec_regioes(
  res$tab_solucao$bioeletr_pec,
  regiao_escopo,
  title = "Biodigestão - Energia Elétrica (Res. Pecuário)"
)

ec_regioes(
  res$tab_solucao$cofiring,
  regiao_escopo,
  title = "Cofiring (Res. Agrícola)"
)

# -------------------------------------------------------------------------
