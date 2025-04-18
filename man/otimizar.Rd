% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/otimizar.R
\name{otimizar}
\alias{otimizar}
\title{Otimizador linear}
\usage{
otimizar(
  inputs,
  solver = c("cbc", "gurobi"),
  num_c = Inf,
  limitar_tamanho_usina = TRUE,
  num_max_usinas_sede = 3,
  limitar_por_demanda = TRUE,
  ...
)
}
\arguments{
\item{inputs}{Tabela de parâmetros.}

\item{solver}{Otimizador a ser utilizado.}

\item{num_c}{Número de municípios a serem considerados como fornecedores de
cada sede.}

\item{limitar_tamanho_usina}{Se \code{TRUE}, limita o tamanho das usinas.}

\item{num_max_usinas_sede}{Número máximo de usinas por sede.}

\item{limitar_por_demanda}{Se \code{TRUE}, limita a energia produzida pela
demanda.}

\item{...}{Argumentos opcionais para as funções de otimização, a depender do
Otimizador a ser utilizado.}
}
\description{
Otimizador linear
}
