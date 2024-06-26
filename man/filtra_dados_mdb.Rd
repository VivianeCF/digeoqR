% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtra_dados_mdb.R
\name{filtra_dados_mdb}
\alias{filtra_dados_mdb}
\title{Filtra a base de dados de campo do banco de dados acess .mdb}
\usage{
filtra_dados_mdb(OS = 1723, input, output)
}
\arguments{
\item{OS}{Código do projeto de levantamento geoquímico}

\item{input}{Arquivo de entrada onde é aplicado o filtro do código do projeto}

\item{output}{Arquivo de saída para onde são gravadas as tabelas filtradas}
}
\value{
Arquivo de saída com as alterações
}
\description{
Esta função foi desenvolvida para extrair dados de campo que estão no fcampo
pré 2012 e gravar num arquivo fcampo vazio que deve estar no diretório
inputs/campo.
}
\examples{
#filtra_dados_mdb(OS = 1723, input = "inputs/campo/FCAMPO_SP_PR_SET_19_LOTES",
#output = "inputs/campo/fcampo")
}
