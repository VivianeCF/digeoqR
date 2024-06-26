% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/le_boletim_quimica.R
\name{le_boletim_quimica}
\alias{le_boletim_quimica}
\title{Lê boletins de análises químicas}
\usage{
le_boletim_quimica(classe_am, dir_bol, dir_ucc, ref_ucc)
}
\arguments{
\item{classe_am}{Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
corrente, 3 = rocha, 4 = solo, 5 = água}

\item{dir_bol}{Diretório dos boletins analíticos ex: "inputs/quimica/R/"}

\item{dir_ucc}{Diretório do arquivo UCC}

\item{ref_ucc}{Planilha com valores da Concentração Média da Crosta Superior
Rudnick e Gao 2004}
}
\value{
Retorna uma lista com todos os dados do boletim: resultados
analíticos, condições analíticas, QA/QC, requisição das análises e
preparação das amostras.
}
\description{
Esta função lê os boletins csv e extrai as informações na forma de tabelas
que são gravadas no diretório de saída.
}
\examples{
# le_boletim_quimica()
}
