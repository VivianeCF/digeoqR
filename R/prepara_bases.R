#'Prepara a base de dados para o processamento dos dados geoquímicos
#'
#'Recupera e une os dados dos boletins analíticos e os dados de campo da base de
#'dados fcampo. Os dados precisam estar nos diretórios gerados pela função
#'estrutura_diretorio. Os boletins analíticos devem estar no formato .csv com
#'layout modelo nos diretórios inputs/mineral ou inputs/quimica, conforme o
#'exemplo.
#'
#'@param analise Selecione o tipo de análise: 1 = mineral ou 2 = química
#'@param tipo Selecione a classe da amostra: 1 = concentrado de bateia, 2 =
#'  "sedimento de corrente", 3 = solo, 4 = rocha, 5 = água
#'
#'@param fcampo Nome do arquivo acess mdb com dados de campo.
#'
#'@return Retorna a lista com as bases de dados em diferentes formatos
#'  transformados ou brutos, pivotados ou não.
#'@export
#' @examples
#' #prepara_bases(analise = 2, tipo = 2)
prepara_bases <- function(analise, tipo, fcampo = "fcampo") {
  out <- list()
  a <- c("mineral", "química")
  t <-
    c("CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "SOLO",
      "ROCHA",
      "ÁGUA")
  if (analise == 2) {
    quimica <- le_boletim_quimica(tipo)
    dados_campo <- extrai_dados_campo(tipo_base = 1, fcampo)

    # Base não pivotada
    out[[1]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados brutos`, by = "N_LAB")
    out[[2]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados transformados`, by = "N_LAB")
    # Base pivotada
    out[[3]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados brutos pivotados`, by = "N_LAB")
    out[[4]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados transformados pivotados`,
                                  by = "N_LAB")

  } else{
    mineral <- le_boletim_mineral(tipo)
    dados_brutos <- mineral$`dados brutos`
    dados_transformados <- mineral$`dados transformados`
    dados_campo <- extrai_dados_campo(tipo_base = 1)

    # Base não pivotada
    out[[1]] <- dplyr::right_join(dados_campo,
                                  mineral$`dados brutos`, by = "N_LAB")
    out[[2]] <- dplyr::right_join(dados_campo,
                                  mineral$`dados transformados`, by = "N_LAB")
    # Base pivotada
    out[[3]] <- dplyr::right_join(dados_campo,
                                  mineral$`dados brutos pivotados`, by = "N_LAB")
    out[[4]] <- dplyr::right_join(dados_campo,
                                  mineral$`dados transformados pivotados`,
                                  by = "N_LAB")
  }
  names(out) <-  c(
    "dados brutos",
    "dados transformados",
    "dados brutos pivotados",
    "dados transformados pivotados"
  )
  return(out)
}
