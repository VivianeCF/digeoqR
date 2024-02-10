#'Prepara a base de dados para o processamento dos dados geoquímicos
#'
#'Recupera e une os dados dos boletins analíticos e os dados de campo da base de
#'dados fcampo. Os dados precisam estar nos diretórios gerados pela função
#'estrutura_diretorio. Os boletins analíticos devem estar no formato .csv com
#'layout modelo nos diretórios inputs/mineral ou inputs/quimica, conforme o
#'exemplo.
#'
#'@param analise Selecione o tipo de análise: 1 = mineral ou 2 = química
#' @param dir_bol Diretório dos boletins analíticos ex: "inputs/qumica/R/"
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo, 5 = água
#' @param dir_base Diretório dos dados de campo
#' @param base_campo Nome do arquivo das bases de campo
#' @param tipo_base tipo de base de dados: 1 = FCAMPO, 2 = SURVEY123, 3 = QFIELD
#'
#'@return Retorna a lista com as bases de dados em diferentes formatos
#'  transformados ou brutos, pivotados ou não.
#'@export
#' @examples
#' #prepara_bases(dir_bol = "inputs/quimica/R/",classe_am = 4,analise = 2,
#' dir_base = "inputs/campo/", tipo_base = 1, base_campo = "fcampo" )
prepara_bases <- function(dir_bol, classe_am, analise, dir_base,
                          tipo_base, base_campo ) {
  out <- list()
  a <- c("mineral", "química")
  t <-
    c("CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "SOLO",
      "ROCHA",
      "ÁGUA")
  if (analise == 2) {
    quimica <- le_boletim_quimica(classe_am, dir_bol)
    dados_campo <- extrai_dados_campo(tipo_base, dir_base,  base_campo )

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
    mineral <- le_boletim_mineral(classe_am, dir_bol)
    dados_brutos <- mineral$`dados brutos`
    dados_transformados <- mineral$`dados transformados`
    dados_campo <- extrai_dados_campo(tipo_base, dir_base,  nome_base)

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
