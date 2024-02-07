#' Extrai dados de campo do banco de dados
#'
#' Uma base de dados é preparada com as informações da localização das amostras
#' e os nomes de campo e laboratório. Esta função só está implementada para o
#' tipo_base = 1 (FCAMPO)
#'

#' @param nome_base Nome da base de dados de campo
#'
#' @param tipo_base Tipo de base de dados: 1 = FCAMPO, 2 = SURVEY123, 3 = QFIELD
#' @param dir_base Diretório da base de dados de campo
#'
#' @return
#' Retorna uma tabela com os dados de campo
#' @export
#'
#' @examples
#'
extrai_dados_campo <- function(tipo_base, dir_base,  nome_base) {
  if (tipo_base == 1) {
    con <-
      RODBC::odbcDriverConnect(
        paste(
          "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)}",
          paste0("DBQ=", dir_base, nome_base),
          "Trusted_Connection=Yes" ,
          sep = ";"
        )
      )


    dados <- RODBC::sqlFetch(con, 'AM_DADOS')
    local <- RODBC::sqlFetch(con, 'AM_LOCALIZACAO')
    unida <- dplyr::left_join(dados, local, by = "AMOSTRA")
    os <- RODBC::sqlFetch(con, 'BB_PROJETO')
    unida_os <- dplyr::left_join(unida, os, by = "OS")
    select <- colnames(unida_os)[c(1, 2, 5, 32, 33, 46, 47, 48, 63)]
    df_base <- unida_os[, select]
    colnames(df_base)[3] <- "COD_CLASSE"
    am_lote <- RODBC::sqlFetch(con, 'AM_LOTE')
    am_lote_amostra <- RODBC::sqlFetch(con, 'AM_LOTE_AMOSTRA')
    colnames(am_lote_amostra)[2] <- "COD_AM_LOTE"
    am_lote_amostra <- am_lote_amostra[, -1]
    bb_dados_classe <- RODBC::sqlFetch(con, 'BB_DADOS_CLASSE')
    df_base <-
      dplyr::left_join(df_base, bb_dados_classe, by = "COD_CLASSE")
    df_base <- df_base[, -11]
    df_base <- df_base[, -3]
    am_lote_amostra <-
      dplyr::left_join(am_lote_amostra, am_lote, by = "COD_AM_LOTE")
    am_lote_amostra <- am_lote_amostra[, -1]
    df_base <-
      dplyr::left_join(df_base, am_lote_amostra, by = "AMOSTRA")
    colnames(df_base)[1] <- "N_LAB"
    out <- data.frame()
    out <- df_base
  } else{
    out <- data.frame()
  }
  RODBC::odbcCloseAll()
  return(out)
}
