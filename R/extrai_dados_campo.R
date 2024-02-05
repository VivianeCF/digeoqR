#' Extrai dados de campo do banco de dados
#'
#' Uma base de dados é preparada com as informações da localização das amostras
#' e os nomes de campo e laboratório. Esta função só está implementada para o
#' tipo_base = 1
#'
#' Arquivo com os dados de campo proveniente de bases de dados: 1 = FCAMPO, 2 =
#' SURVEY123, 3 = QFIELD).
#'
#' @param tipo_base tipo de base de dados: 1 = FCAMPO, 2 = SURVEY123, 3 = QFIELD
#' @param nome nome do arquivo da base de dados
#'
#' @return
#' Retorna uma tabela com os dados de campo
#' @export
#'
#' @examples
#'
extrai_dados_campo <- function(tipo_base = 1,
                               nome = "fcampo") {
  if (tipo_base == 1) {
    con <-
      RODBC::odbcDriverConnect(
        paste(
          "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)}",
          paste0("DBQ=inputs/campo/", nome),
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
    nome_base <- unida_os[, select]
    colnames(nome_base)[3] <- "COD_CLASSE"
    am_lote <- RODBC::sqlFetch(con, 'AM_LOTE')
    am_lote_amostra <- RODBC::sqlFetch(con, 'AM_LOTE_AMOSTRA')
    colnames(am_lote_amostra)[2] <- "COD_AM_LOTE"
    am_lote_amostra <- am_lote_amostra[, -1]
    bb_dados_classe <- RODBC::sqlFetch(con, 'BB_DADOS_CLASSE')
    nome_base <-
      dplyr::left_join(nome_base, bb_dados_classe, by = "COD_CLASSE")
    nome_base <- nome_base[, -11]
    nome_base <- nome_base[, -3]
    am_lote_amostra <-
      dplyr::left_join(am_lote_amostra, am_lote, by = "COD_AM_LOTE")
    am_lote_amostra <- am_lote_amostra[, -1]
    nome_base <-
      dplyr::left_join(nome_base, am_lote_amostra, by = "AMOSTRA")
    colnames(nome_base)[1] <- "N_LAB"
    out <- data.frame()
    out <- nome_base
  } else{
    out <- data.frame()
  }
  write.csv2(out, "outputs/dados_campo.csv", fileEncoding = "latin1")
  RODBC::odbcCloseAll()
  return(out)
}
