#'Filtra a base de dados de campo do banco de dados acess .mdb
#'
#'Esta função foi desenvolvida para extrair dados de campo que estão no fcampo
#'pré 2012 e gravar num arquivo fcampo vazio que deve estar no diretório
#'inputs/campo.
#'@param OS Código do projeto de levantamento geoquímico
#'@param input Arquivo de entrada onde é aplicado o filtro do código do projeto
#'@param output Arquivo de saída para onde são gravadas as tabelas filtradas
#'
#'@return Arquivo de saída com as alterações
#'@export
#' @examples
#' #filtra_dados_mdb(OS = 1723, input = "inputs/campo/FCAMPO_SP_PR_SET_19_LOTES",
#' #output = "inputs/campo/fcampo")
filtra_dados_mdb <- function(OS = 1723, input,
                             output) {
  ## São Paulo e Paraná  --------------------------------------------------------
  con <-
    RODBC::odbcDriverConnect(paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",
      input,
      ".mdb"
    ))
  AM_DADOS <- RODBC::sqlFetch(con, 'AM_DADOS')
  AM_DADOS <- AM_DADOS[AM_DADOS$OS == OS,]
  AM_LOCALIZACAO  <- RODBC::sqlFetch(con, 'AM_LOCALIZACAO')
  AM_LOCALIZACAO <-
    AM_LOCALIZACAO[AM_LOCALIZACAO$AMOSTRA %in% AM_DADOS$AMOSTRA,]
  BB_PROJETO <- RODBC::sqlFetch(con, 'BB_PROJETO')
  BB_PROJETO[is.na(BB_PROJETO$OS), "OS"] <-
    BB_PROJETO[is.na(BB_PROJETO$OS), "COD_AM_PROJETO"]
  ### Combina dados do banco de dados
  AM_LOTE <- RODBC::sqlFetch(con, 'AM_LOTE')
  AM_LOTE_AMOSTRA <- RODBC::sqlFetch(con, 'AM_LOTE_AMOSTRA')
  AM_LOTE_AMOSTRA <-
    AM_LOTE_AMOSTRA[AM_LOTE_AMOSTRA$AMOSTRA %in% AM_DADOS$AMOSTRA, ]
  AM_AMBIENTE <- RODBC::sqlFetch(con, 'AM_AMBIENTE')
  AM_AMBIENTE <-
    AM_AMBIENTE[AM_AMBIENTE$AMOSTRA %in% AM_DADOS$AMOSTRA, ]
  AM_LOTE <-
    AM_LOTE[AM_LOTE$COD_AM_LOTE %in% AM_LOTE_AMOSTRA$LOTE, ]

  con2 <-
    RODBC::odbcDriverConnect(paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=",
      output ,
      ".mdb"
    ))
  RODBC::sqlQuery(con2, "DROP TABLE AM_DADOS")
  RODBC::sqlSave(con2, AM_DADOS,  addPK = TRUE, append = FALSE)
  RODBC::sqlQuery(con2, "DROP TABLE BB_PROJETO")
  RODBC::sqlSave(con2, BB_PROJETO,  addPK = TRUE, append = FALSE)
  RODBC::sqlQuery(con2, "DROP TABLE AM_LOCALIZACAO")

  RODBC::sqlSave(con2, AM_LOCALIZACAO ,  addPK = TRUE, append = FALSE)
  RODBC::sqlQuery(con2, "DROP TABLE AM_AMBIENTE")
  RODBC::sqlSave(con2, AM_AMBIENTE ,  addPK = TRUE, append = FALSE)
  RODBC::sqlQuery(con2, "DROP TABLE AM_LOTE_AMOSTRA")
  RODBC::sqlSave(con2, AM_LOTE_AMOSTRA , addPK = TRUE, append = FALSE)
  RODBC::sqlQuery(con2, "DROP TABLE AM_LOTE")
  RODBC::sqlSave(con2, AM_LOTE , addPK = TRUE, append = FALSE)
  RODBC::odbcCloseAll()
}
