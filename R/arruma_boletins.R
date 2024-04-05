## Arruma boletim para ser lido pelo le_boletim_quimica
#' Title
#'
#' @param dir_in Diretório onde estão os boletins
#' @param dir_out Diretório de saída dos boletins arrumados
#' @param modelo Modelo de estrutura do boletim
#'
#' @return
#' @export
#'
#' @examples
#'
arruma_boletins <- function(dir_in, dir_out, modelo = 1){
if(modelo == 1) {
################################################################################
## MODELO 2014
################################################################################

## lista dos arquivos dos boletins
arquivos <- list.files(dir_in, pattern = "*.csv")
list_bol <- paste0(dir_in, arquivos)
bol=0
df_tudo <- 0
for (i in 1:length(list_bol)) {
  df_tudo = read.csv2(
    bol <- paste0(dir_in, list.files(dir_in, pattern = "*.csv"))[i],
    header = F,
    as.is = T,
    fill = TRUE,
    encoding = "latin1"
  )

  ## bloco info serviço
  df <- df_tudo
  bloco1 <- df[1:9, 3:8]
  colnames(bloco1) <- colnames(df[,1:6])

  ## bloco cabeçalho
  bloco2 <- df[10:14,]

  ## Linha adicionada type
  linha_type <- df[10,]
  linha_type[5] <- "Type"

  ## bloco resultados
  bloco3 <- df[-1:-14,]

  ## Une os blocos
  arrumado <- dplyr::bind_rows(bloco1, bloco2, linha_type, bloco3)

  write.table(arrumado, paste0(dir_out, arquivos[i]),
              row.names = FALSE, col.names = FALSE, sep = ";", dec = ",", na =
              "", quote = FALSE, fileEncoding = "latin1")
}
}
}
