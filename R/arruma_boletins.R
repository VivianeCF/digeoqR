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
  if(modelo == 2) {
    ################################################################################
    ## MODELO 2010 - fosfato
    ################################################################################

    ## lista dos arquivos dos boletins
    arquivos <- list.files(dir_in, pattern = "*.csv")
    list_bol <- paste0(dir_in, arquivos)
    bol=0
    df_tudo <- 0
    for (i in 1:length(list_bol)) {
      df_tudo = read.csv2(
        bol <- paste0(dir_in, list.files(dir_in, pattern = "*.csv"))[i],
        fileEncoding = "latin1", header = FALSE
      )

      df_adm <- df_tudo[1:10,]
      df_an <- df_tudo[11:nrow(df_tudo),]
      df_s <- df_an %>% tidyr::separate(V1, c("NLAB", "COD"), sep = " ") %>%
        dplyr::relocate(COD, .before = NLAB)
      df_s[!is.na(df_s$COD), "NLAB"]<- df_s[!is.na(df_s$COD), "COD"]

      df_s[df_s$V2 == "SEDIMENTO CORRENTE", "COD"] <- "SMP"
      df_s[df_s$V2 == "REP", "COD"] <- "REP"
      df_s[df_s$V2 == "STD", "COD"] <- "STD"

      require(magrittr)
      df_an2 <- df_s %>%
        tibble::add_column(col1 = NA,
                           col2 = NA) %>%
        dplyr::relocate(col1, .before = COD) %>%
        dplyr::relocate(col2, .after = NLAB)
      colnames(df_an2) <- paste0("V", 1:ncol(df_an2))
      arrumado <- dplyr::bind_rows(df_adm, df_an2)
      colnames(arrumado) <- paste0("V", 1:ncol(arrumado))

      write.table(arrumado, paste0(dir_out, arquivos[i]),
                  row.names = FALSE, col.names = FALSE, sep = ";", dec = ",", na =
                    "", quote = FALSE, fileEncoding = "latin1")
    }
  }
}
