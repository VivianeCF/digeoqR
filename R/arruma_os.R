#' Arruma dados da OS
#'
#' @param dir_os Diretório dos documentos de solicitação das OS
#' @param doc_lab Documento das metodologias de análise do laboratório
#'
#' @return
#' @export
#'
#' @examples
arruma_os <- function(dir_os = "Data/PG2024/Rocha/os_solicitada/"){

list_os <- paste0(dir_os, list.files(dir_os, pattern = "*.xlsx"))
os <- list()
for (i in 1:length(list_os)) {
ra_am <- readxl::read_xlsx(list_os[i],
                           sheet = "DadosAmostras")

ra_gerais <- readxl::read_xlsx(list_os[i],
                               sheet = "DadosGerais")
projeto <- as.character(ra_gerais[39, 10])
variaveis_am <- as.character(ra_am[4,])
variaveis_am[c(1:10,76:77,78)] <- c("ID", "Longitude", "Latitude", "N_CAMPO", "Litotipo", "Lote",
                                    "N_LAB", "CLASSE_AM", "Matriz", "Peso_gr", "Mat_ref", "Massa_min", "Observações")
linha_final <- 5 + as.numeric(ra_gerais[38, 10])
ra_am_sel <- ra_am[6:linha_final,]

variaveis_am <- gsub(" (R$/amostra)", "", variaveis_am, fixed = TRUE)
variaveis_am <- gsub("-", "_", variaveis_am, fixed = TRUE)
variaveis_am <- gsub(" (R$/kg)", "", variaveis_am, fixed = TRUE)

colnames(ra_am_sel) <- variaveis_am

ra_am_sel <- transform(ra_am_sel, Latitude = as.numeric(Latitude))
ra_am_sel <- transform(ra_am_sel, Longitude = as.numeric(Longitude))
ra_am_sel<- transform(ra_am_sel, PRP102_E_B = as.numeric(PRP102_E_B))
ra_am_sel <- transform(ra_am_sel, PRP102_Y_B = as.numeric(PRP102_Y_B))
ra_am_sel <- transform(ra_am_sel, PREPS80P_B = as.numeric( PREPS80P_B))
ra_am_sel <- ra_am_sel[!is.na(ra_am_sel$N_CAMPO),]
os[[i]] <- ra_am_sel
}
df_os = do.call(plyr::rbind.fill, os)
return(df_os)
}
