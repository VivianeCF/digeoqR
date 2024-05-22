#' Arruma dados da OS
#'
#' @param dir_os Diretório dos documentos de solicitação das OS
#' @param doc_lab Documento das metodologias de análise do laboratório
#'
#' @return
#' @export
#'
#' @examples
arruma_os <- function(dir_os = "Data/PG2024/Rocha/os_solicitada/",  
                      doc_lab = "sgs-geosol_metodos-analiticos-03_2024.xlsx" ){

# dir_os <- "Data/PG2024/Rocha/OS_RA/"
list_os <- paste0(dir_os, list.files(dir_os, pattern = "*.xlsx"))
df_os = list()
acess <- readxl::read_xlsx(doc_lab)
os <- list()
for (i in 1:length(list_os)) {
ra_am <- readxl::read_xlsx(list_os[i],
                           sheet = "DadosAmostras")

ra_gerais <- readxl::read_xlsx(list_os[i],
                               sheet = "DadosGerais")
projeto <- as.character(ra_gerais[39, 10])
linha_final <- 5 + as.numeric(ra_gerais[38, 10])
ra_am_sel <- ra_am[5:linha_final,c(-48,-49)]
colnames(acess)
variaveis_am <- c("ID", "Longitude", "Latitude", "N_CAMPO", "Litotipo", "Lote",
                  "N_LAB", "CLASSE_AM", "Matriz", "Peso_gr", acess$'Código do Método',
                  "Observação")
colnames(ra_am_sel) <- variaveis_am
str(ra_am_sel)
ra_am_sel <- transform(ra_am_sel, Latitude = as.numeric(Latitude))
ra_am_sel <- transform(ra_am_sel, Longitude = as.numeric(Longitude))
ra_am_sel<- transform(ra_am_sel, PRP102_E...B = as.numeric(PRP102_E...B))
ra_am_sel <- transform(ra_am_sel, PRP102_Y...B = as.numeric(PRP102_Y...B))
ra_am_sel <- transform(ra_am_sel, PREPS80P...B = as.numeric( PREPS80P...B))
ra_am_sel <- ra_am_sel[!is.na(ra_am_sel$N_CAMPO),]
os[[i]] <- ra_am_sel
} 
df_os[[1]] = do.call(plyr::rbind.fill, os)
df_os[[2]] = acess
names(df_os) <- c("RA_OS", "DOC_LAB")
return(df_os)
}
