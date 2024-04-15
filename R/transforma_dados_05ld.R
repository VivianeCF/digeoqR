#' Prepara e transforma dados para o tratamento estatístico
#'
#'Transforma dados <LD para 0,5*LD
#' @param data
#'
#' @return
#' @export
#'
#' @examples
transforma_dados_05ld <- function(data){

out <- list()
data$Lab_orig <- data$Lab
data$Lab_orig <- stringr::str_trim(data$Lab_orig)
data <-  data %>% dplyr::select(!Lab)
Lab_orig <-  c("Absorção Atômica - Água Régia Invertida",
               "Absorção Atômica - EDTA a Frio",
               "Absorção Atômica - HNO3 a Quente",
               "Colorimetria",
               "Absorção Atômica - H3PO4",
               "Absorção Atômica/Geração de Hidretos - Água Régia a Quente",
               "Espectrografia Ótica de Emissão",
               "Absorção Atômica - Fusão Ácida",
               "Medidor de Íon Específico - Semiquantitativa",
               "Medidor de Íon Específico - HCl Diluído a Frio",
               "ICP-MS - Água Régia",
               "ICP-AES - Digestão água régia",
               "Espectrofotometria de Absorção Atômica - Ácido Nítrico a quente (HN0₃)",
               "Espectrofotometria de Absorção Atômica - Ácido bromídrico (HBr)",
               "Abserção Atômica / Geração de Hidretos (AAGH) - Ácidos fortes em forno de microondas",
               "Absorção Atômica - HBr, Br",
               "Medidor de Íon Específico",
               "Absorção Atômica - HNO3, HF, HCl, HClO4",
               "Absorção Atômica - Sublimação KI",
               "Cromatografia - Semiquantitativa",
               "ICP-MS - Digestão Multiácida",
               "Fusão de 50g - Fire Assay",
               "ICP-MS - Digestão Água Régia",
               "Eletrodo de Íon Específico F - Fusão e Dissolução",
               "1F",
               "",
               "ICM14B",
               "FAA505",
               "FAI515",
               "Espectrofotometria de Absorção Atômica - Ácido Nítrico a quente (HN03)",
               "Espectrografia Ótica de Emissão - Semiquantitativa",
               "NA",
               "Água régia_ICP-MS + ICP-AES",
               "SCR33",
               "ICM40B",
               "ICP-AES",
               "AAS19V")

Lab <- c("AA", "AA", "AA", "COL", "AA", "AA", "EE", "AA", "IE", "IE", "EE",
         "EE", "AA", "AA", "AA", "AA", "IE", "AA", "AA", "CR", "EE", "FA",
         "EE", "IE", "EE", "", "EE", "FA", "FA", "AA", "EE", "EE", "EE",
         "EE", "EE", "EE", "AA")

bb_lab <- data.frame(Lab_orig ,
                     Lab)
data <-  dplyr::left_join(data,bb_lab,  by = "Lab_orig")
data <- data %>% dplyr::relocate(c(Lab, Lab_orig), .after = CLASSE)
data[,11: ncol(data)] <- data[,11: ncol(data)] %>%
  dplyr::mutate_all(stringr::str_replace_all, " ", "")
data$ORDEM <- as.numeric(1:nrow(data))

# Importar informações faltantes do Au ppb do Au_ppm e converter
  if(!"AU_PPB" %in% colnames(data)){
    data$AU_PPB <- NA
  }

  para_mudar <- data[is.na(data$AU_PPB), c("ORDEM", "AU_PPM")]
  para_mudar$Q <- stringr::str_extract(para_mudar$AU_PPM, pattern = "<")
  para_mudar[is.na(para_mudar$Q), "Q"] <-
    stringr::str_extract(para_mudar[is.na(para_mudar$Q), "Q"], pattern = ">")
  unique(para_mudar$Q)
  para_mudar[is.na(para_mudar$Q), "Q"] <- ""
  para_mudar$value <- gsub( "<", "", para_mudar$AU_PPM, fixed = TRUE)
  para_mudar$value <-  gsub( ">", "", para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( ",", ".", para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "ND", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I.S.", NA, para_mudar$value, fixed = TRUE)
  para_mudar$value  <- gsub( "I,S,", NA, para_mudar$value, fixed = TRUE)

  para_mudar$value <- as.numeric(para_mudar$value)*1000
  para_mudar$AU_PPB <- paste0(para_mudar$Q, para_mudar$value)
  unique(para_mudar$AU_PPB)
  # para_mudar[para_mudar$AU_PPM == "ND", "AU_PPB"] <- "ND"
  # para_mudar[para_mudar$AU_PPM == "I.S.", "AU_PPB"] <- "I.S."

  data[data$ORDEM %in% para_mudar$ORDEM, "AU_PPB"] <- para_mudar$AU_PPB

  data <- data %>% dplyr::select(-"ORDEM")
  out[[1]] <- data

  data[data == "ND"] <- NA
  data[data == "N.A."] <- NA
  data[data == "I.S."] <- NA
  data[data == "I,S,"] <- NA
  data[data == ""] <- NA
  data[data == "0"] <- NA
  data[data == "NA"] <- NA
  out[[2]] <- data
unique(data$AU_PPB)
df_sc <- data[,11:(ncol(data))]
df_sc_transf <- data.frame(lapply(df_sc, function(x) {
  gsub(",", ".", x)
}))
df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub("<", "-", x)
}))

df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub(">", "", x)
}))
df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub(" ", "", x)
}))

df_sc_transf <- data.frame(data[,1:10], df_sc_transf)

df_sc_transf[,11:ncol(df_sc_transf)] <-
  lapply(df_sc_transf[,11:ncol(df_sc_transf)], function(x) {as.numeric(x)})

df_sc_05ld <- rgr::ltdl.fix.df(df_sc_transf[,-1:-10])
df_sc_05ld <- data.frame(df_sc_transf[1:10], df_sc_05ld)
out[[3]] <- df_sc_05ld

df_zero <- df_sc_transf[,11:ncol(df_sc_transf)]
df_zero[df_zero < 0] <- 0
df_zero[is.na(df_zero)] <- 0
df_zero <- data.frame(df_sc_transf[, 1:10], df_zero)
out[[4]] <- df_zero

return(out)
}
