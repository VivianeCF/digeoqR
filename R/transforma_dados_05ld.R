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
data <-  data %>% dplyr::select(!Lab)
bb_lab <- data.frame(Lab_orig = unique(data$Lab),
                     Lab =c("EE", "AA", "AA", "AA", "AA", "AA", "IE", "COL",
                            "CR", ""))
data <-  dplyr::left_join(bb_lab, data, by = "Lab_orig")
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
  para_mudar$value <- as.numeric(para_mudar$value)*1000
  para_mudar$AU_PPB <- paste0(para_mudar$Q, para_mudar$value)
  unique(para_mudar$AU_PPB)
  para_mudar[para_mudar$AU_PPM == "ND", "AU_PPB"] <- "ND"
  para_mudar[para_mudar$AU_PPM == "I.S.", "AU_PPB"] <- "I.S."

  data[data$ORDEM %in% para_mudar$ORDEM, "AU_PPB"] <- para_mudar$AU_PPB

  data <- data %>% dplyr::select(-"ORDEM")
  out[[1]] <- data

  data[data == "ND"] <- NA
  data[data == "N.A."] <- NA
  data[data == "I.S."] <- NA
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

df_sc_transf <- data.frame(data[,1:10], df_sc_transf)

df_sc_transf[,11:ncol(df_sc_transf)] <-
  lapply(df_sc_transf[,11:ncol(df_sc_transf)], function(x) {as.numeric(x)})

df_sc_05ld <- rgr::ltdl.fix.df(df_sc_transf[,-1:-10])
df_sc_05ld <- data.frame(df_sc_transf[1:10], df_sc_05ld)
out[[3]] <- df_sc_05ld
df_zero <- df_sc_transf
df_zero[df_zero < 0] <- 0
df_zero[is.na(df_zero)] <- 0

out[[4]] <- df_zero



return(out)
}
