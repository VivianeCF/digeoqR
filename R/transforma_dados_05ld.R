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
# require(rgr)
out <- list()
data$Lab_orig <- data$Lab
data <-  data %>% dplyr::select(!Lab)
bb_lab <- data.frame(Lab_orig = unique(data$Lab),
                     Lab =c("EE", "AA", "AA", "AA", "AA", "AA", "IE", "COL",
                            "CR", ""))
data <-  dplyr::left_join(bb_lab, data, by = "Lab_orig")
data <- data %>% dplyr::relocate(c(Lab, Lab_orig), .after = Classe)
unique(data$Au_ppm)
# Importar informações faltantes do Au ppb do Au_ppm e converter
if(!"Au_ppb" %in% colnames(data)){
  data$Au_ppb <- data$Au_ppm
}
  data$Au_ppb <- data$Au_ppm
  data[is.na(data$Au_ppb), "Au_ppb" ] <-
    gsub("< 0,1", "< 100", data$Au_ppm, fixed = TRUE)
  data[is.na(data$Au_ppb), "Au_ppb" ] <-
    gsub("< 0,2", "< 200", data$Au_ppm, fixed = TRUE)
  data[is.na(data$Au_ppb), "Au_ppb" ] <-
    gsub( "< 0,25", "< 250", data$Au_ppm, fixed = TRUE)
  data[is.na(data$Au_ppb), "Au_ppb" ] <-
    gsub( "< 0,05", "< 50", data$Au_ppm, fixed = TRUE)
  data[data == "ND"] <- NA
  data[data == "N.A."] <- NA
  data[data == "I.S."] <- NA
  data[data == ""] <- NA
  data[data == "0"] <- NA

df_sc <- data[,12:(ncol(data))]
df_sc_transf <- data.frame(lapply(df_sc, function(x) {
  gsub(",", ".", x)
}))
df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub("< ", "-", x)
}))

df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
  gsub("> ", "", x)
}))

df_sc_transf <- data.frame(data[,1:11], df_sc_transf)
colnames(df_sc_transf)

df_sc_transf[,12:ncol(df_sc_transf)] <-
  lapply(df_sc_transf[,12:ncol(df_sc_transf)], function(x) {as.numeric(x)})

# str(df_sc_transf)
# unique(is.na(df_sc_transf$PROJETO))
# unique(df_sc_transf$Au_ppm)
df_sc_05ld <- rgr::ltdl.fix.df(df_sc_transf[,-1:-11])
df_sc_05ld <- data.frame(df_sc_transf[1:11], df_sc_05ld)
out[[1]] <- df_sc_05ld
df_zero <- df_sc_transf
df_zero[df_zero < 0] <- 0
df_zero[is.na(df_zero)] <- 0

out[[2]] <- df_zero
# colnames(df_new)
select <- colnames(df_sc_05ld)
df_new_select <- data[, select]

out[[3]] <- df_new_select

return(out)
}
