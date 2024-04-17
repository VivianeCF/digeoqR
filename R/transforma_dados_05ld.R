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
