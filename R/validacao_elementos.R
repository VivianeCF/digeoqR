
#' Validação dos elementos químicos analisados
#'
#' @param dir_out Diretório de saída
#' @param base Lista obtida da função prepara_bases
#' @param corte_val Critério de corte do percentual de dados censurados
#'
#' @return
#' @export
#'
#' @examples
validacao_elementos <- function(dir_out, base, corte_val){
## Calcula % dos dados validos
mydata_b <- base[["dados brutos"]]
ref <- base[["condições analíticas"]]
elementos <- paste0(ref$analito, "_", ref$unidades)
out <- 0
n <- 0
ncoded <- 0
nneg <- 0
nna <- 0
coded <- -9999
xmat <- mydata_b[,elementos]
nvars <- dim(xmat)[2]
vars <- seq(1:nvars)
for (i in 1:nvars) {
  ii <- vars[i]
  x <- xmat[, ii]
  n[i] <- length(x)
  ncoded[i] <- length(x[!is.na(x) & x == coded])
  nna[i] <- length(x[is.na(x)])
  nneg[i] <- length(x[x < 0]) - nna[i] - ncoded[i]

}
elementos <- colnames(xmat)
stat_fix <- cbind.data.frame(elementos,n ,nna, nneg, ncoded)
nval_ppc <- 100-round((stat_fix$nna + stat_fix$nneg)/n*100,0)
stat_fix <- cbind.data.frame(stat_fix, nval_ppc)

# ## Listar elementos com menos de 30% qualificados
filtro_nval <- stat_fix %>% dplyr::filter(nval_ppc > corte_val)
elem_val <- filtro_nval$elementos
out[[1]] <- stat_fix
write.table(stat_fix, paste0(dir_out, "validacao_elementos.csv"),
            sep = ";", dec = ",", row.names = FALSE)

#T
out[[2]] <- elem_val
names(out) <- c("validação dos elementos", "elementos validados")
return(out)
}
