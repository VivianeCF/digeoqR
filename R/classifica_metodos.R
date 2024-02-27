#' Métodos de classificação de dados geoquímicos
#'
#' @param tipo_proc Tipo de processamento 1 = geral 2 = por unidades
#' @param elem_val Elementos válidos
#' @param base Base de dados preparada
#' @param lito_bacia
#' @param nbc
#'
#' @return
#' @export
#'
#' @examples
classifica_metodos <- function(tipo_proc, lito_bacia, elem_val,
                               base, nbc = 10) {
  out <- list()
  #carrega bases
  mydata <-  base[[2]]
  myjob <- base[[5]]

    # Criar vetor das unidades
  mylitho_max <- lito_bacia[[2]]
  unidades <- unique(mylitho_max$Geo_cod)

  elementos <- myjob$nome_analito

  mydata <- dplyr::left_join(mydata, mylitho_max, by = "VALUE")

  t <- data.frame(table(mylitho_max$Geo_cod))
  un_val <- as.numeric(as.character(t[t$Freq > nbc,"Var1"]))
  elementos <- elem_val

  if(tipo_proc == 1){
    un_val <- "geral"
    unidades <- "geral"
    } else {

    unidades <- un_val}
classe <- list()
 ## Classificação - MAD
  for (j in seq(elem_val)) {
    for (i in seq(un_val)) {
      if(tipo_proc == 1){
        el <- mydata[, elementos[j]]
        id <- mydata[,"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("MAD", length(el))
      }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("MAD", length(el))
      }

      classe[[j]] <- data.frame(id, unit ,analito, teor= el, metodo, class=log_class_mad(el))
    }
  }
  #Criar o dataframe com a classificação
  out[[1]] <- do.call(rbind, classe)
  # Método TIF
  for (j in seq(elem_val)) {
    for (i in seq(un_val)) {
      if(tipo_proc == 1){
        el <- mydata[, elementos[j]]
        id <- mydata[,"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("TIF", length(el))

        }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("TIF", length(el))

       }

      classe[[j]] <- data.frame(id, unit ,analito, teor= el, metodo, class=log_class_bxp(el))
    }
  }
  #Criar o dataframe com a classificação
  out[[2]] <- do.call(rbind, classe)
  #
  # Método C-A
  for (j in seq(elem_val)) {
    for (i in seq(un_val)) {
      if(tipo_proc == 1){
        el <- mydata[, elementos[j]]
        id <- mydata[,"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("C-A", length(el))
        area <-  mydata[,"Area_bacia"]

      }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("C-A", length(el))
        area <-  mydata[mydata$Geo_cod == unidades[i],"Area_bacia"]
      }

      classe[[j]] <- data.frame(id, unit ,analito, area, teor = el, metodo,class=log_class_ca(el, area))
    }
  }
  # #Criar o dataframe com a classificação
  out[[3]] <- do.call(rbind, classe)
return(out)
}
