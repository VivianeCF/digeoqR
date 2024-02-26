#' Métodos de classificação de dados geoquímicos
#'
#' @param tipo_proc Tipo de processamento 1 = geral 2 = por unidades
#' @param un_val Unidades válidas
#' @param elem_val Elementos válidos
#' @param base Base de dados preparada
#' @param list_break Define o número de quebra para cada classificação
#'
#' @return
#' @export
#'
#' @examples
classifica_metodos <- function(tipo_proc, un_val, elem_val,
                               base, list_break = c(4,4,4)) {
  if(tipo_proc == 1){
    unidades == "geral" } else {
    unidades <- un_val}

  elementos <- elem_val
  mydata <- base[[2]]

 ## Classificação - MAD
  for (j in seq(elem_val)) {
    for (i in seq(un_val)) {
      if(tipo_proc == 1){
        el <- mydata[, elementos[j]]
        id <- mydata[,"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], nrow(el))
      }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], nrow(el))
      }

      classe[[j]] <- data.frame(id, unit ,analito, class=log_class_mad(el))
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
        analito <- rep(elementos[j], nrow(el))
        }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], nrow(el))
       }

      classe[[j]] <- data.frame(id, unit ,analito, class=log_class_bxp(el))
    }
  }
  #Criar o dataframe com a classificação
  out[[2]] <- do.call(rbind, classe)

  # Método C-A
  for (j in seq(elem_val)) {
    for (i in seq(un_val)) {
      if(tipo_proc == 1){
        el <- mydata[, elementos[j]]
        id <- mydata[,"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], nrow(el))
      }else{
        el <- mydata[mydata$Geo_cod == unidades[i],elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i],"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], nrow(el))
      }

      classe[[j]] <- data.frame(id, unit ,analito, class=log_class_ca(el))
    }
  }
  #Criar o dataframe com a classificação
  out[[3]] <- do.call(rbind, classe)

}
