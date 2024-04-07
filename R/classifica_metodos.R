#' Métodos de classificação de dados geoquímicos
#'
#' @param tipo_proc Tipo de processamento 1 = geral 2 = por unidades
#' @param elem_val Elementos válidos
#' @param base Base de dados preparada
#' @param lito_bacia
#' @param nbc
#' @param mtd_class
#'
#' @return
#' @export
#'
#' @examples
classifica_metodos <- function(tipo_proc, mtd_class, lito_bacia, elem_val,
                               base, nbc = 10) {
  out <- data.frame()
  df_list <- list()
  #carrega bases
  mydata <-  base[[2]]
  myjob <- base[[5]]

    # Criar vetor das unidades
  mylitho_max <- lito_bacia[[2]]

  unidades <- unique(mylitho_max$Geo_cod)

  elementos <- elem_val

  mydata <- dplyr::left_join(mydata, mylitho_max, by = "VALUE")

  t <- data.frame(table(mylitho_max$Geo_cod))

  un_val <- as.numeric(as.character(t[t$Freq > nbc,"Var1"]))

  if(tipo_proc == 1){
    un_val <- "geral"
    unidades <- "geral"
    } else {
    unidades <- un_val
    }
classe <- list()

 ## Classificação - MAD
if(mtd_class == 1){
  for (j in seq(elementos)) {
    for (i in seq(unidades)) {
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

      df <- data.frame(id, unit ,analito, teor= el, metodo, class=log_class_mad(el))
      classe[[i]] <- df[!is.na(df$id),]
    }
      df_list[[j]] <- do.call(rbind, classe)
  }
#Criar o dataframe com a classificação
  out <- do.call(rbind, df_list)
  }

# Método TIF
if(mtd_class == 2){
  for (j in seq(elementos)) {
    for (i in seq(unidades)) {
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
        df <- data.frame(id, unit ,analito, teor= el, metodo, class=log_class_bxp(el))
        classe[[i]] <- df[!is.na(df$id), ]


       }
        df_list[[j]] <- do.call(rbind, classe)
         }
  #Criar o dataframe com a classificação
  out <- do.call(rbind,  df_list)
  }
  # Método C-A
if(mtd_class == 3){
  for (j in seq(elementos)){
    for (i in seq(unidades)){
      if(tipo_proc == 1){
        el <- mydata[ !is.na(mydata$Area_bacia), elementos[j]]
        id <- mydata[!is.na(mydata$Area_bacia),"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("C-A", length(el))
        area <-  mydata[!is.na(mydata$Area_bacia),"Area_bacia"]

      }else{
        el <- mydata[mydata$Geo_cod == unidades[i] & !is.na(mydata$Area_bacia),elementos[j]]
        id <- mydata[mydata$Geo_cod == unidades[i] & !is.na(mydata$Area_bacia),"VALUE"]
        unit <- rep(unidades[i], length(el))
        analito <- rep(elementos[j], length(el))
        metodo <-  rep("C-A", length(el))
        area <-  mydata[mydata$Geo_cod == unidades[i] & !is.na(mydata$Area_bacia),"Area_bacia"]

      }

      df <- data.frame(id, unit ,analito, teor= el, metodo, class=log_class_mad(el))
      classe[[i]] <- df[!is.na(df$id),]
    }
    df_list[[j]] <- do.call(rbind, classe)
  }
  # #Criar o dataframe com a classificação
  out <- do.call(rbind, df_list)
  }
return(out)
}
