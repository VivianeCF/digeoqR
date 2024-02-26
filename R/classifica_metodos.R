#' Métodos de classificação de dados geoquímicos
#'
#' @param un_val
#' @param elem_val
#' @param base
#' @param tipo_class
#'
#' @return
#' @export
#'
#' @examples
classifica_metodos <- function(tipo_class, un_val, elem_val, base, list_break) {
  unidades <- un_val
  elementos <- elem_val
  mydata <- base[[2]]

 ## Classificação - MAD
  if(tipo_class == 1){
  # Criar a lista e o dataframe dos dados
  res <- list()
  df <- list()
  # Calcular as classes boxplot por elemento e por unidade
  for (ul in seq(unidades)) {
  for(i in seq(elementos)) {
    select <- mydata[mydata$Geo_cod == unidades[ul], ]
    eq <- select[,elementos[i]]
    df[[i]] <- data.frame(EL=elementos[i], Unidade=unidades[ul],
                          Limiar1 = 10^(median(log10(eq)) - 2*mad(log10(eq))),
                          Limiar2 =10^(median(log10(eq)) - mad(log10(eq))),
                          Limiar3 =10^(median(log10(eq)) + mad(log10(eq))),
                          Limiar4 = 10^(median(log10(eq)) + 2*mad(log10(eq)))) }
    res[[ul]] <- do.call(rbind, df)
  }
  metodo_mad <- do.call(rbind, res)
}
## Classificação TIF
if(tipo_class == 2){
  res <- list()
  df <- list()
  for (ul in seq(unidades)) {
    for(i in seq(elementos)) {
      select <- mydata[mydata$Geo_cod == unidades[ul], ]
      eq <- select[,elementos[i]]
      df[[i]] <- data.frame(EL=elementos[i], Unidade=unidades[ul],
                            Limiar1 = 10^(quantile(log10(eq),0.25, names = FALSE)
                                          - 1.5*(quantile(log10(eq),0.75, names = FALSE)
                                                 - quantile(log10(eq),0.25, names = FALSE))),
                            Limiar2 = 10^(quantile(log10(eq),0.25, names = FALSE)),
                            Limiar3 = 10^(quantile(log10(eq),0.75, names = FALSE)),
                            Limiar4 = 10^(quantile(log10(eq),0.75, names = FALSE)
                                          + 1.5*(quantile(log10(eq),0.75, names = FALSE)
                                                 - quantile(log10(eq),0.25, names = FALSE))))


    }
    res[[ul]] <- do.call(rbind, df)
  }
  metodo_tif <- do.call(rbind, res)

}
# Classificação - C-A
    if(tipo_class == 3){
      #Decclara variáveis
      b <- as.list(NA)
      k <- as.list(NA)
      p <- as.list(NA)
      pu <- as.list(NA)
      df_cn <- as.list(NA)
      df_cn_todos <- as.list(NA)
      df_cnclass <- as.list(NA)
      df_cnclass_todos <- as.list(NA)
      for(j in 1:length(unidades)) {
        for(i in 1:length(elementos)) {
          elemento <- elementos[i]
          unidade <- myjob[rownames(myjob)==elementos[i], "UN"]
          df <- mydata[mydata$Unidade == unidades[j],]
          df <- df[order(df[, elementos[i]], decreasing = TRUE),] %>%
            mutate(cum_area = cumsum(area_km2))
          df_pivot <- tidyr::pivot_longer(df, cols = 10:24, names_to = "elemento",
                                          values_to = "value")

          df_pivot_filtrado <- df_pivot[df_pivot$elemento==elementos[i],]
          # df_sum <- df_pivot_filtrado
          f = nrow(df_pivot_filtrado)

          if(nrow(df_pivot_filtrado) > 200){
            f=55
            }
          fator <- round(nrow(df_pivot_filtrado)/f)
          df_pivot_filtrado$grupo_index <- c(0, rep(1:(nrow(df_pivot_filtrado)-1)%/%fator))
          df_sum <- group_by(df_pivot_filtrado, grupo_index) %>%
            summarise(mean_value = mean(value), mean_area = mean(cum_area))

          df_limpo <- df_sum[!duplicated(df_sum$mean_value),]

          colnames(df_limpo)[2:3] <- c("value", "cum_area")

          library(segmented)
          y<-round(log10(df_limpo$cum_area),3)
          x<-round(log10(df_limpo$value),3)

          model <- data.frame(x,y)
          model <- model[!duplicated(model$x),]
          x <- model$x
          y <- model$y
          plot(x,y)
          o <- lm(y ~ x)

          o.seg <- segmented(o, seg.Z=~x, npsi=4)
          if(nrow(df_limpo) < 50 & nrow(df_limpo) > 30){

            o.seg <- segmented(o, seg.Z=~x, npsi=3)

          }
          if(nrow(df_limpo) <= 30 & nrow(df_limpo) >= 20){

            o.seg <- segmented(o, seg.Z=~x, npsi=2)

          }

          if(nrow(df_limpo) < 20){

            o.seg <- segmented(o, seg.Z=~x, npsi=1)

          }

          os <- update(o.seg, control=seg.control(it.max=100, display=TRUE))
          # os <- o.seg

          X <- (model$x)
          Y <- (model$y)
          DF <- data.frame(X, Y)
          DF <- DF[!duplicated(DF$X),]
          breaks <- data.frame(breaks=round(10^(os[["indexU"]][["x"]]), 2))
          class <- cut(DF$X, c(Inf,log10((breaks[,1])), -Inf), labels = FALSE, right = TRUE)
          DF <- data.frame(DF, class)
          ##Arrumar aqui 24/12/22

          ca_class <- cut(df_pivot_filtrado$value, c(Inf,(breaks[,1]), -Inf),
                          labels = FALSE, right = TRUE)
          df <- data.frame(ID = df_pivot_filtrado$ID, elemento = df_pivot_filtrado$elemento, ca_class,
                           value= df_pivot_filtrado$value,
                           cum_count = df_pivot_filtrado$cum_area)
          df_cn_todos[[i]] <- df



          b[[i]] <- data.frame(lm1=breaks[1,], lm2=breaks[2,], lm3=breaks[3,], lm4=breaks[4,],Elemento=elementos[i], Unidade = unidades[j])
          df_cn[[i]]  <- data.frame(DF, fator= rep(elementos[i], nrow(DF)), Geo_cod = rep(j, nrow(DF)))

          k[[j]] <- do.call(rbind, b)

        }
        df_cnclass[[j]] <- do.call(rbind, df_cn)
        df_cnclass_todos[[j]] <- do.call(rbind, df_cn_todos)

      }
      dados_limiares_ca <- do.call(rbind, k)


    }
}
