#' Testes Estatísticos
#'
#' Testes de normalidade e de comparação das amostras
#'
#' @param mtd_transf Método de transformação dos dados
#' 1 = sem transformação e 2 = logtransformados
#' @param nbc Número mímimo de bacias por unidade
#' @param base Lista de dados da função prepara_base
#' @param lito_bacia Resultado da função intercecta_bacia
#' @param leg legenda da feição
#' @param dir_out Diretório de saída
#' @param elem_val Elementos validados
#'
#' @return
#' @export
#'
#' @examples
testes_estatisticos <- function(dir_out, base, lito_bacia,
                                mtd_transf = 1, nbc = 10, leg, elem_val){
  lst_pr <- list()
  lst_el <- list()
  out <- list()
  # Lê arquivos
  ## Bacias modeladas pelo SRTM usando o projeto R MODEL_BACIAS
  mydata <-  base[[2]]
  mydata_b <-  base[[1]]
  myjob <- base[[5]]

  # Criar vetor das unidades
  mylitho_max <- lito_bacia[[2]]
  Geo_cod <- unique(mylitho_max$Geo_cod)

  elementos <- myjob$nome_analito
  mydata <- dplyr::left_join(mydata, mylitho_max, by = "VALUE")

  t <- data.frame(table(mylitho_max$Geo_cod))
  un_val <- as.numeric(as.character(t[t$Freq > nbc,"Var1"]))

  # Ordena as Unidades pelo código das unidades (Geo_cod)
  cod_unidades <- leg
  abrev <- cod_unidades$SIGLA
  nome_unidade <- cod_unidades$SIGLA
  lmtd <- "bruto"
  if(mtd_transf == 2){
    maydata <- data.frame(log10(mydata[ ,elem_val]),
                          Geo_cod = maydata$Geo_cod)
    lmtd <- "log"}

    ## Teste de normalidade ########################################################
    ##o valor p> 0,05 implica que a distribuição dos dados não é
    ## significativamente diferente da distribuição normal. Em outras
    ## palavras, podemos assumir a normalidade.

    ## Teste Shapiro_Wilk para os dados logtransformados
    sw <- list()

    for(un in un_val){
      print(un)
      dl <- mydata[mydata$Geo_cod == un ,elem_val]
      ps <- as.data.frame(do.call(rbind, lapply(dl, function(x)
        shapiro.test(x)[c("statistic", "p.value")])))

      df <- data.frame(pvalue=round(do.call(rbind,ps$p.value),3), round(round(do.call(rbind,ps$statistic), 3), 3),
                       unidade = rep(nome_unidade[un], length(elem_val)))

      colnames(df) <- c("p.value", "W")
      df$elemento <- rownames(df)
      sw[[un]] <- df
    }
    res_sw <- do.call(rbind,sw)
    colnames(res_sw)[3] <- "Unidade"
    res_sw <- res_sw[!is.na(res_sw$Unidade),]
    write.table(res_sw, paste0(dir_out, "test_norm_ShapiroWilk",
                               "_unidades", "_", lmtd,".csv"), sep = ";", dec = ",",
                row.names = FALSE)
    out[[1]] <- res_sw
    ## Teste Lilefors
    lf <- list()
    for(un in un_val){
      dl <- mydata[mydata$Geo_cod == un ,elem_val]
      pl <- as.data.frame(do.call(rbind, lapply(dl, function(x)
        nortest::lillie.test(x)[c("statistic", "p.value")])))

      df <- data.frame(pvalue=round(do.call(rbind,ps$p.value),3), round(do.call(rbind,ps$statistic),3),
                       unidade = rep(nome_unidade[un], length(elem_val)))
      colnames(df) <- c("p.value", "W")
      df$elemento <- rownames(df)
      lf[[un]] <- df
    }
    res_lf <- do.call(rbind,lf)
    colnames(res_lf)[3] <- "Unidade"
    write.table(res_lf, paste0(dir_out, "test_norm_Lillie",
                               "_unidades","_", lmtd, ".csv"),
                sep = ";", dec = ",", row.names = FALSE)
    out[[2]] <- res_lf

    # Teste para comparar amostras independentes ################################
    # Teste Wilcoxon não pareado ou de Mann-Whitney
    # É apresentado a estatística de teste "W", o p-valor e a hipótese alternativa.
    # Concluí-se que não há evidências para rejeitar H0 pois p−valor=0.5174 que por
    # sua vez é >0.05, ou seja, as duas amostras pertencem a mesma população.
    #############################################################################
    mydata_p <- mydata %>%
      tidyr::pivot_longer(cols = elementos, names_to = "elemento", values_to = "valor")
    mydata_p$Geo_cod <- as.factor(mydata_p$Geo_cod)
    unidades <- (unique(mydata$Geo_cod))
    unidades <- unidades[!is.na(unidades)]
    comb_u <- t(combn(unidades,2))

    # Teste wilcox não pareado - p-value
    res <- list()
    w <- 0
    df <- 0
    for(u in 1:nrow(comb_u)) {
      for (i in seq(elem_val)) {

        el1 <- mydata[mydata$Geo_cod == comb_u[u,1], elem_val[i]]
        el2 <- mydata[mydata$Geo_cod == comb_u[u,2], elem_val[i]]
        w  <- wilcox.test(el1, el2, correct = TRUE, paired=FALSE,exact=FALSE)
        df[i] <-  round(w$p.value, 3)

      }
      res[[u]] <- df
    }
    test <- data.frame(do.call(cbind, res))
    colnames(test) <- paste0("p-value - ", comb_u[,1],"-" ,comb_u[,2] )
    test$elemento <- elem_val
    # Elimina colunas só com NAs
    test <- test[,colSums(is.na(test))<nrow(test)]
    test <- na.omit(test)
    write.csv2(test, paste0(dir_out, "test_Wilcoxon_pvalue","_", lmtd,".csv"))
    out[[3]] <- test

    # Teste wilcox não pareado - statistic (W)
    w <- 0
    df <- 0
    res2 <- list()
    for(u in 1:nrow(comb_u)) {
      for (i in seq(elem_val)) {
        el1 <- mydata[mydata$Geo_cod == comb_u[u,1], elem_val[i]]
        el2 <- mydata[mydata$Geo_cod == comb_u[u,2], elem_val[i]]
        w  <- wilcox.test(el1, el2, correct = TRUE, paired=FALSE,exact=FALSE)
        df[i] <-  round(w$statistic, 3)

      }
      res2[[u]] <- df
    }

    test2 <- data.frame(do.call(cbind, res2))
    colnames(test2) <- paste0("W - ", comb_u[,1],"-" ,comb_u[,2] )
    test2$elemento <- elem_val
    # Elimina colunas só com NAs
    test2 <- test2[,colSums(is.na(test2)) < nrow(test2)]
    test2 <- na.omit(test2)
    out[[4]] <- test2
    write.csv2(test2, paste0(dir_out, "test_Wilcoxon_statistic","_",lmtd,  ".csv"))

    #Teste Shapiro Wilker##########################################################
    #Todas as amostras

    elementos <- myjob$nome_analito

    ## Elementos válidos
    lista_sum <- lapply(mydata[, elem_val], function(x) sum(duplicated(x)))

    dl <- mydata[ , elem_val]
    ps <- as.data.frame(do.call(rbind, lapply(dl, function(x)
      shapiro.test(x)[c("statistic", "p.value")])))

    df <- data.frame(pvalue=round(do.call(rbind,ps$p.value),3), round(do.call(rbind,ps$statistic),3))
    colnames(df) <- c("p.value", "W")
    df$elemento <- rownames(df)
    # Elimina colunas só com NAs
    df <- df[,colSums(is.na(df))<nrow(df)]
    df <- na.omit(df)
    out[[5]] <- df
    write.table(df, paste0(dir_out, "test_norm_ShapiroWilk",
                           "_Geral","_",lmtd ,".csv"), sep = ";", dec = ",", row.names = FALSE)

    #Teste Lillie-Fors############################################################
    dl <- mydata[ ,elem_val]
    ps <- as.data.frame(do.call(rbind, lapply(dl, function(x)
      nortest::lillie.test(x)[c("statistic", "p.value")])))

    df <- data.frame(pvalue=round(do.call(rbind,ps$p.value),3), round(do.call(rbind,ps$statistic),3))
    colnames(df) <- c("p.value", "W")
    df$elemento <- rownames(df)
    # Elimina colunas só com NAs
    df <- df[,colSums(is.na(df))<nrow(df)]
    df <- na.omit(df)
    out[[6]] <- df
    write.table(df, paste0(dir_out, "test_norm_Lillie",
                           "_Geral","_",lmtd, ".csv"), sep = ";", dec = ",",
                row.names = FALSE)
    names(out) <- c("test. norm. Shapiro-Wilk unidades",
                    "test. norm. Lillie unidades", "test. Wilcoxon pvalue",
                    "test. Wilcoxon statistic", "test. norm. Shapiro-Wilk geral",
                    "test. norm. Lillie geral")


  return(out)
}
