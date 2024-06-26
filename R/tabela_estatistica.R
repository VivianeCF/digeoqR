#' Sumário Estatístico das análises químicas
#'
#' Gera planilhas cos os principais parâmetros estatísticos
#'
#' @param tipo_proc Modo de processamento: 1 = Todas amostras e
#'  2 = agrupadas por unidades
#' @param rotulo_lito Planilha gerada pela função intersecta_bacias com as
#' unidades litológicas dominantes em cada bacia.
#' @param base Lista de dados obtidos pela função prepara_base
#' @param lista_legenda Lista legendas obtida pela função prepara_legenda
#' @param dir_bol
#' @param dir_out
#'
#' @return
#' @export
#'
#' @examples
tabela_estatistica <-
  function(tipo_proc = 1,
           rotulo_lito,
           base, lista_legenda, dir_out){

    # Obter dados das análises químicas
     lst_pr <- list()
     lst_el <- list()
     out <- list()
     # Lê dados brutos
     data_b <- base[[1]]

     # Lê dados transformados
     data <- base[[2]]

    mylitho <- rotulo_lito

    mylegend <- lista_legenda[[2]]

    mylitho <- dplyr::inner_join(data, mylitho, by= "VALUE")


# colnames(mylitho)
mylitho <- mylitho[,c("VALUE","N_LAB","LONG_DEC", "LAT_DEC", "NUM_CAMPO",
                      'Area_bacia', "Geo_cod")]

data_b <-
  dplyr::inner_join(data_b,
                    mylitho[, c("N_LAB", "Area_bacia", "Geo_cod")],
                    by = "N_LAB")

data_b <- dplyr::left_join(data_b, mylegend, by = "Geo_cod")

data <-
  dplyr::inner_join(data,
                    mylitho[, c("VALUE", "Area_bacia", "Geo_cod")],
                    by = "VALUE")

data <- dplyr::left_join(data, mylegend, by = "Geo_cod")

# Configuração das condições de processamento
unidades <- unique(data_b$SIGLA)

mtds_grupos <- c("Geral", "Agrupado")

mtd_grupo <- mtds_grupos[tipo_proc]

if(mtd_grupo == "Geral") {
  s <- 1
} else {
  s <- 1:length(unidades)
}

for(j in s) {

  if(mtd_grupo == "Geral") {
    unidades[j] <- "Geral"
    data_bs <- data_b
    data_s <- data

  } else{
    data_bs <- data_b[data_b$SIGLA == unidades[j], ]
    data_s <- data[data$SIGLA == unidades[j], ]
  }


  # options(OutDec= ",")
  info_bol <- base[[5]]
  # seleciona elementos da planilha de dados
  # Criar vetor com sigla dos elementos
  elementos <- info_bol$analito
  un <- info_bol$unidades
  nomes_analitos <- paste0(elementos, "_", un)
  select <- data_s[ ,nomes_analitos]
  select_bruto <- data_bs[ ,nomes_analitos]
  select_info_bol <- as.data.frame(info_bol[info_bol$analito %in% elementos,])

  # Log-transformação dos dados

  logselect <- log10(select)

  #Declara referencias
  # (i) quantas colunas no data.frame
  n <- length(elementos)
  # (ii) vetor para armazenar resultados
  MD <- numeric(n)
  MDN <- numeric(n)
  DP <- numeric(n)
  CV <- numeric(n)
  MIN <- numeric(n)
  Q1 <- numeric(n)
  Q3 <- numeric(n)
  LS1 <- numeric(n)
  LS2 <- numeric(n)
  LS3 <- numeric(n)
  LI1 <- numeric(n)
  LI2 <- numeric(n)
  LI3 <- numeric(n)
  MAX <- numeric(n)
  UCC <- numeric(n)
  LD <- numeric(n)
  PLD <- numeric(n)
  NO <- numeric(n)
  MAD <- numeric(n)
  SWs <- numeric(n)
  SWp <- numeric(n)
  # (iii) nomeando vetor com nomes das colunas
  names(MD) <- elementos
  names(MDN) <- elementos
  names(DP) <- elementos
  names(CV) <- elementos
  names(MIN) <- elementos
  names(Q1) <- elementos
  names(Q3) <- elementos
  names(LS1) <- elementos
  names(LS2) <- elementos
  names(LS3) <- elementos
  names(LI1) <- elementos
  names(LI2) <- elementos
  names(LI3) <- elementos
  names(MAX) <- elementos
  names(PLD) <- elementos
  names(NO) <- elementos
  names(MAD) <- elementos
  names(SWs) <- elementos
  names(SWp) <- elementos

    dig <- select_info_bol$DIG

  # (iv) loop para cada coluna
  # Limpeza da tabela
  ## Tirar valores < MIN ou valores > MAX
  for(i in seq_along(select)){
    LD[i] <- round(select_info_bol[i, "MDL"], dig[i])
    LD[i] <- ifelse(LD[i] == 0, NA, LD[i])
    MD[i] <- round(mean(select[,i]), dig[i])
    MDN[i] <- round(10^(median(logselect[,i])), dig[i])
    DP[i] <- round(sd(select[,i]), dig[i])
    CV[i] <- round(DP[i]/MD[i], dig[i])
    MIN[i] <- ifelse(length(unique(select[,i])) == 1, unique(select[,i]),
                     round(min(select[,i],na.rm = TRUE), dig[i]))
    MIN[i] <- ifelse(MIN[i]<LD[i], paste0("<", LD[i]), MIN[i])
    Q1[i] <- round(10^(quantile(logselect[,i], c(.25),na.rm = TRUE)), dig[i])
    Q3[i] <- round(10^(quantile(logselect[,i], c(.75),na.rm = TRUE)), dig[i])
    MAX[i] <- max(select[,i])
    MAX[i] <- ifelse(MAX[i] == 0, LD[i]*1.2, MAX[i])
    LS1[i] <- round(10^(quantile(logselect[,i], c(.75),na.rm = TRUE)
                        + 4.5 * (quantile(logselect[,i], c(.75), na.rm = TRUE)
                                 - quantile(logselect[,i], c(.25), na.rm = TRUE))), dig[i])
    LS1[i] <-  ifelse(LS1[i] > MAX[i], NA, LS1[i])
    LS1[i] <-  ifelse(LS1[i] == 0, NA, LS1[i])
    LS2[i] <- round(10^(quantile(logselect[,i], c(.75),na.rm = TRUE)
                        + 3 * (quantile(logselect[,i], c(.75),na.rm = TRUE)
                               - quantile(logselect[,i], c(.25),na.rm = TRUE))), dig[i])
    LS2[i] <- ifelse(LS2[i] > MAX[i] , NA, LS2[i])
    LS2[i] <- ifelse(LS2[i] == 0 , NA, LS2[i])
    LS3[i] <- round(10^(quantile(logselect[,i], c(.75),na.rm = TRUE)
                        + 1.5 * (quantile(logselect[,i], c(.75),na.rm = TRUE)
                                 - quantile(logselect[,i], c(.25),na.rm = TRUE))), dig[i])
    LS3[i] <- ifelse(LS3[i] > MAX[i] , NA, LS3[i])
    LS3[i] <- ifelse(LS3[i] == 0 , NA, LS3[i])
    LI1[i] <- round(10^(quantile(logselect[,i], c(.25),na.rm = TRUE)
                        - 4.5 * (quantile(logselect[,i], c(.75),na.rm = TRUE)
                                 - quantile(logselect[,i], c(.25),na.rm = TRUE))), dig[i])
    LI1[i] <- ifelse(LI1[i] < MIN[i], NA, LI1[i])
    LI1[i] <- ifelse(LI1[i] == 0, NA, LI1[i])
    LI2[i] <- round(10^(quantile(logselect[,i], c(.25),na.rm = TRUE)
                        - 3 * (quantile(logselect[,i], c(.75),na.rm = TRUE)
                               - quantile(logselect[,i], c(.25),na.rm = TRUE))),
                    dig[i])
    LI2[i] <- ifelse(LI2[i] < MIN[i], NA, LI2[i])
    LI2[i] <- ifelse(LI2[i] == 0, NA, LI2[i])
    LI3[i] <- round(10^(quantile(logselect[,i], c(.25),na.rm = TRUE)
                        - 1.5 * (quantile(logselect[,i], c(.75),na.rm = TRUE)
                                 - quantile(logselect[,i], c(.25),na.rm = TRUE))), dig[i])
    LI3[i] <- ifelse(LI3[i] < MIN[i], NA, LI3[i])
    LI3[i] <- ifelse(LI3[i] == 0, NA, LI3[i])
    UCC[i] <- round(select_info_bol[i, "UCC"], digits = dig[i])
    UCC[i] <- ifelse(UCC[i] == 0, NA, UCC[i])
    MAD[i] <- round(10^mad(logselect[,i], na.rm = TRUE), dig[i])
    MAD[i] <- ifelse(MAD[i] == 0, NA, MAD[i])
    SWs[i] <- ifelse(length(unique(logselect[,i])) < 4, NA,
                     round(shapiro.test(logselect[,i])$statistic, 3))
    SWp[i] <- ifelse(length(unique(logselect[,i])) < 4, NA,
                     round(shapiro.test(logselect[,i])$p.value, 3))

  }

  #Prepara para calculo missing
  df <- as.data.frame(lapply(select_bruto,gsub,pattern="<",replacement="-"))
  df <- as.data.frame(lapply(df,gsub,pattern=",",replacement="."))
  df <- as.data.frame(lapply(df,gsub,pattern=">",replacement=""))

  x <- type.convert(df, as.is = TRUE)

  # calculo dos valores qualificados e valores reais
  xnew <- compositions::simulateMissings(x)
  miss <- compositions::missingSummary(xnew)
  NV <- miss[ ,1:1]
  NQ <- miss[ ,2:2]
  NO <- NV + NQ
  NV[NV == 0] <- ""
  UN <- select_info_bol$unidades
  PLD <- round(NQ/NO*100,0)
  EL <- select_info_bol$analito
  AREA <- rep(unidades, length(EL))
  #Cria a tabela de dados
  df_tb <- data.frame(EL, UN, LD, NO, NQ, PLD, MD, DP,
                      CV, MIN, LI1, LI2, LI3, Q1, MDN, MAD, Q3,
                      LS3, LS2, LS1, MAX, UCC, SWs,SWp) # adicionar MAD

  df_tb <- df_tb[df_tb$PLD<30, ]
  df_tb <- data.frame(lapply(df_tb, function(x) {
    gsub("NA", "-", x)
  }))
  # unique(is.na(df_tb))
  #tabela elementar

  n_st_desc <- c("Limite de detecção (LD)", "Número de amostras",
                 "Valores abaixo do LD", "Percentual de valores abaixo do LD",
                 "Coeficiente de variação", "Mínimo", "Média aritmética",
                 "Mediana", "Máximo")

  df_tb_t <- t(df_tb)

  #Salvando sumários
  write.table(df_tb, file = paste0(dir_out,
                             "sum_stat_param_",mtd_grupo, "_",
                             unidades[j],".csv"), na = "",
        row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ";")
  write.table(df_tb_t, file = paste0(dir_out,
                                     "sum_stat_el_",mtd_grupo, "_",
                                     unidades[j] ,".csv"), na = "",
        row.names = TRUE, col.names = FALSE, quote = FALSE, sep = ";")
  lst_pr[[j]] <- data.frame(rep(unidades[j], nrow(df_tb)), df_tb )
  lst_el[[j]] <- df_tb_t

  title <- grid::textGrob(unidades[j] ,gp=grid::gpar(fontsize=20))
  df_tb[is.na(df_tb)] <- ""

  m <- df_tb

  png(filename = paste0(dir_out,"sum_stat_todos_",mtd_grupo, "_", unidades[j], ".png"),
      units = "cm", width=35,height=35,bg = "white", res = 300)


  mytheme <-
    gridExtra::ttheme_default(core = list(fg_params = list(fontsize=7,
                                                           col = "black")),
                              colhead = list(fg_params = list(fontsize=7,
                                                              fontface="bold"))
  )
  mytheme[["core"]][["bg_params"]][["fill"]] = c("white", "white")
  mytheme[["core"]][["bg_params"]][["col"]] = "black"
  mytheme[["colhead"]][["bg_params"]][["col"]] = "black"

  t1 <- gridExtra::tableGrob(m,  rows=NULL)

  # # title <- textGrob("Geral" , gp=gpar(fontsize=20))
  padding <- ggplot2::unit(5,"mm")
  # library(gtable)
  table <- gtable::gtable_add_rows(
    t1,
    heights = grid::grobHeight(title) + padding,
    pos = 0)
  table <- gtable::gtable_add_grob(
    table,
    title,
    1, 1, 1, ncol(table))

  grid::grid.newpage()

  grid::grid.draw(table)

  dev.off()

  png(filename = paste0(dir_out,"sum_stat_resum_",mtd_grupo, "_",
                        unidades[j], ".png"), units = "cm", width = 11,
                        height = 35, bg = "white", res = 300)
  m_resum <- m[, c("EL", "UN", "MIN", "MAX", "MD", "DP", "SWs", "SWp")]
  mytheme <- gridExtra::ttheme_default(core = list(fg_params = list(
    fontsize=7, col = "black")),
    colhead = list(fg_params = list(fontsize=7,fontface="bold") )
  )
  mytheme[["core"]][["bg_params"]][["fill"]] = c("white", "white")
  mytheme[["core"]][["bg_params"]][["col"]] = "black"
  mytheme[["colhead"]][["bg_params"]][["col"]] = "black"

  t1 <- gridExtra::tableGrob(m_resum,  rows=NULL)

  padding <- ggplot2::unit(5,"mm")

  table <- gtable::gtable_add_rows(
    t1,
    heights = grid::grobHeight(title) + padding,
    pos = 0)
  table <- gtable::gtable_add_grob(
    table,
    title,
    1, 1, 1, ncol(table))

  grid::grid.newpage()

  grid::grid.draw(table)

  dev.off()

}

# Criar os dataframes com a classificação ====
base_dados <- do.call(rbind, lst_pr)
colnames(base_dados)[1] <- "Unidade"
base_dados <- base_dados[!is.na(base_dados$Unidade),]
transp_base <- t(base_dados)

# por elementos
write.csv2(transp_base, paste0(dir_out, mtd_grupo, "_",
                                "tabela_descr_pg.csv"))
out[[1]] <- transp_base

# por parâmetros
write.csv2(base_dados, paste0(dir_out, mtd_grupo, "_",
                              "tabela_descr_prm_pg.csv"))
out[[2]] <- base_dados
names(out) <- c("tabela descr pg", "tabela descr prm pg")
return(out)
}
