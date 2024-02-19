#' Sumário Estatístico das análises químicas
#'
#' Gera planilhas cos os principais parâmetros estatísticos
#'
#' @param tipo Modo de processamento: 1 = Todas amaostras e
#'  2 = agrupadas por unidades
#' @param rotulo_lito Planilha gerada pela função intersecta_bacias com as
#' unidades litológicas dominantes em cada bacia.
#' @param estacao Dados de campo das estações amostradas (shp)
#' @param ref_ucc Planilha com valores da concentração média da crosta superior
#' Rudnick e Gao 2004
#' @param dir_bol Diretório dos arquivos dos boletins
#' @param classe_am Classe de amostra
#' @param analise tipo de análise 1 = mineralógica e 2 = química
#' @param dir_base Diretório da base de dados de campo
#' @param tipo_base Tipo de base de campo 1 = FCAMP, 2 = SURVEY123 e 3 = QFIELD
#' @param base_campo Nome do arquivo da base de campo (sem extenção)
#' @param dir_out Diretório de saída dados de campo e estações
#'
#' @return
#' @export
#'
#' @examples
tabela_estatistica <-
  function(tipo = 1,
           rotulo_lito = "outputs/mylitho.csv",
           estacao = "outputs/estacoes.shp",
           ref_ucc = "inputs/quimica/ucc.csv",
           dir_bol = "inputs/quimica/S/",
           classe_am = 2,
           analise = 2,
           dir_base = "inputs/campo/",
           tipo_base = 1,
           base_campo = "fcampo", dir_out = "outputs/"){

    # Obter dados das análises químicas
     dados_bol <-  prepara_bases(dir_bol,
                                 classe_am, analise,
                                 dir_base, tipo_base,
                                 base_campo, dir_out )
     lst_pr <- list()
     lst_el <- list()
     out <- list()
     # Lê dados brutos
     data_b <- dados_bol[[1]]

     # Lê dados transformados
     data <- dados_bol[[2]]

     # Lê condições analíticas
     ref =  dados_bol[[5]]
     ref = unique(ref[, c("analito", "unidades", "MDL")])
     ref$MDL <- as.numeric(gsub(",", ".",  ref$MDL))
     # Lê UCC dos elementos
     ucc <- read.csv2(ref_ucc)
     ref$nome_analito <- paste0(ref$analito,"_", ref$unidades)
     ucc$nome_analito <- paste0(ucc$Elemento,"_", ucc$Unidade)
     ref <- dplyr::left_join(ref, ucc[, c("nome_analito", "UCC")], by = "nome_analito")
     count_decimals = function(x) {
       #length zero input
       if (length(x) == 0) return(numeric())

      # Conta casas decimais
       x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
       x_int = floor(x) %>% abs() %>% nchar()
       x_nchr = x_nchr - 1 - x_int
       x_nchr[x_nchr < 0] = 0

       x_nchr
     }

    ref$DIG <- count_decimals(ref$MDL)
    df <- ref  %>% dplyr::group_by(nome_analito) %>% dplyr::summarize(min = min(MDL))
    df <- dplyr::left_join(df, ref,  by = "nome_analito")
    df$MDL <- df$min
    ref <- df %>% dplyr::select(-"min")

    mylitho <- read.csv2(rotulo_lito)
    # mydata <- sf::read_sf(dados_campo)
    # mydata <- as.data.frame(mydata)

    lista_legenda <- prepara_legenda()
    mylegend <- lista_legenda[[2]]

    mylitho <- dplyr::inner_join(data, mylitho, by= "VALUE")


# colnames(mylitho)
mylitho <- mylitho[,c("VALUE","N_LAB","LONG_DEC", "LAT_DEC", "NUM_CAMPO",
                      'Area_bacia', "Geo_cod")]

# Configuração das condições de processamento


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

unidades <- unique(data_b$SIGLA)

mtds_grupos <- c("Geral", "Agrupado")

mtd_grupo <- mtds_grupos[tipo]

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


  options(OutDec= ",")

  # seleciona elementos da planilha de dados
  # Criar vetor com sigla dos elementos
  elementos <- ref$analito
  un <- ref$unidades
  nomes_analitos <- paste0(elementos, "_", un)
  select <- data_s[ ,nomes_analitos]
  select_bruto <- data_bs[ ,nomes_analitos]
  select_ref <- as.data.frame(ref[ref$analito %in% elementos,])

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
  # dig <- rep(0, nrow(select_ref))
  dig <- select_ref$DIG

  # (iv) loop para cada coluna
  # Limpeza da tabela
  ## Tirar valores < MIN ou valores > MAX
  for(i in seq_along(select)){
    LD[i] <- round(select_ref[i, "MDL"], dig[i])
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
    UCC[i] <- round(select_ref[i, "UCC"], digits = dig[i])
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
  UN <- select_ref$unidades
  PLD <- round(NQ/NO*100,0)
  EL <- select_ref$analito
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
  write.table(df_tb, file = paste0("outputs/",
                             mtd_grupo,"/sum_stat_param_",
                             unidades[j],".csv"), na = "",
        row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ";")
  write.table(df_tb_t, file = paste0("outputs/",
                                     mtd_grupo,"/sum_stat_el_",
                                     unidades[j] ,".csv"), na = "",
        row.names = TRUE, col.names = FALSE, quote = FALSE, sep = ";")
  lst_pr[[j]] <- data.frame(rep(unidades[j], nrow(df_tb)), df_tb )
  lst_el[[j]] <- df_tb_t

  title <- grid::textGrob(unidades[j] ,gp=grid::gpar(fontsize=20))
  df_tb[is.na(df_tb)] <- ""

  m <- df_tb

  png(filename = paste0("outputs/",mtd_grupo,"/sum_stat_todos_", unidades[j], ".png"),
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

  png(filename = paste0("outputs/",mtd_grupo,"/sum_stat_resum_",
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
write.csv2(transp_base, paste0("outputs/", mtd_grupo, "/",
                                "tabela_descr_pg.csv"))
out[[1]] <- transp_base

# por parâmetros
write.csv2(base_dados, paste0("outputs/",mtd_grupo, "/",
                              "tabela_descr_prm_pg.csv"))
out[[2]] <- base_dados
names(out) <- c("tabela descr pg", "tabela descr prm pg")
return(out)
}
