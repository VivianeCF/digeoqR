#' Mapa geoquímico (TIF)
#'
#' @param bacias
#' @param geologia
#' @param info_bol
#' @param litho_max
#' @param dados_transf
#' @param leg
#' @param elem_val
#'
#' @return
#' @export
#'
#' @examples
mapa_tif_unidades <- function(bacias, geologia, dados_transf,
                     info_bol, litho_max, elem_val, leg )
  {

######################################################################
# Boxplot MAP
######################################################################

# Definição do formato numérico, vírgula decimal
options(OutDec = ",", scipen = 999)

# Carrega dados
## Bacias modeladas pelo SRTM pela função modela_bacias
bacias <-  bacias
## Polígonos das unidades
geologia <-  geologia
## Dados de campo, analíticos pela função prepara_bases
data <- dados_transf
# Informações de cada elemento analisado e UCC pela função prepara_bases
info_bol <- as.data.frame(info_bol)
info_bol[is.na(info_bol$UCC),"UCC"] <- 0
# Vetor das unidades
litho <- litho_max
Geo_cod <- unique(litho$Geo_cod)

data <- dplyr::left_join(data, litho, by = "VALUE")

t <- data.frame(table(litho$Geo_cod))
un_val <- as.numeric(as.character(t[t$Freq > 10,"Var1"]))

# Ordena as Unidades pelo código das unidades (Geo_cod)
cod_unidades <- leg
abrev <- cod_unidades$SIGLA
nome_unidade <- cod_unidades$SIGLA

# Criar vetor das unidades tectônicas
Geo_cod <- unique(data$Geo_cod)

# Criar vetor com sigla dos elementos
elementos <- elem_val

# Prepara lista com dados classificados
res <- list()

# Calcula as classes boxplot por elemento e por unidade
un_val <- as.character(un_val)
for (ul in un_val) {
  select <- data[data$Geo_cod == ul, ]
  res[[ul]] <- lapply(select[,elementos], function(x)
    cbind(select[,"Geo_cod"], select[,"VALUE"],  log_class_bxp(x),  x))
}

# Paleta de cores dos simbolos e mapa
pal_cod <- pals::jet(9)
cl_list <- list()
for (e in seq_along(elem_val))   {

  #############################################################################
  # Mapa no ggplot2
  #############################################################################

  # Atribui elemento a variável eq
  eq <-elem_val[e]
  unidade <- info_bol[info_bol$nome_analito == elem_val[e], "unidades"]
  dig <- info_bol[info_bol$nome_analito == elem_val[e], "DIG"]

  # Criar uma lista de dataset  iterando as unidades para o
  #elemento (eq) escolhido

  ls <- list()
  for(ul in un_val){
    VALUE <- res[[ul]][[eq]][,2]
    class <- res[[ul]][[eq]][,3]
    valor <- res[[ul]][[eq]][,4]
    lito <- rep(ul,length(VALUE))
    ls[[ul]] <- data.frame(VALUE,lito,class,valor)
  }

  # Criar o dataframe com a classificação
  base_dados <- do.call(rbind, ls)

  # Configura gráficos
  map_Theme <- ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(size = 4),
                     axis.text.y = ggplot2::element_text(size = 4),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(color="black",
                                               size=10,  hjust = 0.5))

  # Plota o mapa geoquímico do elemento escolhido

  x1 <- round(st_bbox(bacias)[[3]],1)+0.1
  x2 <- round(st_bbox(bacias)[[1]],1)-0.1
  y2 <- round(st_bbox(bacias)[[2]],1)-0.1
  y1 <- round(st_bbox(bacias)[[4]],1)+0.1
  base_dados <- base_dados[!is.na(base_dados$valor),]
  titulo_mapa <- paste0("Geochemical Map -  Stream sediment - ", "\n",
                        info_bol[info_bol$nome_analito == eq, "analito"], " - TIF")

  bacias_rec <- dplyr::inner_join(bacias, base_dados, by='VALUE')

  p1 <- ggplot2::ggplot(data = bacias_rec) +
    ggspatial::geom_sf(ggplot2::aes(fill=factor(class)), colour="grey", lwd = 0) +
    ggspatial::geom_sf(data = geologia, alpha = 0, lwd = 0.01) +
    ggplot2::geom_sf_text(data = geologia, ggplot2::aes(label = SIGLA), size = 1,
                 alpha = 0.6) +
    map_Theme +
    ggplot2::scale_fill_manual(name = paste0("Concentração \n",eq," (",unidade,")"),
                      values=setNames(pal_cod, 1:9),
                      guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggspatial::coord_sf(expand = F, xlim = c(x2-.04,x1+.04), ylim = c(y2-.04,y1+.04)) +
    ggplot2::scale_x_discrete(breaks = seq(x1, x2, by = -0.5)) +
    ggplot2::scale_y_discrete(breaks = seq(y1, y2, by= -0.5))   +
    ggspatial::annotation_scale(location="br",  pad_x = ggplot2::unit(2, "mm"),
                     pad_y = ggplot2::unit(2, "mm") )  +
    ggplot2::ggtitle(titulo_mapa) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 9, face = "bold"))

  bx <- list()

  for (ul in un_val) {
    # Boxplot
    el_org <- base_dados[base_dados$lito == ul,"valor"]
    el <- log10(el_org)
    nome_un <- cod_unidades[cod_unidades$Geo_cod == ul, "SIGLA"]

    # statisticas do boxplot dados log coefs 1.5, 3 e 4.5
    p <- boxplot.stats(el, coef = 4.5)
    p_id <- c(2, 5, 0, 6, 9)
    p2 <- boxplot.stats(el, coef = 3)
    p2_id <- c(3, 5, 0, 6, 8)
    p3 <- boxplot.stats(el, coef = 1.5)
    p3_id <- c(4, 5, 0, 6, 7)

    # max e min
    # linhas para adicionar na matriz das estatísticas do boxplot
    n <- c(min(el), 1)
    l <- c(max(el), 10)

    # valor maximo e mínimo dos dados logtransformados
    lmin <- min(el)
    lmax <- max(el)

    # data frame das estatisticas de cada coef.
    st <- data.frame(p$stats, p_id)
    st2 <- data.frame(p2$stats, p2_id)
    st3 <- data.frame(p3$stats, p3_id)
    colnames(st) <- c("log_teor", "id")
    colnames(st2) <- c("log_teor", "id")
    colnames(st3) <- c("log_teor", "id")

    # Cria a matriz das estatisticas boxplot
    df_lbx <- rbind(st, st2, st3, n, l)

    # Elimina a mediana
    df_lbx <- df_lbx %>%
      dplyr::filter(id != 0)

    # Ordena pelo id
    df_lbx <- dplyr::arrange( df_lbx, id)

    # ## Remove duplicatas, mantendo o valor máximo
    df <- df_lbx %>%
      dplyr::group_by(log_teor) %>%
      dplyr::summarise_at(dplyr::vars(matches("id")), list(~max(., na.rm = TRUE)))

    df_lbx <- data.frame(df)

    # Define os rótulos do boxplot
    #nome dos limites
    label <- c("Min", "Q1-4,5*AIQ","Q1-3*AIQ","Q1-1,5*AIQ", "Q1(25%)",
               "Q3(75%)", "Q3+1,5*AIQ", "Q3+3*AIQ", "Q3+4,5*AIQ","Max")
    rotulo_bp <- label[df_lbx$id]

    #teor do limite
    teor <- 10^df_lbx$log_teor

    #Cria tabela com as definiçõe do rótulo do boxplot
    df_lbx <- data.frame(teor,df_lbx, rotulo_bp)
    sby <- 0

    #Calcula os indices de posição dos limites do boxplot
    sby_id <- df_lbx$id[1:(length(df_lbx$id) - 1)]
    sby_id[1] <- sby_id[2] -1

    # Calcula as posições y das caixas entre os limites
    for (i in 1:(nrow(df_lbx) - 1)) {
      j <- i + 1
      sby[i] <- (df_lbx$log_teor[i] + df_lbx$log_teor[j])/2
    }

    #Define as posições x das caixas e símbolos (1)
    sbx <- c(rep(1, nrow(df_lbx) - 1))
    lbx <- c(rep(1, nrow(df_lbx)))

    # Criar Matriz das definições dos simbolos da legenda
    ind <- seq(1:9) # indice
    cex1_cod <- c(10, 8, 6, 4, 1, 4, 6, 8, 10)/5 # tamanho do simbolo
    cex2_cod <- c(10, 8, 6, 4, 1, 4, 6, 8, 10)/5 # tamanho do halo
    pch_cod <- c(1, 1, 1, 1, 3, 0, 0, 0, 0) # forma do simbolo
    # espessura do símbolo
    lwd1_cod <- c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)/3
    # espessura do halo
    lwd2_cod <- c(1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6)/3
    # cor do halo
    halo_cod <- rep("#000000", 9)

    # Cria a matriz de definicoes dos simbolos da legenda
    df_sb <- data.frame(ind, cex1_cod, cex2_cod, pch_cod, lwd1_cod,
                        lwd2_cod,halo_cod )

    # Define matrizes de dados para a figura do boxplot
    ##Posição dos símbolos do boxplot
    df_sb <- df_sb[sby_id,]
    teor <- 10^(sby)
    pos <- rep(0.05, length(sby))
    dados1 <- data.frame(pos,teor)

    ##Boxplot do elemento
    teor <- el_org
    pos <- rep(0.3, length(teor))
    dados2 <- data.frame(pos,teor)

    ##Posição dos rótulos do boxplot
    # Rótulo
    rotulo_lim <- label[df_lbx$id]
    teor <- df_lbx$teor
    rot_teor <- round(teor, dig)

    # Testa o mínimo e coloca o operador < antes do LD nos rótulos do boxplot
    if(round(10^lmin,dig)<= round(info_bol[info_bol$nome_analito == eq, "MDL"]/2, dig)){
      rot_teor[1] <- paste0("<",info_bol[info_bol$nome_analito == eq, "MDL"])
    }

    # Assegura o rótulo do mínimo
    rotulo_lim[1] <- "Min"

    # Cria tabela dos rótulos dos limiares bp
    pos <- rep(0.5, length(teor))
    dados3 <- data.frame(pos,teor,rot_teor,rotulo_lim)

    #### Calculo do número de amostras outlier para cada elemento
    #Contar se valor for maior que Q3+1.5*AIQ
    LS <- dados3[dados3$rotulo_lim == "Q3+1,5*AIQ", "rot_teor"]
    nls <- sum(dados2$teor > LS)
    LI <- dados3[dados3$rotulo_lim == "Q1-1,5*AIQ", "rot_teor"]
    nli <- sum(dados2$teor < LI)

    #Controle UCC
    if(info_bol[info_bol$nome_analito == eq, "UCC"] > 0.001 & info_bol[info_bol$nome_analito == eq, "UCC"] <= 10^lmax ){
      lb_ucc <- ggplot2::geom_text(ggplot2::aes(label = paste0( info_bol[info_bol$nome_analito == eq, "analito"], ": UCC*"), x = 0.7,
                              y = info_bol[info_bol$nome_analito == eq, "analito"]),
                          size = 3, colour="red")
      ln_ucc <- ggplot2::geom_segment(ggplot2::aes(x=0,xend=0.5, y=info_bol[info_bol$nome_analito == eq, "MDL"],
                                 yend = info_bol[info_bol$nome_analito == eq, "MDL"]),
                             colour = "red", linetype = "dotdash")
      pos <- c(pos,0.5)
      teor <- c(teor, info_bol[eq, "UCC"])
      rotulo_lim <- c(rotulo_lim,"UCC*")
      rot_teor <- c(rot_teor, info_bol[eq, "UCC"])
    }else{
      ln_ucc <- ggplot2::geom_segment(ggplot2::aes(x = 0,xend = 0.5,y = 10^lmax, yend = 10^lmax),
                             colour = "transparent", linetype = "dotdash")
    }


    dados4 <- data.frame(pos, teor, rot_teor, rotulo_lim)
    dados4 <- dplyr::arrange(dados4, teor)

    #Tema ggplot do boxplot
    bp_Theme <- ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                      axis.title.y = ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_blank(),
                      legend.position = "none",
                      axis.ticks = ggplot2::element_blank(),
                      plot.title = ggplot2::element_text(color="black",
                                                size=5, hjust = 0.5))



    bx[[ul]]  <- ggplot2::ggplot(NULL, ggplot2::aes(x= pos, y = teor)) +
      ggplot2::geom_text(data=dados4, size= 3/2.5, ggplot2::aes(label = paste0(rot_teor, ": ",
                                                             rotulo_lim)),
                hjust = 0, check_overlap = TRUE) +
      ggplot2::stat_boxplot(data = dados2, geom ='errorbar', width = 0.1, lwd = 0.2) +
      ggplot2::geom_boxplot(data = dados2, width = 0.2, position = "dodge",
                   outlier.size=0.5, lwd = 0.2) +
      ggplot2::theme_void() +
      ln_ucc +
      bp_Theme +
      ggplot2::geom_point(data = dados1, cex = 10/3, pch = 22, bg = pal_cod[sby_id]) +
      # geom_point(data = dados1, cex = df_sb$cex1_cod, pch = df_sb$pch_cod,
      # col = "black", stroke = df_sb$lwd1_cod ) +
      ggplot2::xlim(-0.02, 1) +
      ggplot2::scale_y_log10(limit = c(min(base_dados$valor), max(base_dados$valor))) +
      # scale_y_continuous(limit = c(min(data[,eq]), max(data[,eq]))) +
      ggplot2::ggtitle(paste0(nome_un, "\noutliers: inf./sup.: ", nli,"/", nls))  +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.4))
  }

  # Arranjar o layout da pagina

  lay <- rbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
               c(2,2,2,2,2,2,2,2,2,2,2,2,2,2),
               c(2,2,2,2,2,2,2,2,2,2,2,2,2,2),
               c(2,2,2,2,2,2,2,2,2,2,2,2,2,2))

  png(filename = paste0("outputs/mapa_geoquimico_tif_geral_unidades_", eq,  ".png"),
      units = "cm", width=15, height=14, bg = "white", res = 300)
  bp_leg <- do.call(gridExtra::"grid.arrange", c(bx, ncol=length(un_val), nrow=1))
   gridExtra::grid.arrange(p1, bp_leg, layout_matrix = lay)
  dev.off()
  cl_list[[e]] <- data.frame(base_dados, EL = rep(eq, nrow(base_dados)))
}
bd_long <- do.call(rbind, cl_list)
bd_long <- bd_long[, -4]
bd_long <- bd_long[, c("VALUE", "EL", "class")]
pv_bd <- tidyr::pivot_wider(bd_long, names_from = "EL", values_from = "class")
write.csv2(pv_bd, "outputs/classes_bxp_log.csv", row.names = FALSE)
}
