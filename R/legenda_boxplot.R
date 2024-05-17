#' Legenda boxplot para o mapa TIF
#'
#' @param data Dados analíticos em csv
#' @param job_info Informações das condições analíticas
#' @param eq Elemento químico
#'
#' @return
#' @export
#'
#' @examples
legenda_boxplot <- function(data = "mydata.csv", job_info = "myjob.csv", eq = "Ni"){
  ## Prancha das legendas - 7 classes-------------------------------------------
  # Carregar dados
  ## Dados analíticos
  df <- read.table(data, head=T,  sep=";", dec=",")

  # Dados das informações de cada elemento analisado e UCC
  myjob <- read.csv2(job_info, head=T, sep=";", dec=",",
                     encoding = "latin1", row.names = 1)
  el <- df[, eq]

  mdl <- myjob[eq,"LDI"]
  digits1 <- function(x, n=NULL, simplify = FALSE) {
    if(length(x) > 1) {
      if(is.null(n) & simplify) {
        n <- floor(max(log10(x))) + 1
      }
      sapply(x,digits, simplify=simplify, n=n)
    } else {
      if(is.null(n)) {
        n <- floor(log10(x))
      } else {
        n <- n - 1
      }
      x %/% 10^seq(n,0) %% 10
    }
    d <- stringr::str_width(x)-2
    return(d)
  }

  dig <- digits1(mdl)

  # Paleta de cores dos simbolos e mapa
  pal_cod <-pals::jet(7)
  # Boxplot
  el_org <- el #res[[eq]][,4]
  el <- log10(el_org)
  # nome_un <- cod_unidades[cod_unidades$MAX_LITO == ul, "abrev"]

  # statisticas do boxplot dados log coefs 1.5, 3 e 4.5
  # p <- boxplot.stats(el, coef = 4.5)
  # p_id <- c(2, 5, 0, 6, 9)
  bs2 <- boxplot.stats(el, coef = 3)
  bs2_id <- c(3, 5, 0, 6, 8)
  bs3 <- boxplot.stats(el, coef = 1.5)
  bs3_id <- c(4, 5, 0, 6, 7)

  # max e min
  # linhas para adicionar na matriz das estatísticas do boxplot
  n <- c(min(el), 1)
  l <- c(max(el), 10)

  # valor maximo e mínimo dos dados logtransformados
  lmin <- min(el)
  lmax <- max(el)

  # data frame das estatisticas de cada coef.
  # st <- data.frame(p$stats, p_id)
  st2 <- data.frame(bs2$stats, bs2_id)
  st3 <- data.frame(bs3$stats, bs3_id)
  # colnames(st) <- c("log_teor", "id")
  colnames(st2) <- c("log_teor", "id")
  colnames(st3) <- c("log_teor", "id")

  # Cria a matriz das estatisticas boxplot
  df_lbx <- rbind(st2, st3, n, l)

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
  label <- c("", "","", "",
             "", "", "","")
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
  ind <- seq(1:7) # indice
  cex1_cod <- c( 8, 6, 4, 1, 4, 6, 8)/5 # tamanho do simbolo
  cex2_cod <- c( 8, 6, 4, 1, 4, 6, 8)/5 # tamanho do halo
  pch_cod <- c( 1, 1, 1, 3, 0, 0, 0) # forma do simbolo
  # espessura do símbolo
  lwd1_cod <- c( 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)/3
  # espessura do halo
  lwd2_cod <- c( 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6)/3
  # cor do halo
  halo_cod <- rep("#000000", 7)

  # Cria a matriz de definicoes dos simbolos da legenda
  df_sb <- data.frame(ind, cex1_cod, cex2_cod, pch_cod, lwd1_cod,
                      lwd2_cod,halo_cod, pal_cod )

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
  if(round(10^lmin,dig)<= round(myjob[eq, "LDI"]/2, dig)){
    rot_teor[1] <- paste0("<",myjob[eq,"LDI"])
  }

  # Assegura o rótulo do mínimo e máximo
  rotulo_lim[1] <- ""
  rotulo_lim[length(rotulo_lim)] <- ""
  # Cria tabela dos rótulos dos limiares bp
  pos <- rep(0.5, length(teor))
  dados3 <- data.frame(pos,teor,rot_teor,rotulo_lim)

  #### Calculo do número de amostras outlier para cada elemento
  #Contar se valor for maior que Q3+1.5*AIQ
  LS <- dados3[dados3$rotulo_lim == "", "rot_teor"]
  # nls <- sum(dados2$teor > LS)
  LI <- dados3[dados3$rotulo_lim == "", "rot_teor"]
  # nli <- sum(dados2$teor < LI)
  max(el_org)
  #Controle UCC
  eq <- eq
  if(myjob[eq, "UCC"] > 0.001 & myjob[eq, "UCC"] <= 10^lmax ){
    lb_ucc <- ggplot2::geom_text(ggplot2::aes(label = paste0(myjob[eq,1], ": UCC*"), x = 0.7,
                            y = myjob[eq,1]), size = 3, colour="red")
    ln_ucc <- ggplot2::geom_segment(ggplot2::aes(x=0,xend=0.5, y=myjob[eq, "UCC"],
                               yend = myjob[eq, "UCC"]),
                           colour = "red", linetype = "dotdash")
    pos <- c(pos,0.5)
    teor <- c(teor, myjob[eq, "UCC"])
    rotulo_lim <- c(rotulo_lim,"UCC*")
    rot_teor <- c(rot_teor, myjob[eq, "UCC"])
  }else
  {
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
                                              size=10, hjust = 0.5))




  bx  <- ggplot2::ggplot(NULL, ggplot2::aes(x= pos, y = teor)) +
    ggplot2::geom_text(data=dados4, size= 3, ggplot2::aes(label = paste0(rot_teor, " ", rotulo_lim)),
              hjust = 0, check_overlap = TRUE) +
    ggplot2::stat_boxplot(data = dados2, geom ='errorbar', width = 0.1, lwd = 0.2) +
    ggplot2::geom_boxplot(data = dados2, width = 0.2, position = "dodge",
                 outlier.size=0.5, lwd = 0.2) +
    theme_void() +
    ln_ucc +
    bp_Theme +
    ggplot2::geom_point(data = dados1, cex = 2.8, pch = 22, bg = pal_cod[sby_id-1]) +
    # ggplot2::geom_point(data = dados1, cex = df_sb$cex1_cod, pch = df_sb$pch_cod,
    # col = "black", stroke = df_sb$lwd1_cod ) +
    xlim(-0.02, 1) +
    ggplot2::scale_y_log10(limit = c(min(el_org), max(el_org))) +
    ggplot2::ggtitle(myjob[eq, "UN"])

  # bxg <- as.grob(bx)
  # pg <-as.grob(p2)
  #
  #
  #
  # mapa[[e]] <- qplot(pg
  #                    )
  #
  # bxg <-   cowplot::as_grob(bx)
  # png("leg_pb.png",
  #     units = "cm", width = 5,
  #     height = 15, res = 300)

  bx
  # dev.off()
}
