#' Controle de qualidade analítico e amostral
#'
#' @param dir_out Diretório de saída
#' @param base Base de dados analíticos
#' @param tipo_am Tipo de amostragem  1 = duplicata de campo
#' 2 = replicata de laboratório
#'
#' @return
#' @export
#'
#' @examples
qaqc_gq <- function(dir_out, base, tipo_am){
  # Define variáveis
  options(scipen = 999, OutDec = ",")
  out <- list()
  data_bol <- base[["dados transformados"]]
  replicatas <- base[["dados qaqc transformados"]]
  ref <- base[["condições analíticas"]]

  elementos <- paste0(ref$analito, "_", ref$unidades)

  data_bol[duplicated(paste0(data_bol$UTM_LESTE, data_bol$UTM_NORTE)), "cod_am"] <-
    "DUP"

  dup1 <- data_bol[data_bol$cod_am == "DUP", "NUM_CAMPO"]
  dup2 <- stringi::stri_sub(dup1, 1, 6)
  elementos[elementos %in% colnames(replicatas)]

  rep_lab <- replicatas[replicatas$cod_am == "REP" &
                          replicatas$N_LAB != "BRANCO_PREP",
                        c("N_LAB", elementos)]
  smp_lab <- data_bol[data_bol$N_LAB %in% rep_lab$N_LAB,
                      c("N_LAB", elementos)]


  dup_campo <- data_bol[data_bol$NUM_CAMPO %in% dup1,
                        c("NUM_CAMPO", elementos)]

  smp_campo <-  data_bol[data_bol$NUM_CAMPO %in% dup2,
                         c("NUM_CAMPO", elementos)]

  dup_campo$NUM_CAMPO <- dup2

  dup <- dplyr::left_join(smp_campo, dup_campo, by = "NUM_CAMPO")
  rep <- dplyr::left_join(smp_lab, rep_lab, by = "N_LAB")

  precisao <- 0
  ptile = 95
  rsd = 5

  if (tipo_am == 2) {
    dup <- rep
  }

  res2 = rep(NA, length(elementos))
  p <- list()
  g <- list()
  for (i in 1:length(elementos)) {
    el <- elementos[i]
    el1 <- paste0(el, ".x")
    el2 <- paste0(el, ".y")
    x1 <- dup[, el1]
    x2 <- dup[, el2]
    # 1. diferença absoluta
    temp <- na.omit(cbind(x1, x2))
    a1 <- temp[, 1]
    a2 <- temp[, 2]
    ndup <- length(a1)
    xdif <- abs(a1 - a2) + 0.00001
    xbar <- (a1 + a2) / 2

    sra <- (a1 - a2) / sqrt(2)
    difas <- (a1 - a2) / sqrt(2) / mean(a1)


    # calculo da precisão do elemento
    soma_org <- sum(dup[, el1])
    soma_dup <- sum(dup[, el2])
    media <- (soma_org + soma_dup) / nrow(dup)
    soma_qua <- sum(xdif ^ 2)
    sd_dup <- sqrt(soma_qua / (2 * nrow(dup)))
    rsd_pct <- round((sd_dup / media) * 100, 0)
    calc2 <- qnorm(1 - (1 - ptile / 100) / 2) * rsd * 0.014142
    ylcalc <- calc2 * min(xbar)
    yhcalc <- calc2 * max(xbar)

    dat <- cbind(dup[, el1], dup[, el2])
    dat.m <- mean(dat)
    dat.s <- 1 / (2 * nrow(dat)) * sum((dup[, el1] - dup[, el2]) ^ 2)
    res2[i] = 100 * sqrt(dat.s) / dat.m


    ratio <- xbar / xdif
    for (j in 1:ndup) {
      if (ratio[j] <= min(xbar) / ylcalc)
        ratio[j] <- 1
      else
        ratio[j] <- 0
    }

    nout <- sum(ratio)
    test <- binom.test(nout, ndup, 1 - (ptile / 100), "greater")
    test.prob <- test$p.value
    if (test.prob >= 0.9999)
      test.prob <- 0.9999
    label_res <- paste(
      "RSD = ",
      rsd,
      " % (Precisão 2SD = ",
      2 * rsd,
      " %)\nPercentil =",
      ptile,
      "%\nNúmero de Duplicatas  =",
      ndup,
      "\nNúmero de Duplicatas 'Fora' =",
      nout,
      "\nProbabilidade =",
      signif(test.prob, 4)
    )


    calc <- data.frame(xbar, xdif, x1 = temp[, 1], x2 = temp[, 2], sra)
    calc <- calc[order(calc[, "xbar"]), ]


    line <-
      data.frame(x = c(min(xbar), max(xbar)), y = c(ylcalc, yhcalc))

    png(
      filename = paste0(dir_out, "TH2_", elementos[i], ".png"),
      units = "in",
      width = 5,
      height = 4,
      res = 300
    )

    p1 <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = calc,
        ggplot2::aes(xbar, xdif),
        pch = 20,
        size = 3
      ) +
      ggplot2::scale_x_continuous(name = paste0("Média das Duplicatas"),
                                  trans = 'log10') +
      ggplot2::scale_y_continuous(name = paste0("Diferença Absoluta entre Duplicatas"),
                                  trans = 'log10') +
      ggplot2::geom_line(data = line, ggplot2::aes(x, y)) +
      ggplot2::ggtitle(paste0(ref$analito[i], " (",  ref$unidades[i], ")"))

    g1 <-   gridExtra::grid.arrange(
      p1,
      nrow = 1,
      bottom = grid::textGrob(
        label_res,
        gp = grid::gpar(font = 3, fontsize = 9),
        hjust = 1,
        x = 1
      )
    )

    dev.off ()

    precisao <- rbind(precisao, c(elementos[i], rsd_pct))
    p[[i]] <- p1
    g[[i]] <- g1
  }
  names(p) <- ref$analito

  names(g) <- ref$analito
  # gridExtra::grid.arrange(grobs=p[3], ncol = 1)
  out[[1]] <- p
  out[[2]] <- g
  precisao <- as.data.frame(precisao[-1, ])
  colnames(precisao) <- c("Elemento", "RSD %")
  rownames(precisao) <- NULL
  names(res2) <- elementos

  res2r <- round(res2, 1)
  write.csv2(precisao, paste0(dir_out, "dados_precisao2.csv"), row.names = FALSE)
  out[[3]] <- precisao
  write.csv2(res2r, file = paste0(dir_out, "dados_precisao_stada2.csv"))
  out[[4]] <- res2r
  colnames(ref)[1:3] <-
    c("Elemento", "Unidade", "Limite de detecção")
  ref$Analito <- paste0(ref$Elemento, "_", ref$Unidade)
  precisao <-
    precisao[!is.na(precisao$`RSD %`) & precisao$`RSD %` != "0", ]
  colnames(precisao)[1] <- "Analito"
  tb <-
    dplyr::inner_join(ref[, c("Elemento", "Unidade", "Limite de detecção", "Analito")], precisao, by = "Analito")
  tb$`RSD %` <- as.numeric(tb$`RSD %`)
  tb <- tb[order(tb$`RSD %`), ]

  ## Tabela por faixa de precisão
  # limiares: 10, 30, 50, 100

  tb[tb$`RSD %` <= 10, "Classe"] <- "< 10 %"
  tb[tb$`RSD %` > 10 & tb$`RSD %` <= 30, "Classe"] <- "10 - 30 %"
  tb[tb$`RSD %` > 30 & tb$`RSD %` <= 70, "Classe"] <- "30-70 %"
  tb[tb$`RSD %` > 70, "Classe"] <- "> 70 %"


  tb2 <- tb %>%
    dplyr::group_by(Classe) %>%
    dplyr::mutate(Elementos = paste0(Elemento, collapse = ", "))

  tabela_precisao <- unique(tb2[, c("Classe", "Elementos")])
  colnames(tabela_precisao)[1] <- "Precisão (RSD %)"
  tabela_precisao$Elementos = stringr::str_wrap(tabela_precisao$Elementos, 40)
  out[[5]] <- tabela_precisao

  # Salva figura
  png(
    paste0(dir_out, "tabela_precisao.png"),
    units = "cm",
    width = 17,
    height = 6,
    res = 300
  )
  gridExtra::grid.table(tabela_precisao, rows = NULL)
  dev.off()

  names(out) <- c("grafico sem texto",
                  "gráfico com texto",
                  "dados de precisão calc",
                  "dados de precisao statDA",
                  "df tabela de precisão")
  return(out)
}
