#' Controle de qualidade analitico e amostral
#'
#' @param dir_out Diretório de saída
#' @param base base de dados analíticos
#'
#' @return
#' @export
#'
#' @examples
qaqc_gq <- function(dir_out, base){

# Define variáveis
data_bol <- base[[ "dados transformados"]]
duplicatas <- base[[ "duplicatas"]]
replicatas <- base[[ "replicatas"]]
ref <- base[[ "condições analíticas"]]

duplicatas <- data_bol[data_bol$cod_am == "DUP",]
data_qaqc <- data_bol[data_bol$N_campo %in% duplicatas$N_campo,]
amostras <- data_qaqc[data_qaqc$cod_am == "SMP",]
replicatas <- replicatas[replicatas$classe_am == "REP" &
                           replicatas$N_LAB != "BRANCO_PREP",]
replicatas$cod_am <- replicatas$classe_am
colnames(replicatas) <- gsub("_ICM40B", "", colnames(replicatas))
colnames(duplicatas)[15:64] <- paste0(colnames(duplicatas)[15:64],2)
colnames(amostras)[15:64] <- paste0(colnames(amostras)[15:64],1)
replicatas <- replicatas[, c(1,10:59)]

dup <- dplyr::left_join(amostras[,c(4,15:64)],
                        duplicatas[,c(4,15:64)], by = "N_campo")

precisao <- 0
ptile = 95
rsd = 5

elementos <- gsub(1, "",  colnames(dup)[2:51])

res2 = rep(NA, length(elementos))

for (i in 1:length(elementos)) {
  el <- elementos[i]
  el1 <- paste0(el,1)
  el2 <- paste0(el,2)
  x1 <- dup[,el1]
  x2 <- dup[,el2]
  # 1. diferença absoluta
  temp <- na.omit(cbind(x1, x2))
  a1 <- temp[, 1]
  a2 <- temp[, 2]
  ndup <- length(a1)
  xdif <- abs(a1 - a2)+0.00001
  xbar <- (a1 + a2)/2

  sra <- (a1 - a2)/sqrt(2)
  difas <- (a1 - a2)/sqrt(2)/mean(a1)


  # calculo da precisão do elemento
  soma_org <- sum(dup[,el1])
  soma_dup <- sum(dup[,el2])
  media <- (soma_org+soma_dup)/nrow(dup)
  soma_qua <- sum(xdif^2)
  sd_dup <- sqrt(soma_qua/(2*nrow(dup)))
  rsd_pct <- round((sd_dup/media)*100,0)
  calc2 <- qnorm(1 - (1 - ptile/100)/2) * rsd * 0.014142
  ylcalc <- calc2 * min(xbar)
  yhcalc <- calc2 * max(xbar)

  dat <- cbind(dup[,el1], dup[,el2])
  dat.m <- mean(dat)
  dat.s <- 1/(2*nrow(dat))*sum((dup[,el1]-dup[,el2])^2)
  res2[i]=100*sqrt(dat.s)/dat.m


  ratio <- xbar/xdif
  for (j in 1:ndup) {
    if (ratio[j] <= min(xbar)/ylcalc)
      ratio[j] <- 1
    else ratio[j] <- 0
  }

  nout <- sum(ratio)
  test <- binom.test(nout, ndup, 1 - (ptile/100), "greater")
  test.prob <- test$p.value
  if(test.prob >= 0.9999) test.prob <- 0.9999
  label_res <- paste("RSD = ", rsd, " % (Precisão 2SD = ", 2*rsd,
                     " %)\nPercentil =", ptile, "%\nNúmero de Duplicatas  =", ndup,
                     "\nNúmero de Duplicatas 'Fora' =",
                     nout, "\nProbabilidade =", signif(test.prob, 4))


  calc <- data.frame(xbar, xdif, x1=temp[,1], x2=temp[,2], sra)
  calc <- calc[order(calc[,"xbar"]),]


  line <- data.frame(x = c(min(xbar), max(xbar)), y= c(ylcalc, yhcalc))

  png(filename = paste0(dir_out, "TH2_", elementos[i], ".png"),
      units = "in", width=5, height=4, res = 300)

  p1 <- ggplot() +
    geom_point(data = calc, aes(xbar, xdif), pch =20, size = 3) +
    scale_x_continuous(name =paste0("Média das Duplicatas" ),
                       trans='log10') +
    scale_y_continuous(name =paste0("Diferença Absoluta entre Duplicatas"), trans='log10') +
    geom_line(data = line, aes(x,y))

  grid.arrange(p1, nrow = 1,
               top = textGrob(paste0("Duplicatas de Campo - ", el ),
                              gp=gpar(fontsize=12,font=3)),
               bottom = textGrob(label_res,
                                 gp = gpar(font = 3, fontsize = 9),
                                 hjust = 1,x = 1))

  dev.off ()

  precisao <- rbind(precisao, c(elementos[i], rsd_pct))

}

precisao <- as.data.frame(precisao[-1,])
colnames(precisao) <- c("Elemento","RSD %")
rownames(precisao) <- NULL
names(res2) <- elementos

res2r <- round(res2,1)
write.csv2(precisao, paste0(dir_out, "dados_precisao2.csv"), row.names = FALSE)
write.csv2(res2r,file= paste0(dir_out, "dados_precisao_stada2.csv"))
ref <- ref[,c(-1,-10:-15)]
colnames(ref)[1:3] <- c("Elemento", "Unidade", "Limite de detecção")
precisao <- precisao[!is.na(precisao$`RSD %` ) & precisao$`RSD %` != "0",]
tb <- dplyr::inner_join(ref[,c("Elemento", "Unidade", "Limite de detecção")], precisao, by = "Elemento")
tb$`RSD %` <- as.numeric(tb$`RSD %`)
tb <- tb[order(tb$`RSD %`),]

tb1 <- tb[1:24,]
tb2 <- tb[25:48,]

tb_final <- cbind(tb1,tb2)

## Tabela por faixa de precisão
# limiares: 10, 30, 50, 100

tb[tb$`RSD %` <= 10, "Classe"] <- "< 10 %"
tb[tb$`RSD %` > 10 & tb$`RSD %` <= 30, "Classe"] <- "10 - 30 %"
tb[tb$`RSD %` > 30 & tb$`RSD %` <= 70, "Classe"] <- "30-70 %"
tb[tb$`RSD %` > 70, "Classe"] <- "> 70 %"


tb2 <- tb %>%
  group_by(Classe) %>%
  mutate(Elementos = paste0(Elemento, collapse = ", "))

tabela_precisao <- unique(tb2[, c("Classe", "Elementos")])
colnames(tabela_precisao)[1] <- "Precisão (RSD %)"
tabela_precisao$Elementos = str_wrap(tabela_precisao$Elementos, 40)
png(paste0(dir_out, "tabela_precisao.png"),
    units = "cm", width = 17, height = 6, res = 300)
grid.table(tabela_precisao, rows = NULL)
dev.off()

grid.table(tb2[[2]], rows = NULL)}
