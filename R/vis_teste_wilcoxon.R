#' Visual do Teste Wilcoxon
#'
#' @param base bases de dados
#' @param lito_bacia bacias enriquecidas
#' @param nbc número de bacias por unidade
#'
#' @return
#' @export
#'
#' @examples
vis_teste_wilcox <- function(base, lito_bacia, nbc){
lst_pr <- list()
lst_el <- list()
out <- list()
# Lê arquivos
## Bacias modeladas pelo SRTM usando o projeto R MODEL_BACIAS
mydata_p <-  base[[4]]

# Criar vetor das unidades
mylitho_max <- lito_bacia[[2]]

Geo_cod <- unique(mylitho_max$Geo_cod)

mydata_p <- dplyr::left_join(mydata_p, mylitho_max, by = "VALUE")

t <- data.frame(table(mylitho_max$Geo_cod))
un_val <- as.numeric(as.character(t[t$Freq > nbc,"Var1"]))

# Ordena as Unidades pelo código das unidades (Geo_cod)
cod_unidades <- leg
SIGLA <- cod_unidades$SIGLA
nome_unidade <- cod_unidades$SIGLA
mydata_p <- dplyr::left_join(mydata_p, cod_unidades, by = "Geo_cod")
mydata_p$SIGLA <- factor(mydata_p$SIGLA, levels = SIGLA)
mydata_p$elemento <- mydata_p$analito
mydata_p$analito <- paste0(mydata_p$elemento, "_", mydata_p$unidade)
t <- list()

for(i in seq(elem_val)) {
  eq <- elem_val[i]
  filtro <- mydata_p[mydata_p$analito==eq,]
  test = pairwise.wilcox.test(filtro$valor, filtro$SIGLA,
                              p.adj = "none",paired = FALSE)

  p <- data.frame(test$p.value) %>% tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-rowname) %>%
    dplyr::mutate( Test = dplyr::case_when(value < 0.05 ~ 'Significant', TRUE ~ '')) %>%
    ggplot() + geom_tile(aes(factor(rowname,levels = SIGLA ),
                             factor(name, levels= rev(SIGLA)), fill = Test)) +
    ggtitle(eq) +
    scale_fill_manual(name = "Wilcoxon Test", values = c('white', 'red')) +
    coord_equal() + theme_bw() +
    # guides(fill="none") +
    xlab("") + ylab("")+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  legenda <- ggpubr::get_legend(p)
  l1 <- ggpubr::as_ggplot(legenda)
  t[[i]] <- p + guides(fill="none")
}
t[[(length(elem_val)+1)]] <- l1
png("Outputs/Testes/test_wilcoxon.png",
    units = "cm", width = 25,
    height = 25, res = 300)

gridExtra::grid.arrange(grobs = t[1:16],  ncol = 4,
                        top = grid::textGrob(""),
                        gp=gpar(fontsize=12,font=3))
dev.off()
}
