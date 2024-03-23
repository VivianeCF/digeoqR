#' Visual do Teste Wilcoxon
#'
#' @param base bases de dados
#' @param lito_bacia bacias rotuladas
#' @param nbc número de bacias por unidade
#' @param dir_out diretório de saída
#' @param elem_val elementos válidados
#'
#' @return
#' @export
#'
#' @examples
vis_teste_wilcox <- function(dir_out, elem_val, base, lito_bacia, nbc){
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
    dplyr::mutate( Test = dplyr::case_when(value < 0.05 ~ 'p-value < 0,05', TRUE ~ '')) %>%
    ggplot2::ggplot() + ggplot2::geom_tile(ggplot2::aes(factor(rowname,levels = SIGLA ),
                             factor(name, levels= rev(SIGLA)), fill = Test)) +
    ggplot2::ggtitle(eq) +
    ggplot2::scale_fill_manual(name = "Teste Wilcoxon", values = c('white', 'red')) +
    ggplot2::coord_equal() + ggplot2::theme_bw() +
    # guides(fill="none") +
    ggplot2::xlab("") + ggplot2::ylab("")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  legenda <- ggpubr::get_legend(p)
  l1 <- ggpubr::as_ggplot(legenda)
  t[[i]] <- p + ggplot2::guides(fill="none")
}
t[[(length(elem_val)+1)]] <- l1
out <- t
return(out)
}
