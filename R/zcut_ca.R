#' Gera os limiares C-A
#'
#' @param x vetor numérico
#' @param a vetor com áreas das bacias
#'
#' @return
#' @export
#'
#' @examples
zcut_ca <- function (x, a) {
  df <- data.frame(x,a)
  df <- df[order(df$x, decreasing = TRUE),]
  df <- unique(df)

  df <- df %>%
    dplyr::mutate(cum_area = cumsum(a))

  # Condição de redução
  f = nrow(df)
  if (nrow(df) > 200) {
    f = 55
  }
  fator <- round(nrow(df) / f)
  df$grupo_index <- c(0, rep(1:(nrow(df) - 1) %/% fator))
  df_sum <- dplyr::group_by(df, grupo_index) %>%
      dplyr::summarise(mean_value = mean(x), mean_area = mean(cum_area))
  df_sum <- df_sum[!duplicated(df_sum$mean_value), ]

  # Transformação logarítmica
  y <- round(log10(df_sum$mean_area), 3)
  x <- round(log10(df_sum$mean_value), 3)
  model <- data.frame(x,y)
  model <- model[!duplicated(model$x),]
  x <- model$x
  y <- model$y
  o <- lm(y ~ x)
  plot(x,y)
  if(length(x) <= 20){ npsi = 3}else{ npsi =  4}

  # Segmentação
  os <- segmented::segmented(o, seg.Z= ~x, npsi = npsi,
                                control = segmented::seg.control(it.max = 100, h = 0.1,
                                                                 display = FALSE,
                                                                 quant=TRUE))


  zcut <- unique(10 ^ (os[["indexU"]][["x"]]))
  zcut
}
