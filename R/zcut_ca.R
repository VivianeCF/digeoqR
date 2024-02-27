#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
zcut_ca <- function (x, a) {
  # area
 x <- mydata$Cu_ppm
  a <- mydata$Area_bacia
  df <- data.frame(x,a)
  df <- df[!is.na(df$a),]
  df <- df[order(df$x, decreasing = TRUE),]


  df <- df %>%
    dplyr::mutate(cum_area = cumsum(a))

  # # condição de redução
  f = nrow(df)
  if (nrow(df) > 200) {
    f = 55
  }
  fator <- round(nrow(df) / f)
  df$grupo_index <- c(0, rep(1:(nrow(df) - 1) %/% fator))
  df_sum <- dplyr::group_by(df, grupo_index) %>%
      dplyr::summarise(mean_value = mean(x), mean_area = mean(cum_area))
  df_sum <- df_sum[!duplicated(df_sum$mean_value), ]
  y <- round(log10(df_sum$mean_area), 3)
  x <- round(log10(df_sum$mean_value), 3)
  # transformação logarítmica


  # x <- round(log10(df$x), 3)
  # y <- round(log10(df$cum_area), 3)

model <- data.frame(x,y)
  plot(x,y)
  x <- model$x
  y <- model$y
  o <- lm(y ~ x)

  # segmentação
  o.seg <- segmented::segmented(o, seg.Z=~x, npsi = 4)

  if (nrow(df_sum) < 50 & nrow(df_sum) > 30) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ x, npsi = 3)
  }
  if (nrow(df_sum) <= 30 & nrow(df_sum) >= 20) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ x, npsi = 2)
  }
  if (nrow(df_sum) < 20) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ x, npsi = 1)
  }
  os <- update(o.seg, control = seg.control(it.max = 100, display = TRUE))

  zcut <- unique(round(10 ^ (os[["indexU"]][["x"]]), 2))
  10 ^ zcut
}
