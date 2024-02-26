zcut_ca <- function (x) {
  # area
  df <- order(x, decreasing = TRUE) %>%
    mutate(cum_area = cumsum(a))

  # condição de redução
  f = nrow(df)
  if (nrow(df) > 200) {
    f = 55
  }
  fator <- round(nrow(df) / f)
  df$grupo_index <- c(0, rep(1:(nrow(df) - 1) %/% fator))
  df_sum <- group_by(df, grupo_index) %>%
    summarise(mean_value = mean(z), mean_area = mean(cum_area))
  df_sum <- df_sum[!duplicated(df_sum$mean_value), ]

  # transformação logarítmica
  y <- round(log10(df_sum$cum_area), 3)
  z <- round(log10(df_sum$value), 3)
  model <- data.frame(z, y)
  model <- model[!duplicated(model$z), ]
  z <- model$z
  y <- model$y
  o <- lm(y ~ z)
  # segmentação
  o.seg <- segmented::segmented(o, seg.Z =  ~ z, npsi = 4)
  if (nrow(df_sum) < 50 & nrow(df_sum) > 30) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ z, npsi = 3)
  }
  if (nrow(df_sum) <= 30 & nrow(df_sum) >= 20) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ z, npsi = 2)
  }
  if (nrow(df_sum) < 20) {
    o.seg <- segmented::segmented(o, seg.Z =  ~ z, npsi = 1)
  }
  os <-
    segmented::update(o.seg, control = seg.control(it.max = 100, display =
                                                     TRUE))
  zcut <- round(10 ^ (os[["indexU"]][["z"]]), 2)
  10 ^ zcut
}
