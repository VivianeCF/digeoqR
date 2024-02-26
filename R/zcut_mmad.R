zcut_mmad <- function (x) {
  z <- log10(x)
  mad <-mad(z, na.rm = TRUE)
  m <- median(z, na.rm = TRUE)
  zcut <- numeric(8)
  zcut[1] <- m + 2*mad
  10^zcut
}
