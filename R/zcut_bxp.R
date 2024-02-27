#' Calcula as quebras do booxplot
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
zcut_bxp <- function (x) {
  z <- log10(x)
  q <- quantile(z, probs = c(0.25, 0.75), na.rm = TRUE)
  zcut <- numeric(8)
  hw <- q[2] - q[1]
  zcut[1] <- q[1] - 4.5 * hw
  zcut[2] <- q[1] - 3 * hw
  zcut[3] <- q[1] - 1.5 * hw
  zcut[4] <- q[1]
  zcut[5] <- q[2]
  zcut[6] <- q[2] + 1.5 * hw
  zcut[7] <- q[2] + 3 * hw
  zcut[8] <- q[2] + 4.5 * hw
  zcut <- unique(zcut)
  10^zcut
}
