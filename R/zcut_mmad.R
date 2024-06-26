#' Gera os limiares MAD
#'
#' @param x vetor numérico > 0
#'
#' @return
#' @export
#'
#' @examples
zcut_mmad <- function (x) {
  z <- log10(x)
  mad <- mad(z, na.rm = TRUE)
  m <- median(z, na.rm = TRUE)
  q <- quantile(z, probs = c(0.25, 0.75), na.rm = TRUE)
  zcut <- numeric(8)
  zcut[6] <- m + 2*mad
  zcut[5] <- m + mad
  zcut[4] <- q[2]
  zcut[3] <- q[1]
  zcut[2] <- m - mad
  zcut[1] <- m - 2*mad
  zcut <- unique(zcut)
  10^zcut
}
