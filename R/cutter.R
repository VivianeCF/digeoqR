#' Corta pelas quebras calculadas do boxplot
#'
#' @param x vetor
#' @param cuts quebra
#'
#' @return
#' @export
#'
#' @examples
cutter <- function (x, cuts) {
  ncuts <- length(cuts)
  xi <- cut(x, breaks = cuts, labels = FALSE)
  xi[x <= cuts[1]] <- 0
  xi[x >= cuts[ncuts]] <- ncuts
  xi <- xi + 1
  invisible(xi)
}
