#' Classifica pelo boxplot
#'
#' @param x  vetor
#'
#' @return
#' @export
#'
#' @examples
log_class_bxp <- function (x) {
  rgr::cutter(x, zcut_bxp(x))

}
