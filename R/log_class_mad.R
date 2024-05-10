#' Classifica pelo MAD
#'
#' @param x vetor numérico > 0
#'
#' @return
#' @export
#'
#' @examples
log_class_mad <- function (x) {
  rgr::cutter(x, zcut_mmad(x))

}
