#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
log_class_mad <- function (x) {
  rgr::cutter(x, zcut_mmad(x))

}
