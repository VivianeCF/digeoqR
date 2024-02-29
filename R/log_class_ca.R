#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
log_class_ca <- function (x, a) {
  rgr::cutter(x, zcut_ca(x, a))

}
