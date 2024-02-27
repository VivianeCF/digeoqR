#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
log_class_mad <- function (x) {
  cutter(x, zcut_mmad(x))

}
