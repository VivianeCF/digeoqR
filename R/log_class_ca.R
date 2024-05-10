#' Classifica pela C-A
#'
#' @param x vetor numérico
#' @param a vetor com áreas das bacias
#'
#' @return
#' @export
#'
#' @examples
log_class_ca <- function (x, a) {
  rgr::cutter(x, zcut_ca(x, a))

}
