log_class_mmad <- function (x) {
  cutter(x, c(-Inf, zcut_mmad(x), Inf), include.left = false)
  
}
