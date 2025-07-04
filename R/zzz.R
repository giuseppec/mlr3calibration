.onLoad <- function(libname, pkgname) {
  register_calibrator("platt", CalibratorPlatt)
}
