.onLoad <- function(libname, pkgname) {
  register_calibrator("platt", CalibratorPlatt)
  register_calibrator("beta", CalibratorBeta)
  register_calibrator("isotonic", CalibratorIsotonic)
}
