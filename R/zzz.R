.onLoad <- function(libname, pkgname) {
  register_calibrator("platt", CalibratorPlatt)
  register_calibrator("beta", CalibratorBeta)
  register_calibrator("isotonic", CalibratorIsotonic)
  register_calibration_pipeop("calib.oof", PipeOpCalibrationOOF)
  register_calibration_pipeop("calib.perfold", PipeOpCalibrationPerFold)
  register_calibration_pipeop("calib.tunefirst.oof", PipeOpCalibrationTuneFirstOOF)
  register_calibration_pipeop("calib.tunefirst.perfold", PipeOpCalibrationTuneFirst)
}
