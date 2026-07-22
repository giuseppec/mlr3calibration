# Static registries of everything this package adds to shared dictionaries.
# Following the mlr3learners / mlr3torch pattern, adds are unconditional so a
# reload replaces stale generators from a previous namespace, and .onUnload
# removes exactly these keys.
mlr3calibration_calibrators = list(
  platt = CalibratorPlatt,
  beta = CalibratorBeta,
  isotonic = CalibratorIsotonic,
  ovr = CalibratorOneVsRest,
  selector = CalibratorSelector
)

mlr3calibration_measures = list(
  "classif.ece" = MeasureClassifECE,
  "classif.conf_ece" = MeasureClassifConfidenceECE,
  "classif.sce" = MeasureClassifSCE,
  "classif.ace" = MeasureClassifACE,
  "classif.tace" = MeasureClassifTACE,
  "classif.tl_ece" = MeasureClassifTopLabelECE,
  "classif.ici" = MeasureClassifICI,
  "classif.hltest" = MeasureClassifHLTest,
  "classif.cox_slope" = MeasureClassifCoxSlope,
  "classif.cox_intercept" = MeasureClassifCoxIntercept,
  "classif.spiegelhaltersz" = MeasureClassifSpiegelhaltersZ
)

mlr3calibration_pipeops = list(
  calibrate = PipeOpCalibrate
)

register_calibrators = function() {
  iwalk(mlr3calibration_calibrators, function(calibrator, key) {
    validate_calibrator_generator(key, calibrator, require_default = TRUE)
    mlr3_calibrators$add(key, calibrator)
  })
}

register_mlr3 = function() {
  iwalk(mlr3calibration_measures, function(measure, key) mlr3::mlr_measures$add(key, measure))
}

register_mlr3pipelines = function() {
  iwalk(mlr3calibration_pipeops, function(pipeop, key) mlr3pipelines::mlr_pipeops$add(key, pipeop))
}

.onLoad = function(libname, pkgname) {
  register_calibrators()
  register_namespace_callback(pkgname, "mlr3", register_mlr3)
  register_namespace_callback(pkgname, "mlr3pipelines", register_mlr3pipelines)
}

.onUnload = function(libpath) {
  walk(names(mlr3calibration_calibrators), function(key) mlr3_calibrators$remove(key))
  walk(names(mlr3calibration_measures), function(key) mlr3::mlr_measures$remove(key))
  walk(names(mlr3calibration_pipeops), function(key) mlr3pipelines::mlr_pipeops$remove(key))
}
