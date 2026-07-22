test_that(".onLoad registers measures and PipeOps", {
  mlr3calibration:::.onLoad(NULL, "mlr3calibration")

  testthat::expect_true(all(c(
    "classif.ece",
    "classif.conf_ece",
    "classif.sce",
    "classif.ace",
    "classif.tace",
    "classif.tl_ece",
    "classif.ici",
    "classif.hltest",
    "classif.cox_slope",
    "classif.cox_intercept",
    "classif.spiegelhaltersz"
  ) %in% mlr3::mlr_measures$keys()))

  testthat::expect_true(all(c(
    "platt",
    "beta",
    "isotonic"
  ) %in% mlr3_calibrators$keys()))

  testthat::expect_true(all(c(
    "calibrate"
  ) %in% mlr3pipelines::mlr_pipeops$keys()))

  testthat::expect_false(any(c(
    "calibration_oof",
    "calibration_per_fold",
    "calibration_tune_first_oof",
    "calibration_tune_first_per_fold"
  ) %in% mlr3pipelines::mlr_pipeops$keys()))
})

test_that(".onUnload deregisters package entries before reload", {
  mlr3calibration:::.onUnload(NULL)
  on.exit(mlr3calibration:::.onLoad(NULL, "mlr3calibration"), add = TRUE)

  testthat::expect_false("platt" %in% mlr3_calibrators$keys())
  testthat::expect_false("classif.ece" %in% mlr3::mlr_measures$keys())
  testthat::expect_false("calibrate" %in% mlr3pipelines::mlr_pipeops$keys())

  mlr3calibration:::.onLoad(NULL, "mlr3calibration")
  testthat::expect_true("platt" %in% mlr3_calibrators$keys())
  testthat::expect_true("classif.ece" %in% mlr3::mlr_measures$keys())
  testthat::expect_true("calibrate" %in% mlr3pipelines::mlr_pipeops$keys())
})

test_that(".onLoad replaces stale same-key entries on reload", {
  on.exit(mlr3calibration:::.onLoad(NULL, "mlr3calibration"), add = TRUE)

  stale = function(...) "stale"
  mlr3::mlr_measures$remove("classif.ece")
  mlr3::mlr_measures$add("classif.ece", stale)
  mlr3calibration:::.onLoad(NULL, "mlr3calibration")

  entry = get("classif.ece", envir = mlr3::mlr_measures$items, inherits = FALSE)$value
  testthat::expect_identical(entry, mlr3calibration:::mlr3calibration_measures[["classif.ece"]])
})
