test_that("MeasureClassifICI scores predictions", {
  data = make_binary_prediction_data()
  measure = mlr3::msr("classif.ici")

  testthat::expect_true(inherits(measure, "MeasureClassifICI"))
  testthat::expect_equal(measure$id, "classif.ici")
  testthat::expect_equal(measure$label, "Integrated Calibration Index")
  testthat::expect_equal(measure$man, "mlr_measures_classif.ici")
  testthat::expect_identical(measure$task_properties, "twoclass")
  testthat::expect_true("mgcv" %in% measure$packages)
  testthat::expect_false("multiclass" %in% measure$properties)
  checkmate::expect_number(suppressWarnings(data$prediction$score(measure)))
  checkmate::expect_number(suppressWarnings(data$prediction$score(MeasureClassifICI$new())))
})

test_that("MeasureClassifICI keeps rejecting multiclass predictions", {
  data = make_multiclass_prediction_data()
  testthat::expect_error(data$prediction$score(mlr3::msr("classif.ici")), "binary")
})

test_that("ICI matches exact linear calibration curves in the LOESS branch", {
  predicted = rep(seq(0.1, 0.9, by = 0.1), each = 10L)
  calibrated_actual = unlist(lapply(seq_len(9L), function(positives) {
    c(rep(1, positives), rep(0, 10L - positives))
  }))
  inverse_actual = unlist(lapply(seq_len(9L), function(index) {
    c(rep(1, 10L - index), rep(0, index))
  }))

  # LOESS reproduces each linear group mean. The calibrated curve has ICI 0; the inverse curve has mean |1 - 2p| = 4/9.
  testthat::expect_equal(
    integrated_calibration_index(calibrated_actual, predicted),
    0,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    integrated_calibration_index(inverse_actual, predicted),
    4 / 9,
    tolerance = 1e-12
  )
})

test_that("ICI matches exact linear calibration curves in the GAM branch", {
  predicted_values = seq(0.05, 0.95, by = 0.1)
  predicted = rep(predicted_values, each = 100L)
  calibrated_actual = unlist(lapply(seq(5L, 95L, by = 10L), function(positives) {
    c(rep(1, positives), rep(0, 100L - positives))
  }))
  inverse_actual = unlist(lapply(seq(5L, 95L, by = 10L), function(positives) {
    c(rep(1, 100L - positives), rep(0, positives))
  }))

  # A cubic regression spline contains linear functions. The inverse curve has mean |1 - 2p| = 0.5.
  testthat::expect_equal(
    integrated_calibration_index(calibrated_actual, predicted),
    0,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    integrated_calibration_index(inverse_actual, predicted),
    0.5,
    tolerance = 1e-10
  )
})

test_that("ICI adapts the GAM basis to sparse distinct predictions", {
  for (unique_predictions in 3:9) {
    predicted_values = seq(0.2, 0.8, length.out = unique_predictions)
    observations_per_value = 2L * ceiling(500 / unique_predictions)
    predicted = rep(predicted_values, each = observations_per_value)
    actual = rep(rep(c(0, 1), observations_per_value / 2L), unique_predictions)

    ici = integrated_calibration_index(actual, predicted)

    testthat::expect_true(is.finite(ici), info = sprintf("unique predictions: %d", unique_predictions))
  }
})

test_that("ICI returns NaN for data that cannot identify a calibration curve", {
  testthat::expect_identical(
    integrated_calibration_index(rep(1, 4L), c(0.1, 0.2, 0.3, 0.4)),
    NaN
  )
  testthat::expect_identical(
    integrated_calibration_index(c(0, 1, 0, 1), c(0.2, 0.2, 0.2, 0.2)),
    NaN
  )
  testthat::expect_error(
    integrated_calibration_index(c(0, 1), c(0.2)),
    "same length"
  )

  predicted = rep(c(0.25, 0.5, 0.75), each = 20L)
  actual = unlist(lapply(c(5L, 10L, 15L), function(positives) {
    c(rep(1, positives), rep(0, 20L - positives))
  }))
  testthat::expect_equal(
    suppressWarnings(integrated_calibration_index(actual, predicted)),
    0,
    tolerance = 1e-12
  )
})

test_that("ICI returns NaN when the smoother fails", {
  testthat::local_mocked_bindings(
    loess = function(...) stop("smoother failed"),
    .package = "mlr3calibration"
  )

  testthat::expect_identical(
    integrated_calibration_index(c(0, 1, 0), c(0.2, 0.5, 0.8)),
    NaN
  )
})
