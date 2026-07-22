test_that("MeasureClassifCoxSlope scores predictions", {
  data = make_binary_prediction_data()
  measure = mlr3::msr("classif.cox_slope")

  testthat::expect_true(inherits(measure, "MeasureClassifCoxSlope"))
  testthat::expect_equal(measure$id, "classif.cox_slope")
  testthat::expect_equal(measure$label, "Cox Calibration Slope")
  testthat::expect_equal(measure$man, "mlr_measures_classif.cox_slope")
  testthat::expect_identical(measure$task_properties, "twoclass")
  checkmate::expect_number(data$prediction$score(measure))
  checkmate::expect_number(data$prediction$score(MeasureClassifCoxSlope$new()))
})

test_that("Cox slope is one for empirically exact calibration and has no monotone direction", {
  predicted = rep(seq(0.1, 0.9, by = 0.1), each = 10L)
  actual = unlist(lapply(seq_len(9L), function(positives) {
    c(rep(1, positives), rep(0, 10L - positives))
  }))
  prediction = make_manual_binary_prediction(predicted, actual)
  measure = MeasureClassifCoxSlope$new()

  # Matching empirical and predicted odds makes the fitted relation logit(y) = 0 + 1 * logit(p).
  testthat::expect_equal(unname(prediction$score(measure)), 1, tolerance = 1e-7)
  testthat::expect_true(is.na(measure$minimize))
})
