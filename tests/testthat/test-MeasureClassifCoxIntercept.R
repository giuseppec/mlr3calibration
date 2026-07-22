test_that("MeasureClassifCoxIntercept scores predictions", {
  data = make_binary_prediction_data()
  measure = mlr3::msr("classif.cox_intercept")

  testthat::expect_true(inherits(measure, "MeasureClassifCoxIntercept"))
  testthat::expect_equal(measure$id, "classif.cox_intercept")
  testthat::expect_equal(measure$label, "Cox Calibration Intercept")
  testthat::expect_equal(measure$man, "mlr_measures_classif.cox_intercept")
  testthat::expect_identical(measure$task_properties, "twoclass")
  checkmate::expect_number(data$prediction$score(measure))
  checkmate::expect_number(data$prediction$score(MeasureClassifCoxIntercept$new()))
})

test_that("Cox intercept is zero for empirically exact calibration and has no monotone direction", {
  predicted = rep(seq(0.1, 0.9, by = 0.1), each = 10L)
  actual = unlist(lapply(seq_len(9L), function(positives) {
    c(rep(1, positives), rep(0, 10L - positives))
  }))
  prediction = make_manual_binary_prediction(predicted, actual)
  measure = MeasureClassifCoxIntercept$new()

  # Each probability p occurs ten times with exactly 10 * p positives, so empirical log odds equal logit(p).
  testthat::expect_equal(unname(prediction$score(measure)), 0, tolerance = 1e-7)
  testthat::expect_true(is.na(measure$minimize))
})
