test_that("MeasureClassifECE registers with the expected metadata and defaults", {
  measure = mlr3::msr("classif.ece")

  testthat::expect_true(inherits(measure, "MeasureClassifECE"))
  testthat::expect_equal(measure$id, "classif.ece")
  testthat::expect_equal(measure$label, "Expected Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.ece")
  testthat::expect_identical(measure$task_properties, "twoclass")
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$bins, 10L)
  testthat::expect_false("method" %in% measure$param_set$ids())
})

test_that("MeasureClassifECE uses equal-width bins for the positive class", {
  # First column "yes" is the positive class. Predictions land clearly inside two width-0.5 bins.
  predicted = c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9)
  actual = c(0, 0, 1, 0, 1, 1, 0, 1)
  prediction = make_manual_binary_prediction(predicted, actual)

  testthat::expect_equal(score1(prediction, "classif.ece"), ref_equal_width_ece(actual, predicted, 10L))
  testthat::expect_equal(
    score1(prediction, "classif.ece", bins = 2L),
    ref_equal_width_ece(actual, predicted, 2L)
  )
})

test_that("MeasureClassifECE stays binary and keys off the first probability column", {
  data = make_multiclass_prediction_data()
  # The binary key rejects multiclass predictions rather than silently becoming confidence-ECE.
  testthat::expect_error(data$prediction$score(mlr3::msr("classif.ece")), "binary")

  # Two rows share confidence 0.7 but land in different first-column bins, so classif.ece (first column "yes")
  # differs from confidence-ECE (pooled maximum probability).
  prob = matrix(c(0.3, 0.7, 0.7, 0.3), ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("yes", "no")))
  prediction = make_manual_prediction(prob, truth = c("no", "no"))

  testthat::expect_equal(score1(prediction, "classif.ece"), ref_equal_width_ece(c(0, 0), c(0.3, 0.7), 10L))
  testthat::expect_false(isTRUE(all.equal(
    score1(prediction, "classif.ece"),
    score1(prediction, "classif.conf_ece")
  )))
})
