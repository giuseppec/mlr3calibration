test_that("MeasureClassifConfidenceECE registers with the expected metadata", {
  measure = mlr3::msr("classif.conf_ece")

  testthat::expect_true(inherits(measure, "MeasureClassifConfidenceECE"))
  testthat::expect_equal(measure$id, "classif.conf_ece")
  testthat::expect_equal(measure$label, "Confidence Expected Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.conf_ece")
  testthat::expect_identical(measure$task_properties, character(0L))
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$bins, 15L)
  testthat::expect_identical(measure$predict_type, "prob")
})

test_that("MeasureClassifConfidenceECE matches Guo Eq. 3 on a hand-computed fixture", {
  prob = matrix(
    c(
      0.7, 0.2, 0.1,
      0.6, 0.3, 0.1,
      0.4, 0.35, 0.25,
      0.3, 0.3, 0.4
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  prediction = make_manual_prediction(prob, truth = c("A", "B", "A", "C"))

  # Confidences 0.7, 0.6, 0.4, 0.4 with two bins; correctness 1, 0, 1, 1.
  # Bin1 {obs 3,4}: acc 1, conf 0.4, gap 0.6, weight 0.5. Bin2 {obs 1,2}: acc 0.5, conf 0.65, gap 0.15, weight 0.5.
  testthat::expect_equal(score1(prediction, "classif.conf_ece", bins = 2L), 0.375)

  # Independent equal-width reference over pooled confidences agrees for a finer binning.
  confidence = apply(prob, 1L, max)
  correct = c(1L, 0L, 1L, 1L)
  testthat::expect_equal(
    score1(prediction, "classif.conf_ece", bins = 5L),
    ref_equal_width_ece(correct, confidence, 5L)
  )
})

test_that("MeasureClassifConfidenceECE breaks top-probability ties by column order", {
  prob = matrix(
    c(
      0.45, 0.45, 0.10,
      0.45, 0.45, 0.10
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  # Classes A and B tie for the maximum. ties.method = "first" chooses column A.
  # Truth is B, so the chosen label A is wrong: acc 0 versus confidence 0.45 gives gap 0.45.
  # Had the tie resolved to B instead, the score would be |1 - 0.45| = 0.55.
  prediction = make_manual_prediction(prob, truth = c("B", "B"))
  testthat::expect_equal(score1(prediction, "classif.conf_ece", bins = 4L), 0.45)
})

test_that("MeasureClassifConfidenceECE scores real multiclass predictions in the unit interval", {
  data = make_multiclass_prediction_data()
  score = data$prediction$score(mlr3::msr("classif.conf_ece"))
  checkmate::expect_number(score, lower = 0, upper = 1)
})
