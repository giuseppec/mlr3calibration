test_that("MeasureClassifSCE registers with the expected metadata", {
  measure = mlr3::msr("classif.sce")

  testthat::expect_true(inherits(measure, "MeasureClassifSCE"))
  testthat::expect_equal(measure$id, "classif.sce")
  testthat::expect_equal(measure$label, "Static Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.sce")
  testthat::expect_identical(measure$task_properties, character(0L))
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$bins, 15L)
})

test_that("MeasureClassifSCE matches the Nixon formula and the mean of classwise Kull ECE", {
  prob = matrix(
    c(
      0.6, 0.3, 0.1,
      0.7, 0.2, 0.1,
      0.2, 0.7, 0.1,
      0.1, 0.8, 0.1,
      0.3, 0.3, 0.4,
      0.2, 0.2, 0.6
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  truth = c("A", "A", "B", "C", "C", "B")
  prediction = make_manual_prediction(prob, truth = truth)

  score = score1(prediction, "classif.sce", bins = 2L)

  # Independent Nixon-form reference: (1 / K) * sum_k sum_b (n_bk / N) * |acc_bk - conf_bk|.
  testthat::expect_equal(score, ref_classwise_ece(prob, truth, 2L))

  # Algebraically identical to the mean of per-class equal-width (Kull) ECE values.
  per_class = vapply(
    colnames(prob),
    function(k) ref_equal_width_ece(as.integer(truth == k), prob[, k], 2L),
    numeric(1L)
  )
  testthat::expect_equal(score, mean(per_class))
})

test_that("MeasureClassifSCE detects a miscalibrated non-maximum class that confidence-ECE ignores", {
  prob = matrix(rep(c(0.6, 0.1, 0.3), times = 10L), ncol = 3L, byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C")))
  truth = c(rep("A", 6L), rep("B", 4L))
  prediction = make_manual_prediction(prob, truth = truth)

  # Confidence-ECE only sees the well-calibrated maximum column A (acc 0.6, conf 0.6).
  testthat::expect_equal(score1(prediction, "classif.conf_ece"), 0)

  # SCE additionally scores columns B (acc 0.4 vs conf 0.1) and C (acc 0 vs conf 0.3).
  testthat::expect_equal(score1(prediction, "classif.sce"), mean(c(0, 0.3, 0.3)))
})

test_that("MeasureClassifSCE is invariant to a joint permutation of columns and class names", {
  prob = matrix(
    c(
      0.6, 0.3, 0.1,
      0.2, 0.7, 0.1,
      0.3, 0.3, 0.4,
      0.1, 0.1, 0.8
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  truth = c("A", "B", "C", "C")
  prediction = make_manual_prediction(prob, truth = truth)

  permutation = c(3L, 1L, 2L)
  permuted_prob = prob[, permutation]
  permuted_prediction = make_manual_prediction(permuted_prob, truth = truth)

  testthat::expect_equal(
    score1(permuted_prediction, "classif.sce", bins = 3L),
    score1(prediction, "classif.sce", bins = 3L)
  )
})

test_that("MeasureClassifSCE weights classes equally under imbalance and keeps unpredicted classes", {
  # Class C is never the argmax but its probability column still contributes to SCE.
  prob = matrix(rep(c(0.6, 0.1, 0.3), times = 10L), ncol = 3L, byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C")))
  truth = c(rep("A", 6L), rep("B", 4L))
  prediction = make_manual_prediction(prob, truth = truth)

  # Equal 1 / K averaging: summing the three class contributions and dividing by 3 (not by prevalence).
  expected = (0 + 0.3 + 0.3) / 3
  testthat::expect_equal(score1(prediction, "classif.sce"), expected)
})
