# Independent binned TL-ECE reference weighting every (class, bin) cell by |T_bk| / N.
ref_tl_ece = function(prob, truth, bins) {
  classes = colnames(prob)
  index = max.col(prob, ties.method = "first")
  label = classes[index]
  confidence = prob[cbind(seq_len(nrow(prob)), index)]
  correct = as.integer(as.character(truth) == label)
  n = nrow(prob)

  total = 0
  for (k in classes) {
    selected = which(label == k)
    if (length(selected) == 0L) next
    bin = ref_equal_width_bin(confidence[selected], bins)
    for (b in unique(bin)) {
      cell = selected[bin == b]
      total = total + length(cell) / n * abs(mean(correct[cell]) - mean(confidence[cell]))
    }
  }
  total
}

test_that("MeasureClassifTopLabelECE registers with the expected metadata", {
  measure = mlr3::msr("classif.tl_ece")

  testthat::expect_true(inherits(measure, "MeasureClassifTopLabelECE"))
  testthat::expect_equal(measure$id, "classif.tl_ece")
  testthat::expect_equal(measure$label, "Top-Label Expected Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.tl_ece")
  testthat::expect_identical(measure$task_properties, character(0L))
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$bins, 15L)
})

test_that("MeasureClassifTopLabelECE is positive where confidence-ECE cancels out", {
  # Both predicted labels report confidence 0.6, but label A is under-confident and label B over-confident.
  prob = rbind(
    matrix(rep(c(0.6, 0.4), times = 5L), ncol = 2L, byrow = TRUE),
    matrix(rep(c(0.4, 0.6), times = 5L), ncol = 2L, byrow = TRUE)
  )
  colnames(prob) = c("A", "B")
  truth = c("A", "A", "A", "A", "B", "B", "B", "A", "A", "A")
  prediction = make_manual_prediction(prob, truth = truth)

  # Confidence-ECE pools both labels at confidence 0.6 with mean correctness 0.6, so it is exactly zero.
  testthat::expect_equal(score1(prediction, "classif.conf_ece"), 0)

  # TL-ECE conditions on the predicted label: cell A gap 0.2 and cell B gap 0.2, each weighted 0.5.
  testthat::expect_equal(score1(prediction, "classif.tl_ece"), 0.2)
  testthat::expect_equal(score1(prediction, "classif.tl_ece", bins = 5L), ref_tl_ece(prob, truth, 5L))
})

test_that("MeasureClassifTopLabelECE creates no cell for a class that is never predicted", {
  # Class C is never the argmax, so it contributes no (class, bin) cell to the observation-weighted sum.
  prob = matrix(
    c(
      0.55, 0.35, 0.10,
      0.55, 0.35, 0.10,
      0.55, 0.35, 0.10,
      0.55, 0.35, 0.10,
      0.55, 0.35, 0.10,
      0.35, 0.55, 0.10,
      0.35, 0.55, 0.10,
      0.35, 0.55, 0.10,
      0.35, 0.55, 0.10,
      0.35, 0.55, 0.10
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  truth = c("A", "A", "A", "A", "B", "B", "B", "A", "A", "A")
  prediction = make_manual_prediction(prob, truth = truth)

  score = score1(prediction, "classif.tl_ece")
  testthat::expect_equal(score, ref_tl_ece(prob, truth, 15L))
  # Manual A and B cells only: cell A gap 0.25 weight 0.5, cell B gap 0.15 weight 0.5.
  testthat::expect_equal(score, 0.5 * 0.25 + 0.5 * 0.15)
})

test_that("MeasureClassifTopLabelECE scores real multiclass predictions in the unit interval", {
  data = make_multiclass_prediction_data()
  score = data$prediction$score(mlr3::msr("classif.tl_ece"))
  checkmate::expect_number(score, lower = 0, upper = 1)
})
