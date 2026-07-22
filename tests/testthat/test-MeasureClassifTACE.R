test_that("MeasureClassifTACE registers with the expected metadata and defaults", {
  measure = mlr3::msr("classif.tace")

  testthat::expect_true(inherits(measure, "MeasureClassifTACE"))
  testthat::expect_equal(measure$id, "classif.tace")
  testthat::expect_equal(measure$label, "Thresholded Adaptive Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.tace")
  testthat::expect_identical(measure$task_properties, character(0L))
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$ranges, 15L)
  testthat::expect_identical(measure$param_set$values$threshold, 0.01)
})

test_that("MeasureClassifTACE with threshold zero matches ACE on strictly positive probabilities", {
  predicted = c(0.1, 0.4, 0.6, 0.9)
  actual = c(0, 1, 0, 1)
  prediction = make_manual_binary_prediction(predicted, actual)

  ace = score1(prediction, "classif.ace", ranges = 2L)
  tace = score1(prediction, "classif.tace", ranges = 2L, threshold = 0)
  testthat::expect_equal(tace, ace)
})

test_that("MeasureClassifTACE excludes each class's low values before forming ranges", {
  prob = matrix(
    c(
      0.60, 0.35, 0.05,
      0.70, 0.25, 0.05,
      0.20, 0.75, 0.05,
      0.15, 0.80, 0.05,
      0.30, 0.25, 0.45,
      0.20, 0.20, 0.60
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  truth = c("A", "A", "B", "B", "C", "C")
  row_ids = 1:6
  prediction = make_manual_prediction(prob, truth = truth, row_ids = row_ids)

  # Independent reference: filter p_k > threshold per class, then form adaptive ranges on what remains.
  ref_tace = function(prob, truth, row_ids, ranges, threshold) {
    per_class = vapply(colnames(prob), function(k) {
      p = prob[, k]
      keep = p > threshold
      ref_adaptive_gaps(as.integer(truth == k)[keep], p[keep], row_ids[keep], ranges)
    }, numeric(1L))
    mean(per_class)
  }

  testthat::expect_equal(
    score1(prediction, "classif.tace", ranges = 2L, threshold = 0.1),
    ref_tace(prob, truth, row_ids, 2L, 0.1)
  )
})

test_that("MeasureClassifTACE uses a strict threshold and reports classes with too few retained values", {
  prob = matrix(
    c(
      0.5, 0.5,
      0.5, 0.5,
      0.9, 0.1
    ),
    ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("A", "B"))
  )
  prediction = make_manual_prediction(prob, truth = c("A", "B", "A"))

  # Strict `>` drops the two class-A values equal to 0.5, leaving one retained value for three ranges.
  testthat::expect_error(
    prediction$score(mlr3::msr("classif.tace", ranges = 3L, threshold = 0.5)),
    "class 'A' retained only 1 observations above threshold 0.5 but 3[[:space:]]+ranges"
  )
})

test_that("MeasureClassifTACE scores real multiclass predictions in the unit interval", {
  data = make_multiclass_prediction_data()
  score = data$prediction$score(mlr3::msr("classif.tace", ranges = 3L))
  checkmate::expect_number(score, lower = 0, upper = 1)
})
