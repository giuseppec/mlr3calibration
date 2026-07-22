test_that("MeasureClassifACE registers with the expected metadata", {
  measure = mlr3::msr("classif.ace")

  testthat::expect_true(inherits(measure, "MeasureClassifACE"))
  testthat::expect_equal(measure$id, "classif.ace")
  testthat::expect_equal(measure$label, "Adaptive Calibration Error")
  testthat::expect_equal(measure$man, "mlr_measures_classif.ace")
  testthat::expect_identical(measure$task_properties, character(0L))
  testthat::expect_identical(measure$range, c(0, 1))
  testthat::expect_true(measure$minimize)
  testthat::expect_identical(measure$param_set$values$ranges, 15L)
})

test_that("MeasureClassifACE matches a hand-computed adaptive-range calculation", {
  predicted = c(0.1, 0.4, 0.6, 0.9)
  actual = c(0, 1, 0, 1)
  prediction = make_manual_binary_prediction(predicted, actual)

  # Two ranges of two per class. Each range has acc 0.5, and confidences 0.25 / 0.75, giving gap 0.25 throughout.
  testthat::expect_equal(score1(prediction, "classif.ace", ranges = 2L), 0.25)
})

test_that("MeasureClassifACE agrees with the independent classwise reference on multiclass data", {
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
  row_ids = 11:16
  prediction = make_manual_prediction(prob, truth = truth, row_ids = row_ids)

  testthat::expect_equal(
    score1(prediction, "classif.ace", ranges = 3L),
    ref_ace(prob, truth, row_ids, 3L)
  )
})

test_that("MeasureClassifACE is invariant to row order and to a joint column/name permutation", {
  prob = matrix(
    c(
      0.6, 0.3, 0.1,
      0.2, 0.7, 0.1,
      0.3, 0.3, 0.4,
      0.1, 0.1, 0.8,
      0.5, 0.4, 0.1,
      0.2, 0.5, 0.3
    ),
    ncol = 3L, byrow = TRUE, dimnames = list(NULL, c("A", "B", "C"))
  )
  truth = c("A", "B", "C", "C", "A", "B")
  row_ids = 1:6
  prediction = make_manual_prediction(prob, truth = truth, row_ids = row_ids)
  baseline = score1(prediction, "classif.ace", ranges = 3L)

  # Reordering rows while keeping their row IDs does not change the result.
  perm = c(4L, 1L, 6L, 2L, 5L, 3L)
  reordered = make_manual_prediction(prob[perm, ], truth = truth[perm], row_ids = row_ids[perm])
  testthat::expect_equal(score1(reordered, "classif.ace", ranges = 3L), baseline)

  # Jointly permuting probability columns and their names leaves the macro average unchanged.
  column_permutation = c(3L, 1L, 2L)
  permuted = make_manual_prediction(prob[, column_permutation], truth = truth, row_ids = row_ids)
  testthat::expect_equal(score1(permuted, "classif.ace", ranges = 3L), baseline)
})

test_that("MeasureClassifACE fails when a class has fewer observations than ranges", {
  prob = matrix(
    c(
      0.6, 0.4,
      0.3, 0.7,
      0.5, 0.5
    ),
    ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("A", "B"))
  )
  prediction = make_manual_prediction(prob, truth = c("A", "B", "A"))
  testthat::expect_error(prediction$score(mlr3::msr("classif.ace", ranges = 5L)), "at least 5 observations")
})

test_that("ACE and TACE score pooled repeated-resampling predictions", {
  task = mlr3::as_task_classif(iris, target = "Species")
  rr = mlr3::resample(
    task,
    mlr3::lrn("classif.featureless", predict_type = "prob"),
    mlr3::rsmp("repeated_cv", folds = 3L, repeats = 2L)
  )
  prediction = rr$prediction()

  testthat::expect_true(anyDuplicated(prediction$row_ids) != 0L)
  checkmate::expect_number(score1(prediction, "classif.ace", ranges = 3L), lower = 0, upper = 1)
  checkmate::expect_number(score1(prediction, "classif.tace", ranges = 3L, threshold = 0), lower = 0, upper = 1)
})
