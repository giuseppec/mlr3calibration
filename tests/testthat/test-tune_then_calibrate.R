testthat::skip_if_not_installed("rpart")
testthat::skip_if_not_installed("mlr3learners")
testthat::skip_if_not_installed("bbotk")
testthat::skip_if_not_installed("mlr3tuning")

test_that("tune_then_calibrate returns a normal learner", {
  tasks = make_binary_split()
  learner = tune_then_calibrate(
    learner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  )

  testthat::expect_true(inherits(learner, "Learner"))
  testthat::expect_true(inherits(learner, "GraphLearner"))
  testthat::expect_identical(learner$predict_type, "prob")
  expect_calibrated_prediction(learner, tasks$train, tasks$test)
})

test_that("tune_then_calibrate reproduces explicit sequential tuning then calibration", {
  tasks = make_binary_split()
  autotuner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L)
  helper_learner = tune_then_calibrate(
    learner = autotuner$clone(deep = TRUE),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  )

  set.seed(7)
  suppressWarnings(helper_learner$train(tasks$train))
  helper_prediction = helper_learner$predict(tasks$test)

  manual_autotuner = autotuner$clone(deep = TRUE)

  set.seed(7)
  suppressWarnings(manual_autotuner$train(tasks$train))
  manual_learner = mlr3::as_learner(mlr3pipelines::po(
    "calibrate",
    learner = manual_autotuner$learner$clone(deep = TRUE),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  ))
  suppressWarnings(manual_learner$train(tasks$train))
  manual_prediction = manual_learner$predict(tasks$test)

  testthat::expect_equal(helper_prediction$prob, manual_prediction$prob, tolerance = 1e-12)
})

test_that("tune_then_calibrate rejects non-AutoTuner learners", {
  testthat::expect_error(
    tune_then_calibrate(mlr3::lrn("classif.rpart", predict_type = "prob")),
    "AutoTuner"
  )
})

test_that("PipeOpTuneThenCalibrate declares dependencies and deep-clones trained state", {
  tasks = make_binary_split()
  pipeop = PipeOpTuneThenCalibrate$new(
    learner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L),
    calibrator = clb("platt"),
    resampling = mlr3::rsmp("cv", folds = 2)
  )

  testthat::expect_true(all(c("mlr3calibration", "mlr3tuning", "rpart") %in% pipeop$packages))
  suppressWarnings(pipeop$train(list(tasks$train)))
  cloned = pipeop$clone(deep = TRUE)

  testthat::expect_false(identical(
    pipeop$state$calibrated_learner,
    cloned$state$calibrated_learner
  ))
})

test_that("PipeOpTuneThenCalibrate phash distinguishes resampling configurations", {
  make_pipeop = function(folds) {
    PipeOpTuneThenCalibrate$new(
      learner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L),
      resampling = mlr3::rsmp("cv", folds = folds)
    )
  }

  testthat::expect_false(identical(make_pipeop(2L)$phash, make_pipeop(3L)$phash))
})

test_that("PipeOpTuneThenCalibrate clones the caller's resampling", {
  resampling = mlr3::rsmp("cv", folds = 2L)
  pipeop = PipeOpTuneThenCalibrate$new(
    learner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L),
    resampling = resampling
  )

  testthat::expect_false(identical(pipeop$resampling, resampling))
  resampling$param_set$values$folds = 3L
  testthat::expect_identical(pipeop$resampling$param_set$values$folds, 2L)
})

test_that("PipeOpTuneThenCalibrate predict_type rejects invalid assignments", {
  pipeop = PipeOpTuneThenCalibrate$new(
    learner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L),
    calibrator = clb("platt"),
    resampling = mlr3::rsmp("cv", folds = 2)
  )

  testthat::expect_identical(pipeop$predict_type, "prob")
  testthat::expect_error(pipeop$predict_type <- "response", "fixed to 'prob'")
  pipeop$predict_type = "prob"
  testthat::expect_identical(pipeop$predict_type, "prob")
})
