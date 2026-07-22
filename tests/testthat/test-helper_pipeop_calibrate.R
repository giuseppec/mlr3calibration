testthat::skip_if_not_installed("rpart")

test_that("learner and resampling helpers validate inputs", {
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  rr = mlr3::resample(tasks$train, learner$clone(), mlr3::rsmp("cv", folds = 2), store_models = TRUE)
  autotuner = make_autotuner(learner$clone())
  graph = get("%>>%", asNamespace("mlr3pipelines"))(
    mlr3pipelines::po("scale"),
    mlr3pipelines::po("learner", learner = learner$clone())
  )

  testthat::expect_invisible(assert_probability_learner(learner))
  testthat::expect_invisible(assert_autotuner_learner(autotuner))
  testthat::expect_error(
    assert_probability_learner(mlr3::lrn("classif.rpart", predict_type = "response")),
    "predict_type has to be 'prob'"
  )
  testthat::expect_true(inherits(calibration_pipeop_learner(rr = rr), "Learner"))
  testthat::expect_true(inherits(calibration_pipeop_learner(learner = graph), "GraphLearner"))
  testthat::expect_identical(learner_param_namespace(learner), "classif.rpart")
  testthat::expect_null(learner_param_namespace(calibration_pipeop_learner(learner = graph)))
  testthat::expect_identical(calibrator_param_namespace(clb("beta")), "beta")
  testthat::expect_null(calibrator_param_namespace(clb("selector", choices = c("platt", "beta"))))
  testthat::expect_error(
    calibration_pipeop_learner(learner = autotuner),
    "tune_then_calibrate"
  )
  testthat::expect_identical(calibration_pipeop_resampling()$id, "cv")
  testthat::expect_identical(calibration_pipeop_resampling(rr = rr)$id, rr$resampling$id)
  testthat::expect_identical(calibration_resample_result(tasks$train, learner, rr$resampling, rr = rr), rr)
  testthat::expect_true(inherits(
    calibration_resample_result(tasks$train, learner, mlr3::rsmp("cv", folds = 2)),
    "ResampleResult"
  ))
})
