# Tests for PipeOpCalibrationOOF

test_that("platt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationOOF$new(learner = learner,
                            method = "platt", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("beta", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)
  learner <- lrn("classif.rpart", predict_type = "prob")

  # Train
  learner_cal <- as_learner(PipeOpCalibrationOOF$new(learner = learner,
                                                  method = "beta",
                                                  parameters = "ab", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})


test_that("isotonic", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Isotonic
  learner_cal <- as_learner(PipeOpCalibrationOOF$new(learner = learner,
                                                  method = "isotonic", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

# Tests for PipeOpCalibrationPerFold

test_that("platt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationPerFold$new(learner = learner,
                                                     method = "platt", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("beta", {
  set.seed(5)
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)
  learner <- lrn("classif.rpart", predict_type = "prob") #TODO make it work for "classif.rpart"

  # Train
  # Problems: parameter is set "abm" even if we pass "ab"
  # install try catch for beta error (seed(5))
  learner_cal <- as_learner(PipeOpCalibrationPerFold$new(learner = learner,
                                                     method = "beta",
                                                     parameters = "ab", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("beta", {
  set.seed(5)
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)
  learner <- lrn("classif.rpart", predict_type = "prob") #TODO make it work for "classif.rpart"

  # Train
  # Problems: parameter is set "abm" even if we pass "ab"
  # install try catch for beta error (seed(5))
  learner_cal <- as_learner(PipeOpCalibrationPerFold$new(learner = learner,
                                                         method = "beta",
                                                         parameters = "abm", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("isotonic", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Isotonic
  learner_cal <- as_learner(PipeOpCalibrationPerFold$new(learner = learner,
                                                     method = "isotonic", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("predict_type", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "response")
  testthat::expect_error(as_learner(PipeOpCalibrationPerFold$new(learner = learner)))
})

# Tests for PipeOpCalibrationTuneFirst

test_that("platt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationTuneFirst$new(learner = learner,
                                                         method = "platt", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("beta", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)
  learner <- lrn("classif.rpart", predict_type = "prob")

  # Train
  learner_cal <- as_learner(PipeOpCalibrationTuneFirst$new(learner = learner,
                                                         method = "beta",
                                                         parameters = "ab", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("isotonic", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Isotonic
  learner_cal <- as_learner(PipeOpCalibrationTuneFirst$new(learner = learner,
                                                         method = "isotonic", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

# Tests for PipeOpCalibrationTuneFirstOOF

test_that("platt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")

  # Define tuner
  tuner = mlr3tuning::tnr("grid_search", resolution = 10)

  # AutoTuner
  at = mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.acc"),
    search_space = paradox::ps(
      cp = paradox::p_dbl(0.001,0.1)
    ),
    terminator = trm("evals", n_evals = 10),
    tuner = tuner
  )
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationTuneFirstOOF$new(learner = at,
                                                           method = "platt", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("beta", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")

  # Define tuner
  tuner = mlr3tuning::tnr("grid_search", resolution = 10)

  # AutoTuner
  at = mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.acc"),
    search_space = paradox::ps(
      cp = paradox::p_dbl(0.001,0.1)
    ),
    terminator = trm("evals", n_evals = 10),
    tuner = tuner
  )
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationTuneFirstOOF$new(learner = at,
                                                              method = "beta", parameters = "abm", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})

test_that("isotonic", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")

  # Define tuner
  tuner = mlr3tuning::tnr("grid_search", resolution = 10)

  # AutoTuner
  at = mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.acc"),
    search_space = paradox::ps(
      cp = paradox::p_dbl(0.001,0.1)
    ),
    terminator = trm("evals", n_evals = 10),
    tuner = tuner
  )
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  learner_cal <- as_learner(PipeOpCalibrationTuneFirstOOF$new(learner = at,
                                                              method = "isotonic", rsmp = rsmp("cv", folds = 5)))
  learner_cal$train(task_train)
  checkmate::expect_numeric(learner_cal$state$train_time)

  # Predict
  preds = learner_cal$predict(task_test)
  checkmate::expect_numeric(mean(preds$prob[,1]))
})
