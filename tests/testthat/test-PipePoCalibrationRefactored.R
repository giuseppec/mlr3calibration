test_that("platt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  # Train
  # TODO
  learner_cal <- as_learner(PipeOpCalibration$new(learner = learner,
                                                      method = "platt",
                                                      protocol = ProtocolPerFold$new(),
                                                      rsmp = rsmp("cv", folds = 5)))
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
  # TODO
  learner_cal <- as_learner(PipeOpCalibration$new(learner = learner,
                                                  method = "beta",
                                                  protocol = ProtocolPerFold$new(),
                                                  parameters = "ab"))
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
  # TODO
  learner_cal <- as_learner(PipeOpCalibration$new(learner = learner,
                                                  method = "isotonic",
                                                  protocol = ProtocolPerFold$new()))
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
  # TODO
  # we should get an error code because of missing arguments? Can it stay like this?
  testthat::expect_error(as_learner(PipeOpCalibration$new(learner = learner)))
})

# TODO write similar tests for PipeOPCalibrationOOF, PipeOpCalibrationTuneFirstPerFold, PipeOpCalibrationTuneFirstOOF


