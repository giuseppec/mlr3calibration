test_that("CalibratorPlatt", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  positive = task$positive
  learner$train(task_train)
  preds = learner$predict(task_test)

  pred_data = as.data.table(preds)
  calibration_data = data.table(truth = pred_data$truth,
                                response = with(pred_data,
                                                get(paste0("prob.", positive))))

  colnames(calibration_data) = c("truth", "response")
  calibration_data$response = as.numeric(calibration_data$response)
  task_for_calibrator = as_task_classif(calibration_data,
                                        target = "truth",
                                        positive = task$positive,
                                        id = "Task_cal")

  # Create Calibrator
  calib_platt = CalibratorPlatt$new()
  calib_platt$train(task_for_calibrator)
  # Predict with Calibrator
  pred_calibrated = calib_platt$predict(task_for_calibrator)
  checkmate::expect_numeric(mean(pred_calibrated$prob[,1]))
})

test_that("CalibratorIsotonic", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  positive = task$positive
  learner$train(task_train)
  preds = learner$predict(task_test)

  pred_data = as.data.table(preds)
  calibration_data = data.table(truth = pred_data$truth,
                                response = with(pred_data,
                                                get(paste0("prob.", positive))))

  colnames(calibration_data) = c("truth", "response")
  calibration_data$response = as.numeric(calibration_data$response)

  task_for_calibrator = as_task_classif(calibration_data,
                                        target = "truth",
                                        positive = task$positive,
                                        id = "Task_cal")
  # Create Calibrator
  calib_isotonic = CalibratorIsotonic$new()
  calib_isotonic$train(task_for_calibrator)
  # Predict with Calibrator
  pred_calibrated = calib_isotonic$predict(task_for_calibrator)
  checkmate::expect_numeric(mean(pred_calibrated$prob[,1]))
})


test_that("CalibratorBeta", {
  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  positive = task$positive
  learner$train(task_train)
  preds = learner$predict(task_test)

  pred_data = as.data.table(preds)
  calibration_data = data.table(truth = pred_data$truth,
                                response = with(pred_data,
                                                get(paste0("prob.", positive))))

  colnames(calibration_data) = c("truth", "response")
  calibration_data$response = as.numeric(calibration_data$response)

  task_for_calibrator = as_task_classif(calibration_data,
                                        target = "truth",
                                        positive = task$positive,
                                        id = "Task_cal")

  # Create Calibrator
  calib_beta = CalibratorBeta$new(parameters = "ab")
  calib_beta$train(task_for_calibrator)
  # Predict with Calibrator
  pred_calibrated = calib_beta$predict(task_for_calibrator)
  checkmate::expect_numeric(mean(pred_calibrated$prob[,1]))
})

test_that("CalibratorDictionary", {

  data("Sonar", package = "mlbench")
  task = as_task_classif(Sonar, target = "Class", positive = "M")
  learner <- lrn("classif.rpart", predict_type = "prob")
  splits = partition(task)
  task_train = task$clone()$filter(splits$train)
  task_test = task$clone()$filter(splits$test)

  positive = task$positive
  learner$train(task_train)
  preds = learner$predict(task_test)

  pred_data = as.data.table(preds)
  calibration_data = data.table(truth = pred_data$truth,
                                response = with(pred_data,
                                                get(paste0("prob.", positive))))


  colnames(calibration_data) = c("truth", "response")
  calibration_data$response = as.numeric(calibration_data$response)

  task_for_calibrator = as_task_classif(calibration_data,
                                        target = "truth",
                                        positive = task$positive,
                                        id = "Task_cal")


  # Calibrator
  cal <- clb("platt")
  cal$train(task_for_calibrator)
  pred_calibrated = cal$predict(task_for_calibrator)

  checkmate::expect_numeric(mean(pred_calibrated$prob[,1]))
})
