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

  # Create Calibrator
  calib_platt = CalibratorPlatt$new(calibration_data)
  # Predict with Calibrator
  pred_calibrated = calib_platt$predict(calibration_data)
  checkmate::expect_numeric(mean(pred_calibrated$prob.M))
})
