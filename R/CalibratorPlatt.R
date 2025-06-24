CalibratorPlatt <- R6::R6Class("CalibratorPlatt",
                           inherit = Calibrator,
                           public = list(
                             calib = NULL,

                             initialize = function(calibration_data, task) {
                               task = task
                               positive = task$positive
                               # For example, fit a logistic regression model
                               task_for_calibrator = as_task_classif(calibration_data,
                                                                     target = "truth",
                                                                     positive = positive,
                                                                     id = "Task_cal")
                               self$calib = lrn("classif.log_reg", predict_type = "prob")
                               self$calib$train(task_for_calibrator)
                             },

                             predict = function(calibration_data, task) {
                               task = task
                               positive = task$positive
                               task_for_calibrator = as_task_classif(calibration_data,
                                                                     target = "truth",
                                                                     positive = positive,
                                                                     id = "Task_cal")
                               pred_calibrated = self$calib$predict(task_for_calibrator)
                               return(pred_calibrated)
                             }
                           )
)
