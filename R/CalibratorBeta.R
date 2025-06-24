CalibratorBeta <- R6::R6Class("CalibratorBeta",
                                  inherit = Calibrator,
                                  public = list(
                                    calib = NULL,
                                    parameters = NULL,

                                    initialize = function(calibration_data, task, parameters) {
                                      parameters = "abm"
                                      self$parameters = parameters
                                      task = task
                                      positive = task$positive
                                      calibration_data$truth <- ifelse(calibration_data$truth == positive,
                                                                       1, 0)
                                      self$calib = betacal::beta_calibration(p = calibration_data$response,
                                                                             y = calibration_data$truth,
                                                                             parameters = self$parameters)
                                    },

                                    predict = function(calibration_data, task) {
                                      task = task
                                      positive = task$positive
                                      pred_calibrated = betacal::beta_predict(calibration_data$response,
                                                                              self$calib)
                                      prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
                                      colnames(prob) = c(task$positive, task$negative)
                                      response = ifelse(pred_calibrated < 0.5, task$negative, task$positive)
                                      pred_calibrated = PredictionClassif$new(
                                        task = task,
                                        row_ids = task$row_ids,
                                        truth = task$truth(),
                                        prob = prob,
                                        response = response
                                      )
                                      return(pred_calibrated)
                                    }
                                  )
)
