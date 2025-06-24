#' @title Abstract interface for Calibrator
#'
#' @description
#' Abstract interface for Calibrator
#' Is implemented by Platt scaling, isotonic regression, and beta calibration.
#'
#' @param calibration_data ['data.table']
#'
#' @export

Calibrator <- R6::R6Class("Calibrator",
                        #' @param parameters Parameters for beta calibration. Default is `"abm"`.
                      public = list(
                        #' @description Abstract initializer
                        #' @param calibration_data A data.table with columns "truth" and "response"
                        #' @param task test set of the task, on which the original learner was trained
                        initialize = function(calibration_data, task) {
                          stop("This is an abstract class. Use a subclass.")
                        },
                        #' @description Abstract prediction method
                        #' @param calibration_data A data.table with columns "truth" and "response"
                        #' @param task test set of the task, on which the original learner was trained
                        #' @return A numeric vector of predicted probabilities
                        predict = function(calibration_data, task) {
                          stop("predict() must be implemented in the subclass.")
                        }
                      )
)
