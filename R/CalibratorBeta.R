#' @title Beta Calibrator
#'
#' @name beta
#'
#' @description
#' Beta Calibration.
#' @description
#' Creates a new instance of this [R6][R6::R6Class] class
#' @param task contains the calibration task with two columns truth and response target is truth and positive is the positive of original task

CalibratorBeta = R6::R6Class("CalibratorBeta",
                              inherit = LearnerClassif,

                              public = list(

                                #' @description
                                #' Creates a new instance of this [R6][R6::R6Class] class.
                                initialize = function(parameters = "abm") {

                                  ps = paradox::ps(
                                    parameters = paradox::p_fct(
                                      levels = c("abm", "ab"),
                                      default = "abm",
                                      tags = "train"
                                    )
                                  )

                                  super$initialize(
                                    id = "beta",
                                    param_set = ps,
                                    predict_types = c("response", "prob"),
                                    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                    properties = c("weights", "twoclass", "offset"),
                                    packages = c("mlr3learners", "stats"),
                                    label = "beta",
                                  )

                                  private$.calibrator = NULL
                                  self$param_set$values$parameters = parameters

                                }

                              ),
                              private = list(
                                .calibrator = NULL,

                                .train = function(task) {
                                  data <- task$data()
                                  parameters <- self$param_set$values$parameters
                                  data$truth <- ifelse(data$truth == task$positive,
                                                       1, 0)
                                  private$.calibrator = betacal::beta_calibration(p = data$response,
                                                                         y = data$truth,
                                                                         parameters = parameters)
                                },

                                .predict = function(task) {
                                  data <- task$data()
                                  pred_calibrated = betacal::beta_predict(data$response,
                                                                          private$.calibrator)
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




