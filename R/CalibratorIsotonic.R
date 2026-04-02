#' @title Platt Isotonic
#'
#' @name isotonic
#'
#' @description
#' Creates a new instance of this [R6][R6::R6Class] class
#' @param task contains the calibration task with two columns truth and response target is truth and positive is the positive of original task

CalibratorIsotonic = R6::R6Class("CalibratorIsotonic",
                              inherit = LearnerClassif,

                              public = list(

                                #' @description
                                #' Creates a new instance of this [R6][R6::R6Class] class.
                                initialize = function() {

                                  ps = paradox::ps()

                                  super$initialize(
                                    id = "isotonic",
                                    param_set = ps,
                                    predict_types = c("response", "prob"),
                                    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                    properties = c("weights", "twoclass", "offset"),
                                    packages = c("mlr3learners", "stats"),
                                    label = "isotonic",
                                  )

                                  private$.calibrator = NULL

                                }

                              ),
                              private = list(
                                .calibrator = NULL,

                                .train = function(task) {
                                  data <- task$data()
                                  data$truth <- ifelse(data$truth == task$positive,
                                                                   1, 0)
                                  private$.calibrator = as.stepfun(stats::isoreg(x = data$response,
                                                                        y = data$truth))
                                },

                                .predict = function(task) {
                                  positive = task$positive
                                  data <- task$data()
                                  pred_calibrated = private$.calibrator(
                                    data$response)
                                  prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
                                  colnames(prob) = c(task$positive, task$negative)
                                  response = ifelse(pred_calibrated < 0.5, task$negative,
                                                    task$positive)
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

