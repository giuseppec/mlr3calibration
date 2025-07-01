#' @title Platt Calibrator
#'
#' @name platt
#'
#' @description
#' Calibration via logistic regression.
#' @description
#' Creates a new instance of this [R6][R6::R6Class] class
#' @param task contains the calibration task with two columns truth and response target is truth and positive is the positive of original task

CalibratorPlatt = R6::R6Class("CalibratorPlatt",
                               inherit = LearnerClassif,

                               public = list(

                                 #' @description
                                 #' Creates a new instance of this [R6][R6::R6Class] class.
                                 initialize = function() {

                                   ps = paradox::ps()

                                   super$initialize(
                                     id = "platt",
                                     param_set = ps,
                                     predict_types = c("response", "prob"),
                                     feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
                                     properties = c("weights", "twoclass", "offset"),
                                     packages = c("mlr3learners", "stats"),
                                     label = "platt",
                                   )

                                   private$.calibrator = mlr3::lrn("classif.log_reg", predict_type = "prob")

                                 }

                               ),
                               private = list(
                                 .calibrator = NULL,

                                 .train = function(task) {
                                   # For example, fit a logistic regression model
                                   private$.calibrator$train(task)
                                 },

                                 .predict = function(task) {
                                   pred_calibrated = private$.calibrator$predict(task)
                                   return(pred_calibrated)
                                 }
                               )
)

