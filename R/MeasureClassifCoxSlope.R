#' @title Cox calibration slope
#'
#' @description
#' Fits a logistic regression model with the log-odds of the predicted
#' probabilities as the independent variable and returns the fitted slope.
#' Perfect calibration corresponds to a slope of 1.
#' Slope below 1 indicates overconfidence; above 1 indicates underconfidence.
#' No single optimization direction (`minimize = NA`); use as a diagnostic, not a tuning objective.
#'
#' @references
#' `r format_bib("cox1958")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.cox_slope"))
#' }
#'
#' @aliases mlr_measures_classif.cox_slope
#' @export
MeasureClassifCoxSlope = R6Class(
  "MeasureClassifCoxSlope",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifCoxSlope` object.
    initialize = function() {
      super$initialize(
        id = "classif.cox_slope",
        label = "Cox Calibration Slope",
        man = "mlr_measures_classif.cox_slope",
        packages = character(),
        properties = character(),
        task_properties = "twoclass",
        predict_type = "prob",
        range = c(-Inf, Inf),
        minimize = NA
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      inputs = binary_prediction_inputs(prediction)
      logit_model = cox_logit_model(actual = inputs$actual, predicted = inputs$predicted)
      coef(logit_model)[[2L]]
    }
  )
)
