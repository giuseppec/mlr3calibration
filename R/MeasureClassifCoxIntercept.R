#' @title Cox calibration intercept
#'
#' @description
#' Fits a logistic regression model with the log-odds of the predicted
#' probabilities as the independent variable and returns the fitted intercept.
#' Perfect calibration corresponds to an intercept of 0.
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
#'   learner$predict(task)$score(mlr3::msr("classif.cox_intercept"))
#' }
#'
#' @aliases mlr_measures_classif.cox_intercept
#' @export
MeasureClassifCoxIntercept = R6Class(
  "MeasureClassifCoxIntercept",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifCoxIntercept` object.
    initialize = function() {
      super$initialize(
        id = "classif.cox_intercept",
        label = "Cox Calibration Intercept",
        man = "mlr_measures_classif.cox_intercept",
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
      coef(logit_model)[[1L]]
    }
  )
)
