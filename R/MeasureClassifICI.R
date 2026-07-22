#' @title Integrated calibration index
#'
#' @description
#' Calculates the integrated calibration index (ICI) for binary classification
#' predictions.
#' A LOESS smoother is used below 1,000 observations, and a cubic regression spline is used otherwise.
#' Returns `NaN` when the calibration sample cannot identify a smooth calibration curve:
#' when only one outcome is observed, when there are fewer than three unique predicted probabilities,
#' or when the smoother fails or produces non-finite fitted values.
#'
#' @references
#' `r format_bib("austin2019")`
#'
#' `r format_bib("weissman2024")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.ici"))
#' }
#'
#' @aliases mlr_measures_classif.ici
#' @export
MeasureClassifICI = R6Class(
  "MeasureClassifICI",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifICI` object.
    initialize = function() {
      super$initialize(
        id = "classif.ici",
        label = "Integrated Calibration Index",
        man = "mlr_measures_classif.ici",
        packages = "mgcv",
        properties = character(),
        task_properties = "twoclass",
        predict_type = "prob",
        range = c(0, 1),
        minimize = TRUE
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      inputs = binary_prediction_inputs(prediction)
      integrated_calibration_index(actual = inputs$actual, predicted = inputs$predicted)
    }
  )
)

integrated_calibration_index = function(actual, predicted) {
  assert_numeric(actual, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(predicted, any.missing = FALSE, null.ok = FALSE)

  if (length(actual) != length(predicted)) {
    cli_abort("actual and predicted must have the same length.")
  }

  if (!all(actual %in% c(0, 1))) {
    cli_abort("actual must contain binary outcomes encoded as 0 and 1.")
  }

  if (any(predicted < 0 | predicted > 1)) {
    cli_abort("predicted probabilities must be between 0 and 1.")
  }

  # Degenerate but valid calibration samples cannot identify a smooth calibration
  # curve: follow the mlr3measures na_value convention instead of aborting.
  unique_predictions = length(unique(predicted))

  if (length(unique(actual)) != 2L || unique_predictions < 3L) {
    return(NaN)
  }

  calibrated = tryCatch(
    if (length(predicted) < 1000L) {
      loess_calibration = loess(actual ~ predicted)
      predict(loess_calibration, newdata = predicted)
    } else {
      basis_dimension = min(10L, unique_predictions)
      gam_calibration = gam(
        actual ~ s(predicted, bs = "cr", k = basis_dimension),
        method = "REML"
      )
      predict(gam_calibration)
    },
    error = function(e) NULL
  )

  if (is.null(calibrated) || any(!is.finite(calibrated))) {
    return(NaN)
  }

  mean(abs(calibrated - predicted))
}
