#' @title Spiegelhalter's Z statistic
#'
#' @description
#' Compares predicted probabilities to observed outcomes using Spiegelhalter's Z test.
#'
#' The constructor argument `type` selects the immutable output mode:
#'
#' - `type = "pvalue"` (default) returns the two-sided p-value with
#'   `range = c(0, 1)` and `minimize = NA`.
#' - `type = "statistic"` returns the signed z score with
#'   `range = c(-Inf, Inf)` and `minimize = NA`.
#' Both modes retain the dictionary ID `"classif.spiegelhaltersz"` and must therefore be scored in separate calls.
#'
#' Neither output is a good primary tuning objective: a larger p-value can reflect low power,
#' and the signed statistic has ideal value zero with larger absolute values indicating stronger miscalibration
#' (`minimize = NA` for both).
#'
#' Both modes return `NaN` when the variance denominator is zero,
#' which happens when every predicted probability lies in \eqn{\{0, 0.5, 1\}}.
#'
#' @param type (`character(1)`)\cr
#'   Output type, fixed at construction.
#'   Either `"pvalue"` (default) or `"statistic"`.
#'
#' @field type (`character(1)`)\cr
#'   Read-only output type selected at construction.
#'
#' @references
#' `r format_bib("spiegelhalter1986")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   prediction = learner$predict(task)
#'   prediction$score(mlr3::msr("classif.spiegelhaltersz"))
#'   prediction$score(mlr3::msr("classif.spiegelhaltersz", type = "statistic"))
#' }
#'
#' @aliases mlr_measures_classif.spiegelhaltersz
#' @export
MeasureClassifSpiegelhaltersZ = R6Class(
  "MeasureClassifSpiegelhaltersZ",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifSpiegelhaltersZ` object.
    initialize = function(type = "pvalue") {
      assert_choice(type, c("pvalue", "statistic"))
      private$.type = type

      measure_range = if (identical(type, "pvalue")) c(0, 1) else c(-Inf, Inf)

      super$initialize(
        id = "classif.spiegelhaltersz",
        label = "Spiegelhalter's Z",
        man = "mlr_measures_classif.spiegelhaltersz",
        packages = character(),
        properties = character(),
        task_properties = "twoclass",
        predict_type = "prob",
        range = measure_range,
        minimize = NA
      )
    }
  ),
  active = list(
    type = function(rhs) {
      if (!missing(rhs)) {
        cli_abort("`$type` is read-only.")
      }
      private$.type
    }
  ),
  private = list(
    .type = NULL,
    .extra_hash = "type",
    .score = function(prediction, ...) {
      inputs = binary_prediction_inputs(prediction)
      actual = inputs$actual
      predicted = inputs$predicted
      numerator = sum((actual - predicted) * (1 - 2 * predicted))
      denominator = sqrt(sum((1 - 2 * predicted)^2 * predicted * (1 - predicted)))

      # Undefined statistic on valid data (every prediction in {0, 0.5, 1}):
      # follow the mlr3measures na_value convention instead of aborting.
      if (denominator == 0) {
        return(NaN)
      }

      z_score = numerator / denominator

      if (identical(private$.type, "statistic")) {
        return(z_score)
      }

      2 * (1 - pnorm(abs(z_score)))
    }
  )
)
