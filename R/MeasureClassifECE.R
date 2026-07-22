#' @title Binary expected calibration error
#'
#' @description
#' Binary ECE for the modeled positive class (first probability column).
#' Dictionary key `classif.ece`; default `bins = 10`.
#' For multiclass targets use [MeasureClassifConfidenceECE], [MeasureClassifSCE],
#' [MeasureClassifACE], [MeasureClassifTACE], or [MeasureClassifTopLabelECE].
#'
#' Uses fixed-width probability bins and computes the single-class component of
#' Kull et al. (2019), Eq. 4, for positive class `c`:
#' \deqn{\mathrm{ECE}_c = \sum_{b = 1}^{B} \frac{|B_{bc}|}{N}\,\left|\mathrm{acc}(B_{bc}, c) - \mathrm{conf}(B_{bc}, c)\right|}
#'
#' Fixed-width bins use right-closed intervals \eqn{((b - 1)/B, b/B]}; probability zero goes in the first bin;
#' empty bins have zero weight.
#' For equal-count adaptive ranges, use [MeasureClassifACE] instead.
#'
#' @references
#' `r format_bib("kull2019")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.ece"))
#' }
#'
#' @aliases mlr_measures_classif.ece
#' @export
MeasureClassifECE = R6Class(
  "MeasureClassifECE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifECE` object.
    initialize = function() {
      super$initialize(
        id = "classif.ece",
        label = "Expected Calibration Error",
        man = "mlr_measures_classif.ece",
        packages = character(),
        param_set = ps(
          bins = p_int(lower = 1L, init = 10L)
        ),
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
      bins = self$param_set$values$bins

      if (is.null(bins)) {
        bins = 10L
      }

      inputs = binary_prediction_inputs(prediction)
      equal_width_calibration_error(
        actual = inputs$actual,
        predicted = inputs$predicted,
        bins = bins
      )
    }
  )
)
