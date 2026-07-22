#' @title Adaptive calibration error
#'
#' @description
#' Adaptive calibration error (ACE) of Nixon et al. (2019) for binary and multiclass predictions.
#' Dictionary key `classif.ace`; default `ranges = 15`.
#' Classwise calibration over adaptive equal-count ranges (vs fixed-width bins in [MeasureClassifSCE]).
#' For each class `k`, the `N` class probabilities are sorted and split into `R` ranges
#' \eqn{A_{rk}} of equal count:
#' \deqn{\mathrm{ACE} = \frac{1}{K R} \sum_{k = 1}^{K} \sum_{r = 1}^{R}
#' \left|\mathrm{acc}(A_{rk}, k) - \mathrm{conf}(A_{rk}, k)\right|}
#' with \eqn{\mathrm{acc}} / \eqn{\mathrm{conf}} the observed class-`k` frequency and mean predicted
#' class-`k` probability in range `r`.
#' Ranges are weighted equally (by construction they hold equal counts).
#'
#' @section Edge conventions:
#' Predictions are sorted by probability then row ID (deterministic tie break).
#' Repeated row IDs, as produced by repeated resampling, are allowed and each prediction is evaluated.
#' When `N` is not divisible by `R`, the first \eqn{N \bmod R} ranges get one extra observation.
#' Every class must retain at least `R` observations.
#' Ranges balance prediction counts, not positive outcomes, so rare classes can still be noisy.
#'
#' @references
#' `r format_bib("nixon2019")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   task = mlr3::as_task_classif(iris, target = "Species")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.ace"))
#' }
#'
#' @aliases mlr_measures_classif.ace
#' @export
MeasureClassifACE = R6Class(
  "MeasureClassifACE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifACE` object.
    initialize = function() {
      super$initialize(
        id = "classif.ace",
        label = "Adaptive Calibration Error",
        man = "mlr_measures_classif.ace",
        packages = character(),
        param_set = ps(
          ranges = p_int(lower = 1L, init = 15L)
        ),
        properties = character(),
        task_properties = character(),
        predict_type = "prob",
        range = c(0, 1),
        minimize = TRUE
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      ranges = self$param_set$values$ranges

      if (is.null(ranges)) {
        ranges = 15L
      }

      classwise_adaptive_calibration_error(prediction, ranges)
    }
  )
)
