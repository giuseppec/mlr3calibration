#' @title Thresholded adaptive calibration error
#'
#' @description
#' Thresholded adaptive calibration error (TACE) of Nixon et al. (2019):
#' [MeasureClassifACE] on class probabilities strictly above `epsilon`
#' (to reduce washout from tiny multiclass probabilities).
#' Dictionary key `classif.tace`; defaults `ranges = 15`, `threshold = 0.01`
#' (Nixon also discusses `0.001`; the threshold is a user choice).
#' For each class `k`, retain \eqn{S_k(\epsilon) = \{i : p_{ik} > \epsilon\}},
#' split into `R` adaptive ranges \eqn{A_{rk}(\epsilon)}, and compute
#' \deqn{\mathrm{TACE}_\epsilon = \frac{1}{K R} \sum_{k = 1}^{K} \sum_{r = 1}^{R} \left|\mathrm{acc}(A_{rk}(\epsilon), k) - \mathrm{conf}(A_{rk}(\epsilon), k)\right|}
#' The threshold uses strict `>`.
#'
#' @section Edge conventions:
#' Same sorting and remainder rules as [MeasureClassifACE].
#' Every class must retain at least `R` observations above the threshold;
#' otherwise the measure errors with the class, retained count, threshold, and `R`
#' (classes are not dropped silently).
#' TACE changes the evaluated population and can be unstable when few values survive the threshold.
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
#'   learner$predict(task)$score(mlr3::msr("classif.tace"))
#' }
#'
#' @aliases mlr_measures_classif.tace
#' @export
MeasureClassifTACE = R6Class(
  "MeasureClassifTACE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifTACE` object.
    initialize = function() {
      super$initialize(
        id = "classif.tace",
        label = "Thresholded Adaptive Calibration Error",
        man = "mlr_measures_classif.tace",
        packages = character(),
        param_set = ps(
          ranges = p_int(lower = 1L, init = 15L),
          threshold = p_dbl(lower = 0, upper = 1, init = 0.01)
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
      threshold = self$param_set$values$threshold

      if (is.null(ranges)) {
        ranges = 15L
      }

      if (is.null(threshold)) {
        threshold = 0.01
      }

      classwise_adaptive_calibration_error(prediction, ranges, threshold = threshold)
    }
  )
)
