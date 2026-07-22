#' @title Static calibration error (classwise ECE)
#'
#' @description
#' Static calibration error (SCE) of Nixon et al. (2019), equal to the classwise ECE of
#' Kull et al. (2019), Eq. 4.
#' Dictionary key `classif.sce`; default `bins = 15`.
#' Every class probability is scored on all `N` one-vs-rest observations (whether or not that class is predicted):
#' \deqn{\mathrm{SCE} = \frac{1}{K} \sum_{k = 1}^{K} \sum_{b = 1}^{B} \frac{n_{bk}}{N}\,\left|\mathrm{acc}(B_{bk}, k) - \mathrm{conf}(B_{bk}, k)\right|}
#' with \eqn{n_{bk} = |B_{bk}|} and \eqn{\mathrm{acc}} / \eqn{\mathrm{conf}} the observed frequency and mean
#' predicted probability for class `k` in bin `b`.
#'
#' Fixed-width bins use right-closed intervals \eqn{((b - 1)/B, b/B]}; probability zero goes in the first bin;
#' empty bins have zero weight and are skipped.
#' The outer \eqn{1/K} is a macro average (not prevalence-weighted); rare classes can be noisy.
#' Tiny non-target probabilities can make the scalar look small without implying full multiclass calibration.
#'
#' @references
#' `r format_bib("nixon2019")`
#'
#' `r format_bib("kull2019")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   task = mlr3::as_task_classif(iris, target = "Species")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.sce"))
#' }
#'
#' @aliases mlr_measures_classif.sce
#' @export
MeasureClassifSCE = R6Class(
  "MeasureClassifSCE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifSCE` object.
    initialize = function() {
      super$initialize(
        id = "classif.sce",
        label = "Static Calibration Error",
        man = "mlr_measures_classif.sce",
        packages = character(),
        param_set = ps(
          bins = p_int(lower = 1L, init = 15L)
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
      bins = self$param_set$values$bins

      if (is.null(bins)) {
        bins = 15L
      }

      prob = prediction_probability_matrix(prediction)
      truth = as.character(prediction$truth)

      per_class = vapply(
        colnames(prob),
        function(k) {
          equal_width_calibration_error(as.integer(truth == k), prob[, k], bins)
        },
        numeric(1L)
      )

      mean(per_class)
    }
  )
)
