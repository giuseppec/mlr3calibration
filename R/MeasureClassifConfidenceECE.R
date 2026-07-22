#' @title Confidence expected calibration error
#'
#' @description
#' Confidence ECE of Guo et al. (2017), Eq. 3.
#' Dictionary key `classif.conf_ece`; default `bins = 15`.
#' Scores the maximum predicted probability against whether the predicted label is correct
#' (labels pooled; unlike [MeasureClassifTopLabelECE], which also conditions on the predicted label).
#' With \eqn{\hat{c}_i = \arg\max_k p_{ik}}, \eqn{h_i = \max_k p_{ik}}, \eqn{z_i = 1(y_i = \hat{c}_i)}:
#' \deqn{\mathrm{conf{-}ECE} = \sum_{b = 1}^{B} \frac{|B_b|}{N}\,\left|\mathrm{mean}(z_i : i \in B_b) - \mathrm{mean}(h_i : i \in B_b)\right|}
#'
#' Fixed-width bins use right-closed intervals \eqn{((b - 1)/B, b/B]}; confidence zero goes in the first bin;
#' empty bins have zero weight.
#' Top-probability ties break to the first class in column order.
#'
#' @references
#' `r format_bib("guo2017")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   task = mlr3::as_task_classif(iris, target = "Species")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.conf_ece"))
#' }
#'
#' @aliases mlr_measures_classif.conf_ece
#' @export
MeasureClassifConfidenceECE = R6Class(
  "MeasureClassifConfidenceECE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifConfidenceECE` object.
    initialize = function() {
      super$initialize(
        id = "classif.conf_ece",
        label = "Confidence Expected Calibration Error",
        man = "mlr_measures_classif.conf_ece",
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

      top = prediction_top_label(prediction)
      correct = as.integer(top$truth == top$label)
      equal_width_calibration_error(actual = correct, predicted = top$confidence, bins = bins)
    }
  )
)
