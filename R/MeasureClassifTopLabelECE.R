#' @title Top-label expected calibration error
#'
#' @description
#' Top-label ECE (TL-ECE) of Gupta and Ramdas (2022), Eqs. 2 and 4.
#' Dictionary key `classif.tl_ece`; default `bins = 15`.
#' Conditions on both the predicted label and its confidence
#' (stricter than [MeasureClassifConfidenceECE], which pools labels).
#' With \eqn{c_i = \arg\max_k p_{ik}}, \eqn{h_i = \max_k p_{ik}}, \eqn{z_i = 1(y_i = c_i)},
#' and \eqn{T_{bk} = \{i : c_i = k \mathrm{\ and\ } h_i \in I_b\}}:
#' \deqn{\mathrm{TL{-}ECE} = \sum_{k = 1}^{K} \sum_{b = 1}^{B} \frac{|T_{bk}|}{N}\,\left|\mathrm{mean}(z_i : i \in T_{bk}) - \mathrm{mean}(h_i : i \in T_{bk})\right|}
#'
#' Empty `(class, bin)` cells are skipped.
#' Classes are weighted by how often they are predicted; never-predicted classes contribute zero.
#' Fixed-width bins use right-closed intervals \eqn{((b - 1)/B, b/B]}; confidence zero goes in the first bin;
#' top-probability ties break to the first class in column order.
#' Older literature sometimes calls confidence-ECE "top-label ECE"; this package uses the Gupta and Ramdas definition.
#' Compare also [MeasureClassifSCE], which scores every class probability whether or not that class is predicted.
#'
#' @references
#' `r format_bib("gupta2022")`
#'
#' `r format_bib("guo2017")`
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   task = mlr3::as_task_classif(iris, target = "Species")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   learner$predict(task)$score(mlr3::msr("classif.tl_ece"))
#' }
#'
#' @aliases mlr_measures_classif.tl_ece
#' @export
MeasureClassifTopLabelECE = R6Class(
  "MeasureClassifTopLabelECE",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifTopLabelECE` object.
    initialize = function() {
      super$initialize(
        id = "classif.tl_ece",
        label = "Top-Label Expected Calibration Error",
        man = "mlr_measures_classif.tl_ece",
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

      contributions = vapply(
        colnames(top$prob),
        function(k) {
          selected = top$label == k

          if (!any(selected)) {
            return(0)
          }

          mean(selected) * equal_width_calibration_error(
            correct[selected],
            top$confidence[selected],
            bins
          )
        },
        numeric(1L)
      )

      sum(contributions)
    }
  )
)
