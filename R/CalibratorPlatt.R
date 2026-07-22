#' @include Calibrator.R
#'
#' @title Platt calibrator
#'
#' @name mlr3_calibrators_platt
#'
#' @description
#' Binary probability calibration with a univariate logistic sigmoid,
#' following Platt (2000) and Niculescu-Mizil and Caruana (2005).
#'
#' By default (`label_smoothing = TRUE`), training targets use Platt's Laplace-smoothed
#' probabilities \(y_+ = (N_+ + 1)/(N_+ + 2)\) and \(y_- = 1/(N_- + 2)\) instead of hard 0/1 labels.
#' This matches the original proposal and scikit-learn's `CalibratedClassifierCV(method = "sigmoid")`.
#'
#' Set `label_smoothing = FALSE` to fit ordinary logistic regression with hard 0/1 labels.
#' That variant matches tidymodels' [probably](https://probably.tidymodels.org/)
#' `cal_estimate_logistic(..., smooth = FALSE)` and the hard-label "logistic calibration" /
#' "logistic recalibration" used in clinical prediction (e.g. Steyerberg-style updating on predicted
#' probabilities).
#'
#' The predictor can be the raw positive-class probability (`input = "probabilities"`, default)
#' or its logit (`input = "logit"`).
#' Silva Filho et al. (2023) recommend a logit transform when the base model already outputs
#' probabilities in \eqn{[0, 1]}.
#' The default keeps the raw probability to match common sklearn / probably practice on
#' `predict_proba` / `.pred_*` columns.
#'
#' Multiclass Platt calibration is obtained with `clb("ovr")`, which wraps this calibrator by default.
#'
#' @param label_smoothing `logical(1)`
#'   If `TRUE` (default), use Platt soft targets \(y_+\), \(y_-\) (Platt 2000; sklearn sigmoid).
#'   If `FALSE`, use hard 0/1 labels as in probably `cal_estimate_logistic(..., smooth = FALSE)`
#'   and hard-label logistic recalibration.
#' @param input `character(1)`
#'   Predictor scale: `"probabilities"` (default) or `"logit"`.
#'   Use `"logit"` to follow Silva Filho et al. (2023) when calibrating probability outputs.
#'
#' @references
#' `r format_bib("platt2000")`
#'
#' `r format_bib("niculescu2005")`
#'
#' `r format_bib("pedregosa2011")`
#'
#' `r format_bib("silvafilho2023")`
CalibratorPlatt = R6Class(
  "CalibratorPlatt",
  inherit = Calibrator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6Class] class.
    initialize = function(label_smoothing = TRUE, input = "probabilities") {
      assert_flag(label_smoothing)
      assert_choice(input, c("probabilities", "logit"))

      param_set = ps(
        label_smoothing = p_lgl(default = TRUE, tags = "train"),
        input = p_fct(levels = c("probabilities", "logit"), default = "probabilities", tags = c("train", "predict"))
      )

      super$initialize(
        id = "platt",
        param_set = param_set,
        properties = "twoclass",
        packages = "stats",
        label = "Platt calibrator"
      )

      self$param_set$values$label_smoothing = label_smoothing
      self$param_set$values$input = input
    }
  ),
  private = list(
    .model = NULL,
    .reset = function() {
      private$.model = NULL
    },
    .train = function(truth, prob, class_names) {
      label_smoothing = self$param_set$values$label_smoothing
      input = self$param_set$values$input

      if (is.null(label_smoothing)) {
        label_smoothing = TRUE
      }
      if (is.null(input)) {
        input = "probabilities"
      }

      y_hard = as.integer(truth == class_names[1L])
      targets = if (isTRUE(label_smoothing)) {
        platt_targets(y_hard)
      } else {
        y_hard
      }

      private$.model = private$.fit_model(
        truth = targets,
        response = platt_predictor(prob[, 1L], input = input)
      )
    },
    .predict = function(prob, class_names) {
      input = self$param_set$values$input
      if (is.null(input)) {
        input = "probabilities"
      }
      prob_pos = predict(
        private$.model,
        newdata = data.frame(response = platt_predictor(prob[, 1L], input = input)),
        type = "response"
      )
      binary_probability_matrix(prob_pos = prob_pos, class_names = class_names)
    },
    .fit_model = function(truth, response) {
      # Fractional Platt targets trigger glm's non-integer successes warning; the binomial
      # log-likelihood with soft labels in (0, 1) is still the intended objective.
      suppressWarnings(
        glm(
          truth ~ response,
          data = data.frame(truth = truth, response = response),
          family = binomial()
        )
      )
    }
  )
)

#' @title Platt soft targets
#' @description
#' Laplace-smoothed binary targets from Platt (2000).
#' @param y_hard Integer or logical vector of hard labels (1 = positive).
#' @return Numeric vector of soft targets in (0, 1).
#' @keywords internal
#' @noRd
platt_targets = function(y_hard) {
  assert_integerish(y_hard, lower = 0L, upper = 1L, any.missing = FALSE, min.len = 1L)
  y_hard = as.integer(y_hard)
  n_pos = sum(y_hard == 1L)
  n_neg = sum(y_hard == 0L)
  y_pos = (n_pos + 1) / (n_pos + 2)
  y_neg = 1 / (n_neg + 2)
  ifelse(y_hard == 1L, y_pos, y_neg)
}

#' @title Platt predictor transform
#' @param prob Numeric probability vector.
#' @param input `"probabilities"` or `"logit"`.
#' @param epsilon Numeric clipping for logits.
#' @return Numeric predictor vector.
#' @keywords internal
#' @noRd
platt_predictor = function(prob, input = "probabilities", epsilon = 1e-7) {
  assert_numeric(prob, any.missing = FALSE, finite = TRUE)
  assert_choice(input, c("probabilities", "logit"))
  assert_number(epsilon, lower = 0, upper = 0.5)

  if (identical(input, "probabilities")) {
    return(prob)
  }

  clipped = pmin(pmax(prob, epsilon), 1 - epsilon)
  log(clipped / (1 - clipped))
}
