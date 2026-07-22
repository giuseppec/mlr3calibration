#' @include Calibrator.R
#'
#' @title Beta calibrator
#'
#' @name mlr3_calibrators_beta
#'
#' @description
#' Beta calibration (Kull et al. 2017) for binary probabilities.
#' Fits the requested parameterization with `stats::glm` on transformed features.
#' Coefficients use the canonical beta-map signs for the `log(p)` and `-log(1 - p)` terms.
#'
#' @param parameters `character(1)`
#'   Parameterization for beta calibration.
#'   `"abm"` (default) is the full three-parameter family; `"ab"` fixes the midpoint at 0.5.
#'
#' @references
#' `r format_bib("kull2017")`
#'
#' `r format_bib("silvafilho2017")`
CalibratorBeta = R6Class(
  "CalibratorBeta",
  inherit = Calibrator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6Class] class.
    initialize = function(parameters = "abm") {
      assert_string(parameters, min.chars = 1L)

      param_set = ps(
        parameters = p_fct(
          levels = c("abm", "ab"),
          default = "abm",
          tags = "train"
        )
      )

      super$initialize(
        id = "beta",
        param_set = param_set,
        properties = "twoclass",
        packages = "stats",
        label = "Beta calibrator"
      )

      private$.calibrator = NULL
      self$param_set$values$parameters = parameters
    }
  ),
  private = list(
    .calibrator = NULL,
    .reset = function() {
      private$.calibrator = NULL
    },
    .train = function(truth, prob, class_names) {
      parameters = self$param_set$values$parameters

      if (is.null(parameters)) {
        parameters = "abm"
      }

      private$.calibrator = fit_beta_calibration(
        p = prob[, 1L],
        y = as.integer(truth == class_names[1L]),
        parameters = parameters
      )
    },
    .predict = function(prob, class_names) {
      prob_pos = predict_beta_calibration(prob[, 1L], private$.calibrator)
      binary_probability_matrix(prob_pos = prob_pos, class_names = class_names)
    }
  )
)

# Beta calibration (Kull et al. 2017) via logistic regression on transformed features.
# Both parameterizations use -log(1 - p), so the fitted second coefficient has the canonical beta-map sign.
fit_beta_calibration = function(p, y, parameters) {
  assert_choice(parameters, c("abm", "ab"))
  assert_numeric(p, any.missing = FALSE, finite = TRUE)
  assert_integerish(y, lower = 0L, upper = 1L, any.missing = FALSE, len = length(p))

  p = pmax(1e-16, pmin(as.numeric(p), 1 - 1e-16))
  y = as.numeric(y)

  if (identical(parameters, "abm")) {
    training_data = data.frame(y = y, lp = log(p), l1p = -log(1 - p))
    model = fit_constrained_beta_glm(training_data, intercept = TRUE)
  } else {
    training_data = data.frame(y = y, lp = log(2 * p), l1p = -log(2 * (1 - p)))
    model = fit_constrained_beta_glm(training_data, intercept = FALSE)
  }

  list(model = model, parameters = parameters)
}

fit_constrained_beta_glm = function(training_data, intercept) {
  assert_data_frame(training_data, types = c("numeric", "numeric", "numeric"), ncols = 3L, min.rows = 1L)
  assert_names(names(training_data), identical.to = c("y", "lp", "l1p"))
  assert_flag(intercept)

  active = c("lp", "l1p")

  repeat {
    formula = if (length(active) == 2L) {
      if (intercept) y ~ lp + l1p else y ~ lp + l1p - 1
    } else if (length(active) == 1L && identical(active, "lp")) {
      if (intercept) y ~ lp else y ~ lp - 1
    } else if (length(active) == 1L) {
      if (intercept) y ~ l1p else y ~ l1p - 1
    } else if (intercept) {
      y ~ 1
    } else {
      y ~ -1
    }
    model = glm(formula, family = binomial(), data = training_data)

    if (length(active) == 0L) {
      return(model)
    }

    shape = coef(model)[active]
    invalid = is.na(shape) | shape < 0

    if (!any(invalid)) {
      return(model)
    }

    # Eliminate one coefficient at a time and refit, as in betacal: the remaining
    # coefficient can turn valid once the other one is constrained to zero.
    active = setdiff(active, active[invalid][1L])

    if (length(active) == 0L) {
      cli_warn("Beta calibration eliminated all shape coefficients; the fitted map is constant.")
    }
  }
}

predict_beta_calibration = function(p, calib) {
  assert_list(calib, any.missing = FALSE)
  assert_names(names(calib), permutation.of = c("model", "parameters"))
  assert_class(calib$model, "glm")
  assert_choice(calib$parameters, c("abm", "ab"))
  assert_numeric(p, any.missing = FALSE, finite = TRUE)

  p = pmax(1e-16, pmin(as.numeric(p), 1 - 1e-16))
  newdata = if (identical(calib$parameters, "abm")) {
    data.frame(lp = log(p), l1p = -log(1 - p))
  } else {
    data.frame(lp = log(2 * p), l1p = -log(2 * (1 - p)))
  }

  as.numeric(predict(calib$model, newdata = newdata, type = "response"))
}
