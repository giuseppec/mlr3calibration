#' @include Calibrator.R
#'
#' @title Isotonic calibrator
#'
#' @name mlr3_calibrators_isotonic
#'
#' @description
#' Calibration via isotonic regression.
#' Fitted values are linearly interpolated between observed score values and extended as constants
#' beyond the training range.
#'
#' @references
#' `r format_bib("zadrozny2002")`
#'
#' `r format_bib("niculescu2005")`
CalibratorIsotonic = R6Class(
  "CalibratorIsotonic",
  inherit = Calibrator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6Class] class.
    initialize = function() {
      super$initialize(
        id = "isotonic",
        param_set = ps(),
        properties = "twoclass",
        packages = "stats",
        label = "Isotonic calibrator"
      )

      private$.calibrator = NULL
    }
  ),
  private = list(
    .calibrator = NULL,
    .reset = function() {
      private$.calibrator = NULL
    },
    .train = function(truth, prob, class_names) {
      truth = as.integer(truth == class_names[1L])
      fit = isoreg(x = prob[, 1L], y = truth)
      x = if (fit$isOrd) fit$x else fit$x[fit$ord]
      unique_x = unique(x)
      score_group = match(x, unique_x)
      fitted = as.numeric(
        rowsum(fit$yf, group = score_group, reorder = FALSE) /
          tabulate(score_group, nbins = length(unique_x))
      )
      private$.calibrator = list(x = unique_x, fitted = fitted)
    },
    .predict = function(prob, class_names) {
      prob_pos = if (length(private$.calibrator$x) == 1L) {
        rep(private$.calibrator$fitted, nrow(prob))
      } else {
        approx(
          x = private$.calibrator$x,
          y = private$.calibrator$fitted,
          xout = prob[, 1L],
          method = "linear",
          rule = 2,
          ties = "ordered"
        )$y
      }
      binary_probability_matrix(prob_pos = prob_pos, class_names = class_names)
    }
  )
)
