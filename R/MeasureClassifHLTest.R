#' @title Hosmer-Lemeshow test
#'
#' @description
#' Tests the goodness of calibration for a binary classifier by comparing
#' expected and observed frequencies in quantile groups of predicted probability.
#'
#' The constructor argument `type` selects the immutable output mode:
#'
#' - `type = "pvalue"` (default) returns the upper-tail chi-square p-value with
#'   `range = c(0, 1)` and `minimize = NA`.
#' - `type = "statistic"` returns the raw Hosmer-Lemeshow chi-square statistic with
#'   `range = c(0, Inf)` and `minimize = TRUE`.
#' Both modes retain the dictionary ID `"classif.hltest"` and must therefore be scored in separate calls.
#'
#' ## Degrees of freedom for the p-value
#'
#' The HL statistic is compared to a chi-square reference distribution whose degrees of
#' freedom depend on how the scored sample relates to model fitting
#' (Hosmer, Lemeshow, and Sturdivant 2013; Fan et al. 2025):
#'
#' - `df = "validation"` (default): use \(M\) degrees of freedom, where \(M\) is the number of
#'   populated groups.
#'   This is the appropriate reference for **external validation / holdout / test** predictions,
#'   which is the usual mlr3 scoring setting (`Prediction` objects from unseen data).
#' - `df = "fit"`: use \(M - 2\) degrees of freedom.
#'   This is the classical reference when the same sample was used to **estimate** the
#'   probability model (goodness-of-fit on the development data).
#'
#' The choice affects only `type = "pvalue"`; the HL statistic is unchanged.
#' Neither output is a good primary tuning objective: a larger p-value can reflect low power,
#' and the statistic depends on sample size and grouping and is not a proper scoring rule.
#' At least three populated groups are required; ties can reduce the configured number of groups.
#'
#' @param type (`character(1)`)\cr
#'   Output type, fixed at construction.
#'   Either `"pvalue"` (default) or `"statistic"`.
#'
#' @field type (`character(1)`)\cr
#'   Read-only output type selected at construction.
#'
#' @references
#' `r format_bib("hosmer2000")`
#'
#' `r format_bib("hosmer2013")`
#'
#' `r format_bib("fan2025")`
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
#'   prediction$score(mlr3::msr("classif.hltest"))
#'   prediction$score(mlr3::msr("classif.hltest", type = "statistic"))
#'   prediction$score(mlr3::msr("classif.hltest", df = "fit"))
#' }
#'
#' @aliases mlr_measures_classif.hltest
#' @export
MeasureClassifHLTest = R6Class(
  "MeasureClassifHLTest",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new `MeasureClassifHLTest` object.
    initialize = function(type = "pvalue") {
      assert_choice(type, c("pvalue", "statistic"))
      private$.type = type

      measure_range = if (identical(type, "pvalue")) c(0, 1) else c(0, Inf)
      measure_minimize = if (identical(type, "pvalue")) NA else TRUE

      super$initialize(
        id = "classif.hltest",
        label = "Hosmer-Lemeshow Test",
        man = "mlr_measures_classif.hltest",
        packages = character(),
        param_set = ps(
          bins = p_int(lower = 3L, init = 10L),
          df = p_fct(
            levels = c("validation", "fit"),
            init = "validation"
          )
        ),
        properties = character(),
        task_properties = "twoclass",
        predict_type = "prob",
        range = measure_range,
        minimize = measure_minimize
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
      bins = self$param_set$values$bins
      df_mode = self$param_set$values$df

      if (is.null(bins)) {
        bins = 10L
      }
      if (is.null(df_mode)) {
        df_mode = "validation"
      }

      assert_int(bins, lower = 3L)
      assert_choice(df_mode, c("validation", "fit"))

      inputs = binary_prediction_inputs(prediction)
      actual = inputs$actual
      predicted = inputs$predicted
      breaks = unique(quantile(
        predicted,
        probs = seq(0, 1, length.out = bins + 1L),
        names = FALSE
      ))

      if (length(breaks) < 2L) {
        populated_groups = 1L
      } else {
        groups = droplevels(cut(predicted, breaks = breaks, include.lowest = TRUE))
        populated_groups = nlevels(groups)
      }

      if (populated_groups < bins) {
        cli_warn(sprintf(
          "Ties or insufficient distinct predictions reduced Hosmer-Lemeshow groups from %d to %d.",
          bins,
          populated_groups
        ))
      }

      if (populated_groups < 3L) {
        cli_abort("The Hosmer-Lemeshow test requires at least three populated groups.")
      }

      group_count = as.numeric(rowsum(rep(1, length(actual)), groups, reorder = FALSE))
      observed_positive = as.numeric(rowsum(actual, groups, reorder = FALSE))
      expected_positive = as.numeric(rowsum(predicted, groups, reorder = FALSE))
      observed_negative = group_count - observed_positive
      expected_negative = group_count - expected_positive
      contribution = function(observed, expected) {
        ifelse(
          expected == 0,
          ifelse(observed == 0, 0, Inf),
          (observed - expected)^2 / expected
        )
      }
      statistic = sum(
        contribution(observed_positive, expected_positive),
        contribution(observed_negative, expected_negative)
      )

      if (identical(private$.type, "statistic")) {
        return(statistic)
      }

      degrees_of_freedom = if (identical(df_mode, "fit")) {
        populated_groups - 2L
      } else {
        populated_groups
      }

      pchisq(statistic, degrees_of_freedom, lower.tail = FALSE)
    }
  )
)
