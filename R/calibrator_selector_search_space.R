#' @title Calibrator selector search space
#'
#' @description
#' Build the dependency-aware search space for `clb("selector")`.
#' This helper is intended for outer tuning workflows around `po("calibrate")`,
#' and returns the calibrator-specific part of the search space with the
#' required parameter dependencies already attached.
#'
#' @param choices `character()` or `NULL`
#'   Registered calibrator keys to expose.
#'   Defaults to the selector defaults.
#' @param prefix `character(1)` or `NULL`
#'   Optional namespace prefix for the returned search space.
#'   Use `"calibrate"` for a default `po("calibrate")` inside a `GraphLearner`,
#'   or `NULL` for the unprefixed selector space.
#'
#' @return A [`ParamSetCollection`][ParamSetCollection].
#'
#' @examples
#' learner_space = paradox::ps(
#'   "calibrate.classif.rpart.cp" = paradox::p_dbl(0.001, 0.1)
#' )
#' search_space = paradox::ParamSetCollection$new(c(
#'   list(learner_space),
#'   list(calibrator_selector_search_space(choices = c("platt", "beta")))
#' ))
#'
#' search_space$ids()
#'
#' @export
calibrator_selector_search_space = function(choices = NULL, prefix = "calibrate") {
  assert_string(prefix, min.chars = 1L, null.ok = TRUE)

  selector = clb("selector", choices = choices)
  param_set = selector$param_set$clone(deep = TRUE)
  param_set$values = list()

  ParamSetCollection$new(
    namespaced_param_set_entries(param_set, namespace = prefix)
  )
}
