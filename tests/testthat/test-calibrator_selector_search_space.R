test_that("calibrator_selector_search_space returns namespaced selector params", {
  search_space = calibrator_selector_search_space(choices = c("platt", "beta"))

  testthat::expect_true(inherits(search_space, "ParamSetCollection"))
  testthat::expect_true(all(c(
    "calibrate.calibrator",
    "calibrate.beta.parameters"
  ) %in% search_space$ids()))
})
