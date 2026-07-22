testthat::skip_if_not_installed("rpart")

test_that("parameter namespace utilities work", {
  namespaces = parameter_namespaces(c("Uncalibrated learner", "beta"))
  namespaced = namespace_param_values(list(cp = 0.01, minsplit = 5L), "classif.rpart")
  component = component_param_values(
    values = namespaced,
    param_set = mlr3::lrn("classif.rpart", predict_type = "prob")$param_set,
    namespace = "classif.rpart"
  )

  testthat::expect_identical(namespaces[["Uncalibrated learner"]], "Uncalibrated.learner")
  testthat::expect_identical(names(namespaced), c("classif.rpart.cp", "classif.rpart.minsplit"))
  testthat::expect_identical(component, list(cp = 0.01, minsplit = 5L))
})
