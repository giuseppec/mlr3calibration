test_that("probability helpers normalize and order matrices", {
  normalized_vector = normalize_probability_matrix(
    prob = c(0.2, 0.8),
    class_names = c("M", "R")
  )
  normalized_matrix = normalize_probability_matrix(
    prob = matrix(c(0.2, 0.8, 0.3, 0.3), ncol = 2, byrow = TRUE),
    class_names = c("M", "R")
  )

  testthat::expect_equal(colnames(normalized_vector), c("M", "R"))
  testthat::expect_true(all(abs(rowSums(normalized_vector) - 1) < 1e-8))
  testthat::expect_true(all(abs(rowSums(normalized_matrix) - 1) < 1e-8))
  testthat::expect_equal(unname(normalized_matrix[2L, ]), c(0.5, 0.5))
})

test_that("probability helpers reject invalid values instead of laundering them", {
  invalid_non_finite = list(
    matrix(c(NA_real_, 1), nrow = 1L),
    matrix(c(NaN, 1), nrow = 1L),
    matrix(c(Inf, 1), nrow = 1L)
  )

  for (prob in invalid_non_finite) {
    colnames(prob) = c("M", "R")
    testthat::expect_error(normalize_probability_matrix(prob), "finite")
  }

  for (prob in list(matrix(c(-0.1, 1.1), nrow = 1L), matrix(c(1.1, 0), nrow = 1L))) {
    colnames(prob) = c("M", "R")
    testthat::expect_error(normalize_probability_matrix(prob), "between 0 and 1")
  }

  zero_row = matrix(c(0, 0), nrow = 1L, dimnames = list(NULL, c("M", "R")))
  testthat::expect_error(normalize_probability_matrix(zero_row), "positive row sums")
})

test_that("probability helpers reject extra named columns", {
  prob = matrix(
    c(0.2, 0.7, 0.1, 0.8, 0.1, 0.1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("yes", "no", "maybe"))
  )

  testthat::expect_error(
    normalize_probability_matrix(prob, class_names = c("yes", "no")),
    "not present in the provided class names"
  )
})

test_that("probability helpers tolerate only floating-point boundary drift", {
  prob = matrix(c(-5e-13, 1 + 5e-13), nrow = 1L, dimnames = list(NULL, c("M", "R")))
  normalized = normalize_probability_matrix(prob)

  testthat::expect_equal(unname(normalized), matrix(c(0, 1), nrow = 1L), tolerance = 0)
})

test_that("binary scoring helpers return stable inputs and scores", {
  data = make_binary_prediction_data()
  inputs = binary_prediction_inputs(data$prediction)
  ece = equal_width_calibration_error(actual = inputs$actual, predicted = inputs$predicted, bins = 10L)
  ici = integrated_calibration_index(actual = inputs$actual, predicted = inputs$predicted)
  model = cox_logit_model(actual = inputs$actual, predicted = inputs$predicted)

  testthat::expect_identical(inputs$positive, data$class_names[[1L]])
  testthat::expect_identical(colnames(data$prediction$prob)[1L], data$task$positive)
  checkmate::expect_number(ece, lower = 0)
  checkmate::expect_number(suppressWarnings(ici), lower = 0)
  testthat::expect_s3_class(model, "glm")
})

test_that("binary_prediction_inputs follows a flipped task positive class", {
  skip_if_binary_backend_unavailable()
  skip_if_rpart_backend_unavailable()
  set.seed(1)
  data("Sonar", package = "mlbench", envir = environment())
  task = mlr3::as_task_classif(Sonar, target = "Class", positive = "R")
  train_ids = sample(task$row_ids, floor(0.7 * task$nrow))
  task_train = task$clone()$filter(train_ids)
  task_test = task$clone()$filter(setdiff(task$row_ids, train_ids))
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(task_train)
  prediction = learner$predict(task_test)
  inputs = binary_prediction_inputs(prediction)

  testthat::expect_identical(task$positive, "R")
  testthat::expect_identical(colnames(prediction$prob)[1L], "R")
  testthat::expect_identical(inputs$positive, "R")
  testthat::expect_equal(unname(inputs$predicted), unname(prediction$prob[, "R"]))
})
