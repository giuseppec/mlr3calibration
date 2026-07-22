testthat::skip_if_not_installed("rpart")

test_that("calibration_plot returns a ggplot", {
  tasks = make_binary_split()

  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(tasks$train)

  plot = calibration_plot(list(learner), tasks$test, smooth = TRUE, ci = TRUE, rug = TRUE)

  testthat::expect_s3_class(plot, "ggplot")
})

test_that("calibration_plot accepts named predictions", {
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(tasks$train)
  prediction = learner$predict(tasks$test)

  plot = calibration_plot(predictions = list(rpart = prediction), smooth = TRUE)

  testthat::expect_s3_class(plot, "ggplot")
})

test_that("calibration_plot rejects multiclass tasks", {
  task = mlr3::as_task_classif(iris, target = "Species")
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(task)

  testthat::expect_error(
    calibration_plot(list(learner), task),
    "binary classification tasks only"
  )
})

test_that("calibration_plot requires named prediction inputs", {
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(tasks$train)
  prediction = learner$predict(tasks$test)

  testthat::expect_error(
    calibration_plot(predictions = list(prediction)),
    "Must have names|named list"
  )
})

test_that("calibration_plot rejects response-only predictions clearly", {
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "response")
  learner$train(tasks$train)
  prediction = learner$predict(tasks$test)

  testthat::expect_error(
    calibration_plot(predictions = list(rpart = prediction)),
    "binary classification predictions only"
  )
  testthat::expect_error(
    calibration_plot(learners = list(learner), task = tasks$test),
    "requires probability predictions"
  )
})
