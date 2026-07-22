skip_if_binary_backend_unavailable = function() {
  testthat::skip_if_not_installed("mlbench")
  testthat::skip_if_not_installed("mlr3learners")
  testthat::skip_if_not_installed("rpart")
}

skip_if_rpart_backend_unavailable = function() {
  testthat::skip_if_not_installed("mlr3learners")
  testthat::skip_if_not_installed("rpart")
}

make_binary_task = function() {
  skip_if_binary_backend_unavailable()
  data("Sonar", package = "mlbench", envir = environment())
  mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
}

make_binary_split = function() {
  set.seed(1)
  task = make_binary_task()
  train_ids = sample(task$row_ids, floor(0.7 * task$nrow))

  list(
    train = task$clone()$filter(train_ids),
    test = task$clone()$filter(setdiff(task$row_ids, train_ids))
  )
}

make_binary_prediction_data = function() {
  skip_if_rpart_backend_unavailable()
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(tasks$train)
  prediction = learner$predict(tasks$test)
  class_names = task_class_names(tasks$test)

  list(
    task = tasks$test,
    prediction = prediction,
    data = prediction_calibration_data(prediction, class_names = class_names),
    class_names = class_names
  )
}

make_multiclass_prediction_data = function() {
  skip_if_rpart_backend_unavailable()
  set.seed(1)
  task = mlr3::as_task_classif(iris, target = "Species")
  train_ids = sample(task$row_ids, floor(0.7 * task$nrow))
  task_train = task$clone()$filter(train_ids)
  task_test = task$clone()$filter(setdiff(task$row_ids, train_ids))
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(task_train)
  prediction = learner$predict(task_test)

  list(
    train = task_train,
    test = task_test,
    prediction = prediction,
    data = prediction_calibration_data(prediction, class_names = task_test$class_names),
    class_names = task_test$class_names
  )
}

make_autotuner = function(learner, evals = 4L) {
  testthat::skip_if_not_installed("bbotk")
  testthat::skip_if_not_installed("mlr3tuning")

  mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = mlr3::rsmp("cv", folds = 2),
    measure = mlr3::msr("classif.bbrier"),
    search_space = paradox::ps(cp = paradox::p_dbl(0.001, 0.1)),
    terminator = bbotk::trm("evals", n_evals = evals),
    tuner = mlr3tuning::tnr("grid_search", resolution = 2)
  )
}

expect_calibrated_prediction = function(learner, task_train, task_test, suppress_training = FALSE) {
  if (suppress_training) {
    suppressWarnings(learner$train(task_train))
  } else {
    learner$train(task_train)
  }

  prediction = learner$predict(task_test)
  testthat::expect_s3_class(prediction, "PredictionClassif")
  testthat::expect_true(all(abs(rowSums(prediction$prob) - 1) < 1e-8))
  prediction
}

make_manual_binary_prediction = function(predicted, actual, class_names = c("yes", "no")) {
  prob = cbind(predicted, 1 - predicted)
  colnames(prob) = class_names
  truth = factor(ifelse(actual == 1, class_names[[1L]], class_names[[2L]]), levels = class_names)

  mlr3::PredictionClassif$new(
    row_ids = seq_along(predicted),
    truth = truth,
    prob = prob
  )
}

# Score a prediction with a single measure and drop the measure-id name for plain numeric comparisons.
score1 = function(prediction, key, ...) {
  unname(prediction$score(mlr3::msr(key, ...)))
}

make_manual_prediction = function(prob, truth, row_ids = seq_len(nrow(prob))) {
  truth = factor(as.character(truth), levels = colnames(prob))

  mlr3::PredictionClassif$new(
    row_ids = row_ids,
    truth = truth,
    prob = prob
  )
}

# Independent right-closed equal-width bin assignment used only by tests.
# ceiling(p * bins) reproduces intervals ((b - 1) / bins, b / bins]; an exact zero maps to bin 1.
ref_equal_width_bin = function(predicted, bins) {
  bin = ceiling(predicted * bins)
  bin[bin < 1L] = 1L
  as.integer(bin)
}

# Independent single-class equal-width ECE reference: sum_b (n_b / N) * |acc_b - conf_b|.
ref_equal_width_ece = function(actual, predicted, bins) {
  bin = ref_equal_width_bin(predicted, bins)
  n = length(predicted)
  total = 0
  for (b in unique(bin)) {
    selected = bin == b
    total = total + sum(selected) / n * abs(mean(actual[selected]) - mean(predicted[selected]))
  }
  total
}

# Independent classwise ECE (SCE) reference: (1 / K) * sum_k sum_b (n_bk / N) * |acc_bk - conf_bk|.
ref_classwise_ece = function(prob, truth, bins) {
  classes = colnames(prob)
  per_class = vapply(
    classes,
    function(k) ref_equal_width_ece(as.integer(as.character(truth) == k), prob[, k], bins),
    numeric(1L)
  )
  mean(per_class)
}

# Independent adaptive equal-count range gaps reference for one class.
ref_adaptive_gaps = function(actual, predicted, row_ids, ranges) {
  index = order(predicted, row_ids)
  actual = actual[index]
  predicted = predicted[index]
  n = length(predicted)
  rest = n %% ranges
  sizes = c(rep(ceiling(n / ranges), rest), rep(floor(n / ranges), ranges - rest))
  range_id = rep(seq_len(ranges), sizes)
  gaps = vapply(
    seq_len(ranges),
    function(r) abs(mean(actual[range_id == r]) - mean(predicted[range_id == r])),
    numeric(1L)
  )
  mean(gaps)
}

# Independent classwise ACE reference over adaptive equal-count ranges.
ref_ace = function(prob, truth, row_ids, ranges) {
  classes = colnames(prob)
  per_class = vapply(
    classes,
    function(k) ref_adaptive_gaps(as.integer(as.character(truth) == k), prob[, k], row_ids, ranges),
    numeric(1L)
  )
  mean(per_class)
}
