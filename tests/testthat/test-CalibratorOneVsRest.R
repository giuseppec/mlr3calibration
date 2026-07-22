test_that("clb('ovr') constructs with required metadata", {
  calibrator = clb("ovr")

  testthat::expect_true(inherits(calibrator, "CalibratorOneVsRest"))
  testthat::expect_identical(calibrator$id, "ovr")
  testthat::expect_identical(calibrator$label, "One-vs-rest calibrator")
  testthat::expect_setequal(calibrator$properties, c("twoclass", "multiclass"))
  testthat::expect_true(all(
    c("platt", "beta", "isotonic", "ovr", "selector") %in% mlr3_calibrators$keys()
  ))
})

test_that("ovr-platt with hard labels matches former logistic calibration branch", {
  prob = matrix(
    c(
      0.70, 0.20, 0.10,
      0.10, 0.60, 0.30,
      0.25, 0.25, 0.50,
      0.40, 0.40, 0.20,
      0.15, 0.55, 0.30,
      0.55, 0.15, 0.30
    ),
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  truth = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c"))
  expected = matrix(
    c(
      1, 0, 0,
      0.029067639247, 0.728199270564, 0.242733090188,
      0.109485095875, 0, 0.890514904125,
      0.999999972833, 3.97e-10, 2.677e-08,
      0.042158577453, 0.718381066894, 0.239460355654,
      0.648293013156, 0, 0.351706986844
    ),
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(as.character(1:6), c("a", "b", "c"))
  )

  calibrator = clb("ovr", calibrator = clb("platt", label_smoothing = FALSE))
  suppressWarnings(calibrator$train(truth, prob, c("a", "b", "c")))
  out = calibrator$predict(prob, c("a", "b", "c"))

  testthat::expect_equal(unname(out), unname(expected), tolerance = 1e-10)
  testthat::expect_identical(colnames(out), c("a", "b", "c"))
})

test_that("default ovr-platt uses soft Platt targets", {
  prob = matrix(
    c(
      0.70, 0.20, 0.10,
      0.10, 0.60, 0.30,
      0.25, 0.25, 0.50,
      0.40, 0.40, 0.20,
      0.15, 0.55, 0.30,
      0.55, 0.15, 0.30
    ),
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  truth = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c"))
  hard_expected = matrix(
    c(
      1, 0, 0,
      0.029067639247, 0.728199270564, 0.242733090188,
      0.109485095875, 0, 0.890514904125,
      0.999999972833, 3.97e-10, 2.677e-08,
      0.042158577453, 0.718381066894, 0.239460355654,
      0.648293013156, 0, 0.351706986844
    ),
    ncol = 3L,
    byrow = TRUE
  )

  calibrator = clb("ovr")
  suppressWarnings(calibrator$train(truth, prob, c("a", "b", "c")))
  out = calibrator$predict(prob, c("a", "b", "c"))

  testthat::expect_false(isTRUE(all.equal(unname(out), unname(hard_expected), tolerance = 1e-8)))
  testthat::expect_true(all(abs(rowSums(out) - 1) < 1e-8))
})

test_that("binary ovr delegates to one wrapped fit", {
  prob = cbind(yes = c(0.1, 0.3, 0.7, 0.9), no = c(0.9, 0.7, 0.3, 0.1))
  truth = factor(c("no", "no", "yes", "yes"), levels = c("yes", "no"))
  direct = CalibratorPlatt$new()
  wrapped = clb("ovr", calibrator = clb("platt"))

  direct$train(truth, prob, c("yes", "no"))
  wrapped$train(truth, prob, c("yes", "no"))

  testthat::expect_equal(
    wrapped$predict(prob, c("yes", "no")),
    direct$predict(prob, c("yes", "no")),
    tolerance = 1e-12
  )
})

test_that("three-class isotonic ovr returns valid probabilities", {
  data = make_multiclass_prediction_data()
  calibrator = clb("ovr", calibrator = clb("isotonic"))
  calibrator$train(data$data$truth, data$data$prob, data$class_names)
  out = calibrator$predict(data$data$prob, data$class_names)

  checkmate::expect_matrix(out, nrows = nrow(data$data$prob), ncols = 3L)
  testthat::expect_identical(colnames(out), data$class_names)
  testthat::expect_true(all(is.finite(out)))
  testthat::expect_true(all(out >= 0 & out <= 1))
  testthat::expect_true(all(abs(rowSums(out) - 1) < 1e-8))
})

test_that("multiclass ovr predicts a single observation", {
  data = make_multiclass_prediction_data()
  calibrator = clb("ovr", calibrator = clb("isotonic"))
  calibrator$train(data$data$truth, data$data$prob, data$class_names)

  out = calibrator$predict(data$data$prob[1L, , drop = FALSE], data$class_names)

  checkmate::expect_matrix(out, nrows = 1L, ncols = 3L)
  testthat::expect_identical(colnames(out), data$class_names)
  testthat::expect_equal(rowSums(out), 1, tolerance = 1e-12)
})

test_that("ovr restores column order and handles a real .rest class name", {
  class_names = c("a", ".rest", "c")
  set.seed(2)
  prob = matrix(runif(18L), nrow = 6L)
  prob = prob / rowSums(prob)
  colnames(prob) = class_names
  truth = factor(rep(class_names, each = 2L), levels = class_names)
  calibrator = clb("ovr", calibrator = clb("isotonic"))
  calibrator$train(truth, prob, class_names)

  reversed = rev(class_names)
  out = calibrator$predict(prob[, reversed, drop = FALSE], reversed)
  testthat::expect_identical(colnames(out), reversed)
  testthat::expect_true(all(abs(rowSums(out) - 1) < 1e-8))
})

test_that("ovr propagates nested beta parameters", {
  calibrator = clb("ovr", calibrator = clb("beta", parameters = "ab"))
  testthat::expect_identical(calibrator$param_set$values$beta.parameters, "ab")

  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    calibrator = calibrator
  )
  testthat::expect_true("ovr.beta.parameters" %in% pipeop$param_set$ids())
})

test_that("ovr reset, stale config, and deep clone behave correctly", {
  data = make_binary_prediction_data()
  calibrator = clb("ovr", calibrator = clb("beta", parameters = "ab"))
  suppressWarnings(calibrator$train(data$data$truth, data$data$prob, data$class_names))
  config_hash = calibrator$hash

  cloned = calibrator$clone(deep = TRUE)
  testthat::expect_false(identical(
    calibrator$.__enclos_env__$private$.prototype,
    cloned$.__enclos_env__$private$.prototype
  ))
  calibrator$reset()
  testthat::expect_false(calibrator$is_trained)
  testthat::expect_true(cloned$is_trained)
  testthat::expect_identical(calibrator$hash, config_hash)

  suppressWarnings(calibrator$train(data$data$truth, data$data$prob, data$class_names))
  calibrator$param_set$values$beta.parameters = "abm"
  testthat::expect_error(calibrator$predict(data$data$prob, data$class_names), "configuration changed")
})

test_that("ovr rejects nested wrappers and unsupported calibrators", {
  testthat::expect_error(clb("ovr", calibrator = clb("ovr")), "cannot wrap another")

  Unsupported = R6::R6Class(
    "Unsupported",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "unsupported_binary", properties = character())
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) prob
    )
  )
  register_calibrator("unsupported_binary", Unsupported)
  on.exit(mlr3_calibrators$remove("unsupported_binary"), add = TRUE)
  testthat::expect_error(
    CalibratorOneVsRest$new(clb("unsupported_binary")),
    "does not declare binary"
  )
})

test_that("partial ovr training failure leaves the wrapper untrained", {
  FailOnB = R6::R6Class(
    "FailOnB",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "fail_on_b", properties = "twoclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        if (identical(class_names[[1L]], "b")) {
          stop("second class failed")
        }
      },
      .predict = function(prob, class_names) prob[, 1L]
    )
  )
  register_calibrator("fail_on_b", FailOnB)
  on.exit(mlr3_calibrators$remove("fail_on_b"), add = TRUE)

  class_names = c("a", "b", "c")
  prob = matrix(c(0.5, 0.3, 0.2), nrow = 3L, ncol = 3L, byrow = TRUE, dimnames = list(NULL, class_names))
  truth = factor(class_names, levels = class_names)
  calibrator = clb("ovr", calibrator = clb("fail_on_b"))

  testthat::expect_error(calibrator$train(truth, prob, class_names), "second class failed")
  testthat::expect_false(calibrator$is_trained)
  testthat::expect_null(calibrator$.__enclos_env__$private$.calibrators)
})
