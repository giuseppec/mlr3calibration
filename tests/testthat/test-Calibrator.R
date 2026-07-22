test_that("CalibratorPlatt calibrates binary probability matrices", {
  data = make_binary_prediction_data()
  calibrator = CalibratorPlatt$new()

  calibrator$train(
    truth = data$data$truth,
    prob = data$data$prob,
    class_names = data$class_names
  )
  prob_calibrated = calibrator$predict(prob = data$data$prob, class_names = data$class_names)

  checkmate::expect_matrix(prob_calibrated, ncols = 2L)
  testthat::expect_true(all(abs(rowSums(prob_calibrated) - 1) < 1e-8))
})

test_that("CalibratorIsotonic calibrates binary probability matrices", {
  data = make_binary_prediction_data()
  calibrator = CalibratorIsotonic$new()

  calibrator$train(
    truth = data$data$truth,
    prob = data$data$prob,
    class_names = data$class_names
  )
  prob_calibrated = calibrator$predict(prob = data$data$prob, class_names = data$class_names)

  checkmate::expect_matrix(prob_calibrated, ncols = 2L)
  testthat::expect_true(all(abs(rowSums(prob_calibrated) - 1) < 1e-8))
})

test_that("CalibratorIsotonic interpolates between fitted score values", {
  class_names = c("yes", "no")
  predicted = c(0.01, 0.5, 0.7, 0.9)
  prob = cbind(yes = predicted, no = 1 - predicted)
  truth = factor(c("no", "yes", "yes", "yes"), levels = class_names)
  calibrator = CalibratorIsotonic$new()
  calibrator$train(truth, prob, class_names)

  new_prob = cbind(yes = 0.255, no = 0.745)
  calibrated = calibrator$predict(new_prob, class_names)

  testthat::expect_equal(unname(calibrated[1L, "yes"]), 0.5, tolerance = 1e-12)
})

test_that("CalibratorIsotonic handles unsorted and tied scores and serializes fitted state", {
  class_names = c("yes", "no")
  predicted = c(0.8, 0.2, 0.2, 0.6, 0.4, 0.4)
  prob = cbind(yes = predicted, no = 1 - predicted)
  truth = factor(c("yes", "no", "yes", "yes", "no", "yes"), levels = class_names)
  calibrator = CalibratorIsotonic$new()
  calibrator$train(truth, prob, class_names)
  grid = seq(0.1, 0.9, by = 0.1)
  grid_prob = cbind(yes = grid, no = 1 - grid)
  expected = calibrator$predict(grid_prob, class_names)
  restored = unserialize(serialize(calibrator, NULL))

  testthat::expect_true(all(diff(expected[, "yes"]) >= -1e-12))
  testthat::expect_equal(restored$predict(grid_prob, class_names), expected, tolerance = 0)
})

test_that("CalibratorIsotonic handles a constant training score", {
  class_names = c("yes", "no")
  prob = cbind(yes = rep(0.5, 4L), no = rep(0.5, 4L))
  truth = factor(c("yes", "no", "yes", "no"), levels = class_names)
  calibrator = CalibratorIsotonic$new()
  calibrator$train(truth, prob, class_names)

  calibrated = calibrator$predict(cbind(yes = c(0.1, 0.9), no = c(0.9, 0.1)), class_names)

  testthat::expect_equal(calibrated[, "yes"], c(0.5, 0.5), tolerance = 0)
})

test_that("CalibratorBeta calibrates binary probability matrices", {
  data = make_binary_prediction_data()
  calibrator = CalibratorBeta$new(parameters = "ab")

  suppressWarnings(calibrator$train(
    truth = data$data$truth,
    prob = data$data$prob,
    class_names = data$class_names
  ))
  prob_calibrated = calibrator$predict(prob = data$data$prob, class_names = data$class_names)

  checkmate::expect_matrix(prob_calibrated, ncols = 2L)
  testthat::expect_true(all(abs(rowSums(prob_calibrated) - 1) < 1e-8))
})

test_that("Calibrator dictionary returns registered calibrators", {
  calibrator = clb("platt")

  testthat::expect_true(inherits(calibrator, "CalibratorPlatt"))
})

test_that("Binary-only calibrators reject multiclass matrices", {
  data = make_multiclass_prediction_data()

  testthat::expect_error(
    CalibratorBeta$new(parameters = "ab")$train(
      truth = data$data$truth,
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "does not support multiclass"
  )
  testthat::expect_error(
    CalibratorIsotonic$new()$train(
      truth = data$data$truth,
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "does not support multiclass"
  )
})

test_that("CalibratorSelector validates the active calibrator support", {
  data = make_multiclass_prediction_data()
  calibrator = clb("selector", choices = c("ovr", "beta"))

  calibrator$param_set$values$calibrator = "beta"
  testthat::expect_error(
    calibrator$train(
      truth = data$data$truth,
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "does not support multiclass"
  )

  calibrator$param_set$values$calibrator = "ovr"
  suppressWarnings(calibrator$train(
    truth = data$data$truth,
    prob = data$data$prob,
    class_names = data$class_names
  ))
  prob_calibrated = calibrator$predict(
    prob = data$data$prob,
    class_names = data$class_names
  )

  checkmate::expect_matrix(prob_calibrated, ncols = 3L)
  testthat::expect_identical(colnames(prob_calibrated), data$class_names)
})

test_that("Calibrator validates inputs", {
  data = make_binary_prediction_data()
  calibrator = CalibratorPlatt$new()

  testthat::expect_error(
    calibrator$train(
      truth = data$data$truth[-1L],
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "same number of observations"
  )

  extra_prob = cbind(data$data$prob, other = 0)
  testthat::expect_error(
    calibrator$train(
      truth = data$data$truth,
      prob = extra_prob,
      class_names = data$class_names
    ),
    "not present in the provided class names"
  )
})

test_that("Calibrator enforces fitted state and observes every declared class", {
  prob = cbind(yes = c(0.2, 0.4, 0.6, 0.8), no = c(0.8, 0.6, 0.4, 0.2))
  calibrator = CalibratorPlatt$new()

  testthat::expect_error(calibrator$predict(prob), "must be trained")
  testthat::expect_error(
    calibrator$train(
      truth = factor(rep("yes", 4L), levels = c("yes", "no")),
      prob = prob,
      class_names = c("yes", "no")
    ),
    "observed sample for every class"
  )

  truth = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no"))
  testthat::expect_invisible(calibrator$train(truth, prob, c("yes", "no")))
  checkmate::expect_matrix(calibrator$predict(prob), ncols = 2L)
})

test_that("Calibrator preserves fitted class semantics across named permutations", {
  OrderSensitiveCalibrator = R6::R6Class(
    "OrderSensitiveCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(
          id = "order_sensitive",
          properties = c("twoclass", "multiclass")
        )
      }
    ),
    private = list(
      .offset = NULL,
      .train = function(truth, prob, class_names) {
        private$.offset = (seq_along(class_names) - mean(seq_along(class_names))) / 20
      },
      .predict = function(prob, class_names) {
        sweep(prob, 2L, private$.offset, "+")
      }
    )
  )

  for (class_names in list(c("yes", "no"), c("a", "b", "c"))) {
    n_classes = length(class_names)
    prob = matrix(seq_len(4L * n_classes), nrow = 4L)
    prob = prob / rowSums(prob)
    colnames(prob) = class_names
    truth = factor(rep(class_names, length.out = 4L), levels = class_names)
    calibrator = OrderSensitiveCalibrator$new()
    calibrator$train(truth, prob, class_names)
    expected = calibrator$predict(prob, class_names)
    reversed_names = rev(class_names)
    permuted = calibrator$predict(prob[, reversed_names, drop = FALSE], reversed_names)

    testthat::expect_equal(permuted[, class_names, drop = FALSE], expected, tolerance = 1e-15)
  }

  calibrator = OrderSensitiveCalibrator$new()
  prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  calibrator$train(factor(c("yes", "no"), levels = c("yes", "no")), prob, c("yes", "no"))
  testthat::expect_error(
    calibrator$predict(cbind(yes = c(0.2, 0.8), maybe = c(0.8, 0.2)), c("yes", "maybe")),
    "same classes"
  )
})

test_that("Calibrator hash is read-only", {
  calibrator = CalibratorPlatt$new()

  testthat::expect_error(
    {
      calibrator$hash = "replacement"
    },
    "read-only"
  )
})

test_that("CalibratorPlatt rejects multiclass probability matrices", {
  data = make_multiclass_prediction_data()
  calibrator = CalibratorPlatt$new()

  testthat::expect_identical(calibrator$properties, "twoclass")
  testthat::expect_error(
    calibrator$train(
      truth = data$data$truth,
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "does not support multiclass"
  )
})

test_that("CalibratorSelector metadata and fitted configuration are stable", {
  prob = cbind(yes = c(0.1, 0.3, 0.7, 0.9), no = c(0.9, 0.7, 0.3, 0.1))
  truth = factor(c("no", "no", "yes", "yes"), levels = c("yes", "no"))
  selector = CalibratorSelector$new(c("platt", "isotonic"))
  selector$param_set$values$calibrator = "isotonic"
  configured_hash = selector$hash
  selector$train(truth, prob, c("yes", "no"))

  testthat::expect_identical(selector$hash, configured_hash)
  selector$param_set$values$calibrator = "platt"
  testthat::expect_error(selector$predict(prob, c("yes", "no")), "configuration changed")
})

test_that("CalibratorSelector declares every possible dependency", {
  selector = CalibratorSelector$new(c("platt", "beta"))

  testthat::expect_setequal(selector$packages, "stats")
  testthat::expect_identical(selector$properties, "twoclass")
})

test_that("CalibratorSelector derives class support semantically", {
  BinaryCalibrator = R6::R6Class(
    "BinaryCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "binary_metadata_test", properties = "twoclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) prob
    )
  )
  MulticlassCalibrator = R6::R6Class(
    "MulticlassCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "multiclass_metadata_test", properties = "multiclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) prob
    )
  )
  register_calibrator("binary_metadata_test", BinaryCalibrator)
  register_calibrator("multiclass_metadata_test", MulticlassCalibrator)
  on.exit(mlr3_calibrators$remove("binary_metadata_test"), add = TRUE)
  on.exit(mlr3_calibrators$remove("multiclass_metadata_test"), add = TRUE)

  selector = CalibratorSelector$new(c("binary_metadata_test", "multiclass_metadata_test"))
  prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  truth = factor(c("yes", "no"), levels = c("yes", "no"))

  testthat::expect_identical(selector$properties, "twoclass")
  selector$param_set$values$calibrator = "multiclass_metadata_test"
  testthat::expect_invisible(selector$train(truth, prob, c("yes", "no")))
})

test_that("CalibratorSelector hashes include the available choices", {
  beta_only = CalibratorSelector$new("beta")
  expanded = CalibratorSelector$new(c("beta", "isotonic"))

  testthat::expect_false(identical(beta_only$hash, expanded$hash))
})

test_that("CalibratorBeta fits and predicts with abm parameterization", {
  predicted = c(
    0.19118461,
    0.69832656,
    0.57185981,
    0.17469088,
    0.93496255,
    0.93460546,
    0.13657580,
    0.82677984,
    0.46865815,
    0.54898407,
    0.55162059,
    0.24411686,
    0.75530305,
    0.18720370,
    0.40717654,
    0.84647748,
    0.96687052,
    0.23130895,
    0.44591304,
    0.08347984
  )
  actual = c(1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0)
  prob = cbind(yes = predicted, no = 1 - predicted)
  truth = factor(ifelse(actual == 1, "yes", "no"), levels = c("yes", "no"))
  calibrator = CalibratorBeta$new("abm")

  testthat::expect_silent(calibrator$train(truth, prob, c("yes", "no")))
  testthat::expect_identical(calibrator$param_set$values$parameters, "abm")
  checkmate::expect_matrix(calibrator$predict(prob), nrows = length(predicted), ncols = 2L)
})

test_that("CalibratorBeta matches betacal when the unconstrained shape coefficients are valid", {
  testthat::skip_if_not_installed("betacal")

  set.seed(42L)
  p = stats::runif(200L, 0.05, 0.95)
  y = stats::rbinom(200L, 1L, p)
  prob = cbind(yes = p, no = 1 - p)
  truth = factor(ifelse(y == 1L, "yes", "no"), levels = c("yes", "no"))

  for (parameters in c("ab", "abm")) {
    ours = fit_beta_calibration(p = p, y = y, parameters = parameters)
    reference = NULL
    utils::capture.output({
      reference = betacal::beta_calibration(p = p, y = y, parameters = parameters)
    })
    testthat::expect_equal(
      unname(predict_beta_calibration(p, ours)),
      unname(betacal::beta_predict(p, reference)),
      tolerance = 0
    )

    calibrator = CalibratorBeta$new(parameters)
    calibrator$train(truth, prob, c("yes", "no"))
    testthat::expect_equal(
      unname(calibrator$predict(prob)[, "yes"]),
      unname(betacal::beta_predict(p, reference)),
      tolerance = 0
    )
  }
})

test_that("CalibratorBeta stores the canonical second coefficient sign for ab", {
  set.seed(42L)
  p = stats::runif(200L, 0.05, 0.95)
  y = stats::rbinom(200L, 1L, p)
  raw_data = data.frame(
    y = y,
    lp = log(2 * p),
    l1p = log(2 * (1 - p))
  )
  raw_model = stats::glm(y ~ lp + l1p - 1, family = stats::binomial(), data = raw_data)
  fitted = fit_beta_calibration(p, y, parameters = "ab")

  testthat::expect_equal(
    unname(stats::coef(fitted$model)[["l1p"]]),
    -unname(stats::coef(raw_model)[["l1p"]]),
    tolerance = 0
  )
  testthat::expect_equal(
    predict_beta_calibration(p, fitted),
    unname(stats::predict(raw_model, newdata = raw_data, type = "response")),
    tolerance = 0
  )
})

test_that("CalibratorBeta warns when all shape coefficients are eliminated", {
  p = seq(0.05, 0.95, length.out = 20L)
  y = rev(as.integer(p > 0.5))

  for (parameters in c("abm", "ab")) {
    testthat::expect_warning(
      fitted <- suppressWarnings(
        fit_beta_calibration(p, y, parameters = parameters),
        classes = "simpleWarning"
      ),
      "fitted map is constant"
    )
    calibrated = predict_beta_calibration(p, fitted)
    testthat::expect_equal(diff(range(calibrated)), 0, tolerance = 0, info = parameters)
  }
})

test_that("CalibratorBeta enforces monotone beta maps", {
  p = seq(0.05, 0.95, length.out = 10L)
  y = c(1, 1, 0, 1, 0, 0, 1, 0, 0, 0)

  for (parameters in c("abm", "ab")) {
    fitted = suppressWarnings(fit_beta_calibration(p, y, parameters = parameters))
    shape = stats::coef(fitted$model)[intersect(names(stats::coef(fitted$model)), c("lp", "l1p"))]
    calibrated = predict_beta_calibration(p, fitted)

    testthat::expect_true(all(shape >= 0), info = parameters)
    testthat::expect_true(all(diff(calibrated) >= -1e-12), info = parameters)
  }
})

test_that("Calibrator lifecycle bindings and reset behave as documented", {
  prob = cbind(yes = c(0.2, 0.4, 0.6, 0.8), no = c(0.8, 0.6, 0.4, 0.2))
  truth = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no"))
  calibrator = CalibratorPlatt$new()
  config_hash = calibrator$hash

  testthat::expect_false(calibrator$is_trained)
  testthat::expect_null(calibrator$class_names)
  testthat::expect_error(calibrator$is_trained <- TRUE, "read-only")
  testthat::expect_error(calibrator$class_names <- c("yes", "no"), "read-only")

  calibrator$train(truth, prob, c("yes", "no"))
  testthat::expect_true(calibrator$is_trained)
  testthat::expect_identical(calibrator$class_names, c("yes", "no"))
  testthat::expect_identical(calibrator$hash, config_hash)

  result = calibrator$reset()
  testthat::expect_identical(result, calibrator)
  testthat::expect_false(calibrator$is_trained)
  testthat::expect_null(calibrator$class_names)
  testthat::expect_identical(calibrator$hash, config_hash)
  testthat::expect_error(calibrator$predict(prob), "must be trained")
})

test_that("Calibrator validation failure leaves a prior fit usable", {
  prob = cbind(yes = c(0.2, 0.4, 0.6, 0.8), no = c(0.8, 0.6, 0.4, 0.2))
  truth = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no"))
  calibrator = CalibratorPlatt$new()
  calibrator$train(truth, prob, c("yes", "no"))
  expected = calibrator$predict(prob)

  testthat::expect_error(
    calibrator$train(truth[-1L], prob, c("yes", "no")),
    "same number of observations"
  )
  testthat::expect_true(calibrator$is_trained)
  testthat::expect_equal(calibrator$predict(prob), expected, tolerance = 1e-15)
})

test_that("Calibrator backend fitting failure leaves the object untrained", {
  FailCalibrator = R6::R6Class(
    "FailCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "fail", properties = "twoclass")
      }
    ),
    private = list(
      .marker = NULL,
      .reset = function() {
        private$.marker = NULL
      },
      .train = function(truth, prob, class_names) {
        private$.marker = "partial"
        stop("backend boom")
      },
      .predict = function(prob, class_names) prob
    )
  )

  prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  truth = factor(c("yes", "no"), levels = c("yes", "no"))
  calibrator = FailCalibrator$new()

  testthat::expect_error(calibrator$train(truth, prob, c("yes", "no")), "backend boom")
  testthat::expect_false(calibrator$is_trained)
  testthat::expect_null(calibrator$class_names)
  testthat::expect_null(calibrator$.__enclos_env__$private$.marker)
})

test_that("Calibrator preserves braces in backend error messages", {
  BraceErrorCalibrator = R6::R6Class(
    "BraceErrorCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() super$initialize(id = "brace_error", properties = "twoclass")
    ),
    private = list(
      .train = function(truth, prob, class_names) stop("backend failed on {closure}"),
      .predict = function(prob, class_names) prob
    )
  )
  prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  truth = factor(c("no", "yes"), levels = c("yes", "no"))

  testthat::expect_error(
    BraceErrorCalibrator$new()$train(truth, prob, c("yes", "no")),
    "backend failed on {closure}",
    fixed = TRUE
  )
})

test_that("Calibrator rejects stale configuration until retrained", {
  prob = cbind(yes = c(0.2, 0.4, 0.6, 0.8), no = c(0.8, 0.6, 0.4, 0.2))
  truth = factor(c("yes", "no", "yes", "no"), levels = c("yes", "no"))
  calibrator = CalibratorBeta$new("ab")
  suppressWarnings(calibrator$train(truth, prob, c("yes", "no")))

  calibrator$param_set$values$parameters = "abm"
  testthat::expect_error(calibrator$predict(prob), "configuration changed")
  suppressWarnings(calibrator$train(truth, prob, c("yes", "no")))
  checkmate::expect_matrix(calibrator$predict(prob), ncols = 2L)
})

test_that("Calibrator rejects duplicated or unknown properties", {
  testthat::expect_error(
    Calibrator$new(id = "bad", properties = c("twoclass", "twoclass")),
    "duplicated"
  )
  testthat::expect_error(
    Calibrator$new(id = "bad", properties = "regression"),
    "subset"
  )
})

test_that("multiclass-only calibrators still accept binary input", {
  MulticlassOnly = R6::R6Class(
    "MulticlassOnly",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "mc_only", properties = "multiclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) prob
    )
  )

  binary_prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  binary_truth = factor(c("yes", "no"), levels = c("yes", "no"))
  multi = make_multiclass_prediction_data()
  calibrator = MulticlassOnly$new()

  testthat::expect_invisible(calibrator$train(binary_truth, binary_prob, c("yes", "no")))
  calibrator$reset()
  testthat::expect_invisible(calibrator$train(
    multi$data$truth,
    multi$data$prob,
    multi$class_names
  ))
})

test_that("built-in calibrators clear fitted backend state on reset", {
  data = make_binary_prediction_data()

  platt = CalibratorPlatt$new()
  platt$train(data$data$truth, data$data$prob, data$class_names)
  platt$reset()
  testthat::expect_null(platt$.__enclos_env__$private$.model)

  isotonic = CalibratorIsotonic$new()
  isotonic$train(data$data$truth, data$data$prob, data$class_names)
  isotonic$reset()
  testthat::expect_null(isotonic$.__enclos_env__$private$.calibrator)

  beta = CalibratorBeta$new("ab")
  suppressWarnings(beta$train(data$data$truth, data$data$prob, data$class_names))
  beta$reset()
  testthat::expect_null(beta$.__enclos_env__$private$.calibrator)

  selector = CalibratorSelector$new(c("platt", "isotonic"))
  selector$train(data$data$truth, data$data$prob, data$class_names)
  selector$reset()
  testthat::expect_null(selector$.__enclos_env__$private$.calibrator)
})

test_that("Calibrator enforces declared packages before private hooks", {
  probe = new.env(parent = emptyenv())
  probe$hook_ran = FALSE

  FakePkgCalibrator = R6::R6Class(
    "FakePkgCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(
          id = "fake_pkg",
          properties = "twoclass",
          packages = "definitely_missing_pkg_xyz"
        )
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        probe$hook_ran = TRUE
      },
      .predict = function(prob, class_names) prob
    )
  )

  prob = cbind(yes = c(0.2, 0.8), no = c(0.8, 0.2))
  truth = factor(c("yes", "no"), levels = c("yes", "no"))
  calibrator = FakePkgCalibrator$new()

  testthat::expect_error(
    calibrator$train(truth, prob, c("yes", "no")),
    "definitely_missing_pkg_xyz|package"
  )
  testthat::expect_false(probe$hook_ran)
  testthat::expect_false(calibrator$is_trained)
})

test_that("calibrator input helpers validate data", {
  data = make_binary_prediction_data()
  parsed = calibrator_train_inputs(
    truth = data$data$truth,
    prob = data$data$prob,
    class_names = data$class_names
  )
  predicted = calibrator_predict_inputs(prob = data$data$prob, class_names = data$class_names)

  testthat::expect_identical(parsed$class_names, data$class_names)
  testthat::expect_identical(predicted$class_names, data$class_names)
  testthat::expect_error(
    calibrator_train_inputs(
      truth = data$data$truth[-1L],
      prob = data$data$prob,
      class_names = data$class_names
    ),
    "same number of observations"
  )
})
