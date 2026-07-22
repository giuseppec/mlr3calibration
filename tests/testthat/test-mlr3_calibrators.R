test_that("calibrator dictionary registers and resolves calibrators", {
  ShiftCalibrator = R6::R6Class(
    "ShiftCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function(shift = 0) {
        super$initialize(
          id = "shift_test",
          param_set = paradox::ps(
            shift = paradox::p_dbl(-0.25, 0.25, default = 0, init = shift, tags = "train")
          ),
          properties = c("twoclass", "multiclass"),
          packages = character(),
          label = "Shift calibrator"
        )
      }
    ),
    private = list(
      .shift = 0,
      .reset = function() {
        private$.shift = 0
      },
      .train = function(truth, prob, class_names) {
        if (is.null(self$param_set$values$shift)) {
          private$.shift = 0
        } else {
          private$.shift = self$param_set$values$shift
        }
      },
      .predict = function(prob, class_names) {
        prob[, 1L] = prob[, 1L] + private$.shift
        normalize_probability_matrix(prob, class_names = class_names)
      }
    )
  )

  register_calibrator("shift_test", ShiftCalibrator)
  on.exit(mlr3_calibrators$remove("shift_test"), add = TRUE)
  calibrator = clb("shift_test", shift = 0.1)
  resolved = resolve_calibrator(calibrator)

  testthat::expect_true(inherits(calibrator, "ShiftCalibrator"))
  testthat::expect_true(inherits(resolved, "ShiftCalibrator"))
  testthat::expect_false(identical(calibrator, resolved))
  testthat::expect_invisible(assert_calibrator(resolved))
})

test_that("resolve_calibrator defaults to platt", {
  calibrator = resolve_calibrator()

  testthat::expect_true(inherits(calibrator, "CalibratorPlatt"))
})

test_that("register_calibrator errors on duplicate keys", {
  keys_before = mlr3_calibrators$keys()
  testthat::expect_error(
    register_calibrator("platt", CalibratorPlatt),
    "platt"
  )
  testthat::expect_identical(mlr3_calibrators$keys(), keys_before)
})

test_that("register_calibrator rejects invalid and unrelated generators", {
  UnrelatedGenerator = R6::R6Class("UnrelatedGenerator")
  SpoofedBase = R6::R6Class("Calibrator")
  SpoofedChild = R6::R6Class("SpoofedChild", inherit = SpoofedBase)
  keys_before = mlr3_calibrators$keys()
  on.exit(
    {
      for (key in c("scalar_test", "unrelated_test", "spoofed_test")) {
        if (key %in% mlr3_calibrators$keys()) {
          mlr3_calibrators$remove(key)
        }
      }
    },
    add = TRUE
  )

  testthat::expect_error(register_calibrator("scalar_test", 1), "R6ClassGenerator")
  testthat::expect_error(
    register_calibrator("unrelated_test", UnrelatedGenerator),
    "inherit.*Calibrator"
  )
  testthat::expect_error(
    register_calibrator("spoofed_test", SpoofedChild),
    "inherit.*Calibrator"
  )
  testthat::expect_identical(mlr3_calibrators$keys(), keys_before)
})

test_that("required-argument calibrators register and construct through clb", {
  RequiredArgCalibrator = R6::R6Class(
    "RequiredArgCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function(scale) {
        checkmate::assert_number(scale)
        super$initialize(
          id = "required_arg_test",
          properties = "twoclass",
          label = "Required-argument calibrator"
        )
        private$.scale = scale
      }
    ),
    private = list(
      .scale = NULL,
      .reset = function() {},
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) {
        private$.scale * prob[, 1L]
      }
    )
  )

  keys_before = mlr3_calibrators$keys()
  register_calibrator("required_arg_test", RequiredArgCalibrator)
  on.exit(mlr3_calibrators$remove("required_arg_test"), add = TRUE)

  testthat::expect_error(clb("required_arg_test"), "scale|argument|missing")
  calibrator = clb("required_arg_test", scale = 0.5)
  testthat::expect_identical(calibrator$id, "required_arg_test")
  testthat::expect_true("required_arg_test" %in% mlr3_calibrators$keys())
  testthat::expect_true(all(keys_before %in% mlr3_calibrators$keys()))
})

test_that("clb rejects constructed id mismatches", {
  MismatchCalibrator = R6::R6Class(
    "MismatchCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "other_id", properties = "twoclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {},
      .predict = function(prob, class_names) prob[, 1L]
    )
  )

  register_calibrator("mismatch_key", MismatchCalibrator)
  on.exit(mlr3_calibrators$remove("mismatch_key"), add = TRUE)

  testthat::expect_error(clb("mismatch_key"), "must equal dictionary key")
})

test_that("default-constructible custom calibrators work with selector and PipeOp", {
  skip_if_rpart_backend_unavailable()
  CustomCalibrator = R6::R6Class(
    "CustomCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function(offset = 0) {
        param_set = paradox::ps(
          offset = paradox::p_dbl(-0.2, 0.2, default = 0, tags = "train")
        )
        super$initialize(
          id = "custom_extension_test",
          param_set = param_set,
          properties = c("twoclass", "multiclass"),
          label = "Custom extension calibrator"
        )
        self$param_set$values$offset = offset
      }
    ),
    private = list(
      .offset = NULL,
      .reset = function() {
        private$.offset = NULL
      },
      .train = function(truth, prob, class_names) {
        offset = self$param_set$values$offset
        if (is.null(offset)) {
          offset = 0
        }
        private$.offset = offset
      },
      .predict = function(prob, class_names) {
        prob[, 1L] = pmin(pmax(prob[, 1L] + private$.offset, 0), 1)
        if (ncol(prob) == 2L) {
          return(prob[, 1L])
        }
        prob
      }
    )
  )

  register_calibrator("custom_extension_test", CustomCalibrator)
  on.exit(mlr3_calibrators$remove("custom_extension_test"), add = TRUE)

  calibrator = clb("custom_extension_test", offset = 0.05)
  testthat::expect_identical(calibrator$id, "custom_extension_test")

  selector = CalibratorSelector$new(c("platt", "custom_extension_test"))
  testthat::expect_true("custom_extension_test" %in% selector$choices)
  testthat::expect_true(any(grepl("custom_extension_test", names(selector$namespaces), fixed = TRUE)))

  tasks = make_binary_split()
  learner = mlr3::as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    calibrator = clb("custom_extension_test"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2L)
  ))
  expect_calibrated_prediction(learner, tasks$train, tasks$test)
})

test_that("mlr3_calibrators is exported and contains required built-ins", {
  testthat::expect_true(inherits(mlr3_calibrators, "Dictionary"))
  testthat::expect_true(all(
    c("platt", "beta", "isotonic", "ovr", "selector") %in% mlr3_calibrators$keys()
  ))
})

test_that("builtin calibrator registration remains idempotent", {
  keys_before = mlr3_calibrators$keys()
  validate_calibrator_generator("platt", CalibratorPlatt, require_default = TRUE)
  testthat::expect_identical(mlr3_calibrators$keys(), keys_before)
  testthat::expect_error(
    register_calibrator("platt", CalibratorPlatt),
    "already registered"
  )
  testthat::expect_identical(mlr3_calibrators$keys(), keys_before)
})

test_that("default-construction errors preserve braces", {
  BraceDefaultCalibrator = R6::R6Class(
    "BraceDefaultCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() stop("default failed on {closure}")
    )
  )

  testthat::expect_error(
    validate_calibrator_generator("brace_default", BraceDefaultCalibrator, require_default = TRUE),
    "default failed on {closure}",
    fixed = TRUE
  )
})
