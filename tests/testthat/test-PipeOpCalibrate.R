testthat::skip_if_not_installed("rpart")
testthat::skip_if_not_installed("mlr3learners")

test_that("PipeOpCalibrate is registered", {
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")

  testthat::expect_true(inherits(
    mlr3pipelines::po("calibrate", learner = learner, calibrator = clb("platt")),
    "PipeOpCalibrate"
  ))
})

test_that("PipeOpCalibrate rejects non-probability learners", {
  testthat::expect_error(
    PipeOpCalibrate$new(
      learner = mlr3::lrn("classif.rpart", predict_type = "response"),
      calibrator = clb("platt")
    ),
    "predict_type has to be 'prob'"
  )
})

test_that("PipeOpCalibrate supports oof and per_fold strategies", {
  tasks = make_binary_split()

  learner_oof = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 3)
  ))
  learner_per_fold = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "per_fold",
    resampling = mlr3::rsmp("cv", folds = 3)
  ))

  expect_calibrated_prediction(learner_oof, tasks$train, tasks$test)
  expect_calibrated_prediction(learner_per_fold, tasks$train, tasks$test)
})

test_that("oof pooling averages repeated predictions by task row and preserves instantiated splits", {
  task = make_binary_task()
  class_names = task_class_names(task)
  spy_state = new.env(parent = emptyenv())
  PoolingSpyCalibrator = R6::R6Class(
    "PoolingSpyCalibrator",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(id = "pooling_spy", properties = "twoclass")
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        spy_state$truth = truth
        spy_state$prob = prob
      },
      .predict = function(prob, class_names) prob
    )
  )
  resamplings = list(
    repeated_cv = mlr3::rsmp("repeated_cv", folds = 3L, repeats = 2L),
    bootstrap = mlr3::rsmp("bootstrap", ratio = 0.67, repeats = 3L)
  )

  set.seed(42L)
  for (resampling_id in names(resamplings)) {
    resampling = resamplings[[resampling_id]]
    resampling$instantiate(task)
    rr = mlr3::resample(
      task,
      mlr3::lrn("classif.featureless", predict_type = "prob"),
      resampling,
      store_models = TRUE
    )
    predictions = rr$predictions(predict_sets = "test")
    row_ids = unlist(lapply(predictions, function(prediction) prediction$row_ids), use.names = FALSE)
    prob = do.call(rbind, lapply(predictions, function(prediction) {
      prediction_probability_matrix(prediction, class_names = class_names)
    }))
    unique_row_ids = unique(row_ids)
    group = match(row_ids, unique_row_ids)
    expected_prob = t(vapply(
      seq_along(unique_row_ids),
      function(index) colMeans(prob[group == index, , drop = FALSE]),
      numeric(length(class_names))
    ))
    testthat::expect_true(anyDuplicated(row_ids) != 0L)
    if (identical(resampling_id, "bootstrap")) {
      testthat::expect_lt(length(unique_row_ids), task$nrow)
    }

    supplied = PipeOpCalibrate$new(
      rr = rr,
      calibrator = PoolingSpyCalibrator$new(),
      strategy = "oof"
    )
    supplied$train(list(task))
    testthat::expect_equal(unname(spy_state$prob), unname(expected_prob), tolerance = 1e-12)
    testthat::expect_identical(as.character(spy_state$truth), as.character(task$truth(rows = unique_row_ids)))

    internal = PipeOpCalibrate$new(
      learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
      calibrator = PoolingSpyCalibrator$new(),
      strategy = "oof",
      resampling = resampling
    )
    internal$train(list(task))
    testthat::expect_equal(unname(spy_state$prob), unname(expected_prob), tolerance = 1e-12)
    testthat::expect_identical(as.character(spy_state$truth), as.character(task$truth(rows = unique_row_ids)))
    testthat::expect_identical(nrow(spy_state$prob), length(unique_row_ids))

    per_fold = PipeOpCalibrate$new(
      rr = rr,
      calibrator = PoolingSpyCalibrator$new(),
      strategy = "per_fold"
    )
    testthat::expect_error(per_fold$train(list(task)), NA)
  }
})

test_that("oof calibrator is trained on out-of-fold predictions, not in-sample (leakage guard)", {
  skip_if_rpart_backend_unavailable()

  spy_state = new.env()
  SpyCalibratorLeakageGuard = R6::R6Class(
    "SpyCalibratorLeakageGuard",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(
          id = "spy_leakage",
          param_set = paradox::ps(),
          properties = c("twoclass", "multiclass"),
          label = "Spy leakage calibrator"
        )
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        spy_state$prob = prob
        spy_state$truth = truth
        invisible()
      },
      .predict = function(prob, class_names) prob
    )
  )
  register_calibrator("spy_leakage", SpyCalibratorLeakageGuard)
  on.exit(mlr3_calibrators$remove("spy_leakage"), add = TRUE)

  task = make_binary_task()
  positive_class = task_class_names(task)[[1L]]
  base = mlr3::lrn("classif.rpart", predict_type = "prob")
  resampling = mlr3::rsmp("cv", folds = 4)
  resampling$instantiate(task)

  pipeop = PipeOpCalibrate$new(
    learner = base$clone(),
    calibrator = clb("spy_leakage"),
    strategy = "oof",
    resampling = resampling
  )
  pipeop$train(list(task))
  captured = sort(spy_state$prob[, positive_class])

  # Independently reproduce out-of-fold predictions using the SAME instantiated splits.
  rr = mlr3::resample(task, base$clone(), resampling, store_models = TRUE)
  oof = sort(unlist(lapply(
    rr$predictions("test"),
    function(prediction) prediction$prob[, positive_class]
  )))

  # In-sample predictions of a model trained on the FULL task (the leaky alternative).
  full_model = base$clone()
  full_model$train(task)
  in_sample = sort(full_model$predict(task)$prob[, positive_class])

  # The calibrator must see every training row exactly once,
  testthat::expect_length(spy_state$prob[, positive_class], task$nrow)
  # those inputs must equal the out-of-fold predictions,
  testthat::expect_equal(captured, oof, tolerance = 1e-12)
  # and must differ from the in-sample predictions a leaky implementation would use.
  testthat::expect_false(isTRUE(all.equal(captured, in_sample)))
})

test_that("oof refits a single full-data model and calibrates it without fold averaging", {
  skip_if_rpart_backend_unavailable()

  tasks = make_binary_split()
  class_names = task_class_names(tasks$train)
  base = mlr3::lrn("classif.rpart", predict_type = "prob")
  resampling = mlr3::rsmp("cv", folds = 3)
  resampling$instantiate(tasks$train)

  pipeop = PipeOpCalibrate$new(
    learner = base$clone(),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = resampling
  )
  pipeop$train(list(tasks$train))

  # oof stores a single refitted model, not a list of fold models.
  testthat::expect_true(inherits(pipeop$state$learner, "Learner"))
  testthat::expect_null(pipeop$state$learners)

  # Prediction equals the single model's scores mapped by the calibrator (no averaging).
  got = pipeop$predict(list(tasks$test))[[1L]]$prob
  raw = prediction_probability_matrix(pipeop$state$learner$predict(tasks$test), class_names = class_names)
  expected = pipeop$state$calibrator$predict(prob = raw, class_names = class_names)
  testthat::expect_equal(got[, class_names], expected[, class_names], tolerance = 1e-10)
})

test_that("PipeOpCalibrate calibrates full graphs directly", {
  tasks = make_binary_split()
  graph = get("%>>%", asNamespace("mlr3pipelines"))(
    mlr3pipelines::po("scale"),
    mlr3pipelines::po("learner", learner = mlr3::lrn("classif.rpart", predict_type = "prob"))
  )
  learner = mlr3::as_learner(mlr3pipelines::po(
    "calibrate",
    learner = graph,
    calibrator = clb("platt"),
    strategy = "per_fold",
    resampling = mlr3::rsmp("cv", folds = 3)
  ))

  expect_calibrated_prediction(learner, tasks$train, tasks$test)
})

test_that("PipeOpCalibrate exposes wrapped graph ids", {
  graph = get("%>>%", asNamespace("mlr3pipelines"))(
    mlr3pipelines::po("scale"),
    mlr3pipelines::po("learner", learner = mlr3::lrn("classif.rpart", predict_type = "prob"))
  )
  pipeop = PipeOpCalibrate$new(
    learner = graph,
    calibrator = clb("platt")
  )

  testthat::expect_true(all(c("scale.center", "classif.rpart.cp") %in% pipeop$param_set$ids()))
})

test_that("PipeOpCalibrate exposes read-only wrapper bindings", {
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "per_fold"
  )

  testthat::expect_identical(pipeop$strategy, "per_fold")
  testthat::expect_true(inherits(pipeop$learner, "Learner"))
  testthat::expect_true(inherits(pipeop$calibrator, "Calibrator"))
  testthat::expect_error(
    {
      pipeop$learner = mlr3::lrn("classif.rpart", predict_type = "prob")
    },
    "read-only"
  )
  testthat::expect_error(
    {
      pipeop$strategy = "oof"
    },
    "param_set\\$values\\$strategy"
  )
})

test_that("PipeOpCalibrate phash distinguishes resampling configurations", {
  make_pipeop = function(folds) {
    PipeOpCalibrate$new(
      learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
      resampling = mlr3::rsmp("cv", folds = folds)
    )
  }

  testthat::expect_identical(make_pipeop(2L)$phash, make_pipeop(2L)$phash)
  testthat::expect_false(identical(make_pipeop(2L)$phash, make_pipeop(3L)$phash))
})

test_that("PipeOpCalibrate clones the caller's resampling", {
  resampling = mlr3::rsmp("cv", folds = 2L)
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    resampling = resampling
  )

  testthat::expect_false(identical(pipeop$resampling, resampling))
  resampling$param_set$values$folds = 3L
  testthat::expect_identical(pipeop$resampling$param_set$values$folds, 2L)
})

test_that("PipeOpCalibrate reuses precomputed resample results", {
  tasks = make_binary_split()
  rr = mlr3::resample(
    tasks$train,
    mlr3::lrn("classif.rpart", predict_type = "prob"),
    mlr3::rsmp("cv", folds = 3),
    store_models = TRUE
  )
  pipeop = PipeOpCalibrate$new(
    rr = rr,
    calibrator = clb("platt"),
    strategy = "oof"
  )
  learner = as_learner(pipeop)

  testthat::expect_false("classif.rpart.cp" %in% pipeop$param_set$ids())
  expect_calibrated_prediction(learner, tasks$train, tasks$test)
})

test_that("PipeOpCalibrate constrains and validates precomputed resample results", {
  tasks = make_binary_split()
  rr = mlr3::resample(
    tasks$train,
    mlr3::lrn("classif.rpart", predict_type = "prob"),
    mlr3::rsmp("cv", folds = 2),
    store_models = TRUE
  )

  testthat::expect_error(
    PipeOpCalibrate$new(
      learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
      rr = rr
    ),
    "cannot be supplied together"
  )
  testthat::expect_error(
    PipeOpCalibrate$new(rr = rr, resampling = mlr3::rsmp("cv", folds = 3)),
    "cannot be supplied together"
  )

  pipeop = PipeOpCalibrate$new(rr = rr, calibrator = clb("platt"))
  testthat::expect_error(pipeop$train(list(tasks$test)), "task used to create 'rr'")
})

test_that("PipeOpCalibrate hashes distinguish fitted resample results", {
  tasks = make_binary_split()
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  make_rr = function(seed) {
    set.seed(seed)
    mlr3::resample(
      tasks$train,
      learner$clone(),
      mlr3::rsmp("cv", folds = 2),
      store_models = TRUE
    )
  }
  rr_1 = make_rr(1L)
  rr_2 = make_rr(2L)

  testthat::expect_identical(
    calibration_resample_result_hash(rr_1),
    calibration_resample_result_hash(rr_1$clone(deep = TRUE))
  )
  testthat::expect_false(identical(
    PipeOpCalibrate$new(rr = rr_1)$phash,
    PipeOpCalibrate$new(rr = rr_2)$phash
  ))
})

test_that("PipeOpCalibrate rejects AutoTuner learners", {
  autotuner = make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 2L)

  testthat::expect_error(
    PipeOpCalibrate$new(
      learner = autotuner,
      calibrator = clb("platt")
    ),
    "tune_then_calibrate"
  )
})

test_that("PipeOpCalibrate rejects AutoTuner learners stored in a ResampleResult", {
  skip_if_rpart_backend_unavailable()
  testthat::skip_if_not_installed("bbotk")
  testthat::skip_if_not_installed("mlr3tuning")
  task = make_binary_task()
  rr = mlr3::resample(
    task,
    make_autotuner(mlr3::lrn("classif.rpart", predict_type = "prob"), evals = 1L),
    mlr3::rsmp("holdout"),
    store_models = TRUE
  )

  testthat::expect_error(
    PipeOpCalibrate$new(rr = rr),
    "fixed Learner"
  )
})

test_that("PipeOpCalibrate exposes and applies wrapper-style parameters", {
  tasks = make_binary_split()
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = CalibratorBeta$new(parameters = "ab")
  )
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = CalibratorBeta$new(),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 3),
    param_vals = list(
      classif.rpart.cp = 0.03,
      beta.parameters = "ab"
    )
  ))

  testthat::expect_true(all(c("classif.rpart.cp", "beta.parameters") %in% pipeop$param_set$ids()))
  testthat::expect_identical(pipeop$predict_type, "prob")
  expect_calibrated_prediction(learner, tasks$train, tasks$test, suppress_training = TRUE)
})

test_that("PipeOpCalibrate declares wrapped learner and calibrator packages", {
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = CalibratorBeta$new("ab")
  )

  testthat::expect_true(all(c("mlr3calibration", "rpart", "stats") %in% pipeop$packages))
})

test_that("PipeOpCalibrate hashes distinguish selector choice spaces", {
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  beta_only = PipeOpCalibrate$new(
    learner = learner,
    calibrator = CalibratorSelector$new("beta")
  )
  expanded = PipeOpCalibrate$new(
    learner = learner,
    calibrator = CalibratorSelector$new(c("beta", "isotonic"))
  )

  testthat::expect_false(identical(beta_only$phash, expanded$phash))
})

test_that("PipeOpCalibrate sanitizes wrapped learner ids", {
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$id = "Uncalibrated learner"
  pipeop = PipeOpCalibrate$new(
    learner = learner,
    calibrator = clb("platt")
  )

  testthat::expect_true("Uncalibrated.learner.cp" %in% pipeop$param_set$ids())
})

test_that("Outer AutoTuner can jointly tune learner and calibrator parameters", {
  tasks = make_binary_split()
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = CalibratorBeta$new(),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  ))
  autotuner = mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = mlr3::rsmp("cv", folds = 2),
    measure = mlr3::msr("classif.bbrier"),
    search_space = paradox::ps(
      "calibrate.classif.rpart.cp" = paradox::p_dbl(0.001, 0.1),
      "calibrate.beta.parameters" = paradox::p_fct(c("ab", "abm"))
    ),
    terminator = bbotk::trm("evals", n_evals = 2),
    tuner = mlr3tuning::tnr("grid_search", resolution = 2)
  )

  suppressWarnings(autotuner$train(tasks$train))
  prediction = autotuner$predict(tasks$test)

  testthat::expect_true(all(
    c(
      "calibrate.classif.rpart.cp",
      "calibrate.beta.parameters"
    ) %in%
      names(autotuner$tuning_instance$archive$data)
  ))
  testthat::expect_s3_class(prediction, "PredictionClassif")
})

test_that("Outer AutoTuner can tune over selector calibrator choices", {
  tasks = make_binary_split()
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("selector", choices = c("platt", "beta")),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  ))
  search_space = paradox::ParamSetCollection$new(c(
    list(paradox::ps(
      "calibrate.classif.rpart.cp" = paradox::p_dbl(0.001, 0.1)
    )),
    list(calibrator_selector_search_space(choices = c("platt", "beta")))
  ))
  autotuner = mlr3tuning::AutoTuner$new(
    learner = learner,
    resampling = mlr3::rsmp("cv", folds = 2),
    measure = mlr3::msr("classif.bbrier"),
    search_space = search_space,
    terminator = bbotk::trm("evals", n_evals = 2),
    tuner = mlr3tuning::tnr("grid_search", resolution = 2)
  )

  suppressWarnings(autotuner$train(tasks$train))
  prediction = autotuner$predict(tasks$test)

  testthat::expect_true(all(
    c(
      "calibrate.classif.rpart.cp",
      "calibrate.calibrator",
      "calibrate.beta.parameters"
    ) %in%
      names(autotuner$tuning_instance$archive$data)
  ))
  testthat::expect_s3_class(prediction, "PredictionClassif")
})

test_that("PipeOpCalibrate leaves the mlr3 logger threshold unchanged", {
  tasks = make_binary_split()
  logger = lgr::get_logger("mlr3")
  previous = logger$threshold
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  ))

  on.exit(logger$set_threshold(previous), add = TRUE)
  logger$set_threshold("debug")
  debug_threshold = logger$threshold
  suppressWarnings(learner$train(tasks$train))

  testthat::expect_identical(logger$threshold, debug_threshold)
})

test_that("CalibratorSelector defaults remain deterministic", {
  TestCalibratorSelectorDummy = R6::R6Class(
    "TestCalibratorSelectorDummyDefault",
    inherit = Calibrator,
    public = list(
      initialize = function() {
        super$initialize(
          id = "dummy_selector_default",
          param_set = paradox::ps(),
          properties = "twoclass",
          label = "Dummy selector default calibrator"
        )
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        invisible()
      },
      .predict = function(prob, class_names) {
        prob
      }
    )
  )

  register_calibrator("dummy_selector_default", TestCalibratorSelectorDummy)
  on.exit(mlr3_calibrators$remove("dummy_selector_default"), add = TRUE)

  selector = clb("selector")
  testthat::expect_identical(selector$choices, c("platt", "beta", "isotonic"))
})

test_that("Selector picks up custom registered calibrators", {
  TestCalibratorSelectorDummy = R6::R6Class(
    "TestCalibratorSelectorDummy",
    inherit = Calibrator,
    public = list(
      initialize = function(alpha = 0.5) {
        super$initialize(
          id = "dummy_selector",
          param_set = paradox::ps(
            alpha = paradox::p_dbl(0, 1, default = 0.5, tags = "train")
          ),
          properties = "twoclass",
          label = "Dummy selector calibrator"
        )

        self$param_set$values$alpha = alpha
      }
    ),
    private = list(
      .train = function(truth, prob, class_names) {
        invisible()
      },
      .predict = function(prob, class_names) {
        prob
      }
    )
  )

  register_calibrator("dummy_selector", TestCalibratorSelectorDummy)
  on.exit(mlr3_calibrators$remove("dummy_selector"), add = TRUE)
  selector = clb("selector", choices = c("platt", "dummy_selector"))

  testthat::expect_true(all(c("calibrator", "dummy_selector.alpha") %in% selector$param_set$ids()))
})

test_that("PipeOpCalibrate supports multiclass ovr calibration", {
  data = make_multiclass_prediction_data()
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("ovr", calibrator = clb("isotonic")),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 3)
  ))

  prediction = expect_calibrated_prediction(learner, data$train, data$test, suppress_training = TRUE)
  checkmate::expect_matrix(prediction$prob, ncols = 3L)
  testthat::expect_identical(colnames(prediction$prob), data$test$class_names)
})

test_that("PipeOpCalibrate preserves a fitted binary calibrator when the positive class changes", {
  tasks = make_binary_split()
  learner = as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 2)
  ))
  learner$train(tasks$train)
  prediction = learner$predict(tasks$test)
  reversed_task = tasks$test$clone(deep = TRUE)
  reversed_task$positive = setdiff(reversed_task$class_names, reversed_task$positive)
  reversed_prediction = learner$predict(reversed_task)
  class_names = sort(colnames(prediction$prob))

  testthat::expect_equal(
    prediction$prob[, class_names, drop = FALSE],
    reversed_prediction$prob[, class_names, drop = FALSE],
    tolerance = 1e-12
  )
})

test_that("trained PipeOpCalibrate clones and serializes fitted objects independently", {
  tasks = make_binary_split()
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt"),
    resampling = mlr3::rsmp("cv", folds = 2)
  )
  pipeop$train(list(tasks$train))
  cloned = pipeop$clone(deep = TRUE)
  restored = unserialize(serialize(pipeop, NULL))

  testthat::expect_false(identical(pipeop$state$calibrator, cloned$state$calibrator))
  testthat::expect_false(identical(pipeop$state$learner, cloned$state$learner))
  expected = pipeop$predict(list(tasks$test))[[1L]]$prob
  testthat::expect_equal(restored$predict(list(tasks$test))[[1L]]$prob, expected, tolerance = 1e-12)
})

make_imbalanced_binary_task = function(n = 100L, n_pos = 5L, seed = 42L) {
  set.seed(seed)
  y = factor(c(rep("pos", n_pos), rep("neg", n - n_pos)))
  x = data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n), y = y)
  mlr3::as_task_classif(x, target = "y", positive = "pos")
}

test_that("PipeOpCalibrate stratifies package-executed resampling for per_fold and oof", {
  skip_if_rpart_backend_unavailable()
  task = make_imbalanced_binary_task()
  roles_before = task$col_roles

  learner_per_fold = mlr3::as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "per_fold",
    resampling = mlr3::rsmp("cv", folds = 5L)
  ))
  learner_oof = mlr3::as_learner(mlr3pipelines::po(
    "calibrate",
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    calibrator = clb("platt"),
    strategy = "oof",
    resampling = mlr3::rsmp("cv", folds = 5L)
  ))

  testthat::expect_error(learner_per_fold$train(task), NA)
  testthat::expect_error(learner_oof$train(task), NA)
  testthat::expect_identical(task$col_roles, roles_before)
  testthat::expect_false("y" %in% task$col_roles$stratum)
})

test_that("calibration_resample_result stratifies a user-supplied Resampling and preserves fold count", {
  task = make_imbalanced_binary_task()
  roles_before = task$col_roles
  resampling = mlr3::rsmp("cv", folds = 5L)
  rr = calibration_resample_result(
    task = task,
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    resampling = resampling
  )

  testthat::expect_identical(rr$resampling$iters, 5L)
  testthat::expect_identical(task$col_roles, roles_before)
  for (i in seq_len(5L)) {
    fold_truth = as.character(task$data(rows = rr$resampling$test_set(i), cols = "y")$y)
    testthat::expect_true(all(c("pos", "neg") %in% fold_truth), info = sprintf("fold %d", i))
  }
})

test_that("PipeOpCalibrate reuses a supplied rr without rerunning or stratifying it", {
  skip_if_rpart_backend_unavailable()
  task = make_imbalanced_binary_task()
  set.seed(1)
  rr = mlr3::resample(
    task,
    mlr3::lrn("classif.featureless", predict_type = "prob"),
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE
  )
  rr_hash = calibration_resample_result_hash(rr)
  roles_before = task$col_roles

  returned = calibration_resample_result(
    task = task,
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    resampling = mlr3::rsmp("cv", folds = 3L),
    rr = rr
  )
  pipeop = PipeOpCalibrate$new(rr = rr, calibrator = clb("platt"), strategy = "oof")
  pipeop$train(list(task))

  testthat::expect_identical(calibration_resample_result_hash(returned), rr_hash)
  testthat::expect_identical(calibration_resample_result_hash(pipeop$rr), rr_hash)
  testthat::expect_identical(task$col_roles, roles_before)
  testthat::expect_false("y" %in% task$col_roles$stratum)
})

test_that("calibration_resample_result keeps OOF row IDs aligned under stratified resampling", {
  task = make_imbalanced_binary_task()
  rr = calibration_resample_result(
    task = task,
    learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
    resampling = mlr3::rsmp("cv", folds = 5L)
  )

  prediction_row_ids = unlist(
    lapply(rr$predictions(predict_sets = "test"), function(prediction) prediction$row_ids),
    use.names = FALSE
  )
  testthat::expect_false(anyDuplicated(prediction_row_ids) != 0L)
  testthat::expect_true(setequal(prediction_row_ids, task$row_ids))
})

test_that("PipeOpCalibrate predict_type rejects invalid assignments", {
  pipeop = PipeOpCalibrate$new(
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    calibrator = clb("platt")
  )

  testthat::expect_identical(pipeop$predict_type, "prob")
  testthat::expect_error(pipeop$predict_type <- "response", "fixed to 'prob'")
  pipeop$predict_type = "prob"
  testthat::expect_identical(pipeop$predict_type, "prob")
})

test_that("prediction helpers preserve class order", {
  data = make_binary_prediction_data()
  prob = prediction_probability_matrix(data$prediction, class_names = data$class_names)
  restored_prediction = prediction_from_probability_matrix(
    task = data$task,
    prob = prob,
    class_names = data$class_names
  )

  testthat::expect_identical(task_class_names(data$task), data$class_names)
  testthat::expect_equal(colnames(prob), data$class_names)
  testthat::expect_s3_class(restored_prediction, "PredictionClassif")
  testthat::expect_equal(colnames(restored_prediction$prob), data$task$class_names)
})
