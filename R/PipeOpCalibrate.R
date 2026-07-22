#' @title Calibration PipeOp
#'
#' @description
#' Calibrates a probabilistic [`Learner`][mlr3::Learner],
#' [`GraphLearner`][mlr3pipelines::GraphLearner], or bare
#' [`Graph`][mlr3pipelines::Graph] (converted with [as_learner()]).
#'
#' Both strategies fit the calibrator on predictions held out from model training
#' (`r format_bib("niculescu2005")`):
#'
#' - `strategy = "oof"`: one calibrator on pooled out-of-fold predictions; one uncalibrated
#'   model refitted on the full task and calibrated at prediction time
#'   (scikit-learn `CalibratedClassifierCV` with `ensemble = FALSE`; `r format_bib("pedregosa2011")`).
#' - `strategy = "per_fold"`: one calibrator per fold; fold-wise calibrated probabilities are averaged
#'   (`ensemble = TRUE`).
#'
#' When the PipeOp runs an uninstantiated resampling itself (no precomputed `rr`), it clones the task and sets the
#' target `"stratum"` role before `resample()`, which helps avoid missing-class folds while keeping
#' the requested scheme and fold count.
#' A pre-instantiated resampling is cloned and its stored train/test splits are reused unchanged.
#' Infeasible class counts still fail with a diagnostic.
#' A supplied [`ResampleResult`][mlr3::ResampleResult] is validated and reused as-is
#' (never rerun, re-stratified, or overwritten).
#' Repeated or overlapping test predictions, as produced by repeated cross-validation or bootstrap,
#' are averaged by task row before fitting the pooled `"oof"` calibrator.
#' Rows without any test prediction are omitted from that fit.
#' `predict_type` is fixed to `"prob"`.
#'
#' Calibrators are [Calibrator] prototypes.
#' Wrapper-style parameter ids expose both the learner and calibrator for outer `AutoTuner` use.
#' Multiclass calibration requires a calibrator with the `"multiclass"` property.
#' `AutoTuner` is not accepted as `learner`; wrap the calibrated object in an outer `AutoTuner`,
#' or use [tune_then_calibrate()] for sequential tuning then calibration.
#' Use `clb("selector")` to tune over several calibrators.
#' Put preprocessing that should enter the calibration resampling inside the wrapped `learner`,
#' not upstream of `po("calibrate")`.
#'
#' @param learner [`Learner`][mlr3::Learner],
#'   [`GraphLearner`][mlr3pipelines::GraphLearner], or
#'   [`Graph`][mlr3pipelines::Graph]
#'   Fixed predictive object to calibrate. Must predict probabilities.
#'   Bare graphs are converted with [as_learner()].
#'   `AutoTuner` objects are not accepted.
#' @param calibrator [Calibrator]
#'   Calibrator prototype. Defaults to `clb("platt")`.
#' @param strategy `character(1)`
#'   Calibration strategy. One of `"oof"` or `"per_fold"`.
#' @param resampling [`Resampling`][mlr3::Resampling]
#'   Resampling strategy used to generate calibration predictions.
#'   The object is cloned during construction.
#'   If it is already instantiated, its stored train/test splits are preserved and are not re-stratified.
#' @param rr [`ResampleResult`][mlr3::ResampleResult]
#'   Optional precomputed resample result for the same calibration task.
#'   When supplied, `learner` and `resampling` must be `NULL`, and learner parameters are not exposed because the
#'   out-of-fold predictions of the stored fitted learners are reused unchanged for calibrator fitting.
#'   With `strategy = "oof"` a single uncalibrated model is still refitted on the full task,
#'   using the configuration of the stored learners.
#' @param param_vals `list`
#'   Initial parameter values for the merged learner and calibrator parameter
#'   space, exposed using wrapped learner ids and calibrator ids.
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   set.seed(1)
#'
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner_cal = mlr3::as_learner(mlr3pipelines::po(
#'     "calibrate",
#'     learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
#'     calibrator = clb("platt"),
#'     strategy = "oof",
#'     resampling = mlr3::rsmp("cv", folds = 5)
#'   ))
#'
#'   learner_cal$train(task)
#'   learner_cal$predict(task)$score(mlr3::msr("classif.bbrier"))
#' }
#'
#' @references
#' `r format_bib("platt2000")`
#'
#' `r format_bib("zadrozny2002")`
#'
#' `r format_bib("niculescu2005")`
#'
#' `r format_bib("kull2017")`
#'
#' `r format_bib("pedregosa2011")`
#'
#' @aliases mlr_pipeops_calibrate
#' @export
PipeOpCalibrate = R6Class(
  "PipeOpCalibrate",
  inherit = PipeOp,
  public = list(
    #' @description
    #' Creates a new `PipeOpCalibrate` object.
    initialize = function(
      learner = NULL,
      calibrator = NULL,
      strategy = "oof",
      resampling = NULL,
      rr = NULL,
      param_vals = list()
    ) {
      assert_choice(strategy, c("oof", "per_fold"))

      private$.rr = if (is.null(rr)) NULL else rr$clone(deep = TRUE)
      private$.rr_hash = if (is.null(private$.rr)) NULL else calibration_resample_result_hash(private$.rr)
      private$.learner = calibration_pipeop_learner(learner = learner, rr = private$.rr)
      assert_probability_learner(private$.learner)
      private$.calibrator = resolve_calibrator(calibrator = calibrator)
      private$.learner_namespace = if (is.null(private$.rr)) learner_param_namespace(private$.learner) else NULL
      private$.calibrator_namespace = calibrator_param_namespace(private$.calibrator)
      private$.resampling = calibration_pipeop_resampling(resampling = resampling, rr = private$.rr)

      learner_param_vals = if (is.null(private$.rr)) {
        namespace_param_values(private$.learner$param_set$values, private$.learner_namespace)
      } else {
        list()
      }
      merged_param_vals = modifyList(
        learner_param_vals,
        namespace_param_values(private$.calibrator$param_set$values, private$.calibrator_namespace),
        keep.null = TRUE
      )
      merged_param_vals = modifyList(
        list(strategy = strategy),
        merged_param_vals,
        keep.null = TRUE
      )
      merged_param_vals = modifyList(merged_param_vals, param_vals, keep.null = TRUE)
      learner_param_sets = if (is.null(private$.rr)) {
        namespaced_param_set_entries(private$.learner$param_set, private$.learner_namespace)
      } else {
        list()
      }

      super$initialize(
        id = "calibrate",
        packages = unique(c(
          "mlr3calibration",
          private$.learner$packages,
          private$.calibrator$packages
        )),
        param_set = ParamSetCollection$new(c(
          list(ps(
            strategy = p_fct(
              levels = c("oof", "per_fold"),
              default = "oof",
              tags = "train"
            )
          )),
          learner_param_sets,
          namespaced_param_set_entries(private$.calibrator$param_set, private$.calibrator_namespace)
        )),
        param_vals = merged_param_vals,
        input = data.table(
          name = "input",
          train = "Task",
          predict = "Task"
        ),
        output = data.table(
          name = "output",
          train = "NULL",
          predict = "PredictionClassif"
        )
      )
    }
  ),
  active = list(
    #' @field learner [`Learner`][mlr3::Learner].
    #'   Read-only access to the wrapped learner prototype.
    learner = function(val) {
      if (!missing(val)) {
        cli_abort("`$learner` is read-only.")
      }

      private$.learner
    },

    #' @field calibrator [Calibrator].
    #'   Read-only access to the wrapped calibrator prototype.
    calibrator = function(val) {
      if (!missing(val)) {
        cli_abort("`$calibrator` is read-only.")
      }

      private$.calibrator
    },

    #' @field strategy `character(1)`.
    #'   Read-only access to the active calibration strategy.
    strategy = function(val) {
      if (!missing(val)) {
        cli_abort("`$strategy` is read-only. Set `param_set$values$strategy` instead.")
      }

      private$.current_strategy()
    },

    #' @field resampling [`Resampling`][mlr3::Resampling].
    #'   Read-only access to the resampling used to generate calibration
    #'   predictions.
    resampling = function(val) {
      if (!missing(val)) {
        cli_abort("`$resampling` is read-only.")
      }

      private$.resampling
    },

    #' @field rr [`ResampleResult`][mlr3::ResampleResult].
    #'   Read-only access to the optional precomputed resample result.
    rr = function(val) {
      if (!missing(val)) {
        cli_abort("`$rr` is read-only.")
      }

      private$.rr
    },

    #' @field learner_namespace `character(1)` or `NULL`.
    #'   Read-only access to the learner parameter namespace.
    learner_namespace = function(val) {
      if (!missing(val)) {
        cli_abort("`$learner_namespace` is read-only.")
      }

      private$.learner_namespace
    },

    #' @field calibrator_namespace `character(1)` or `NULL`.
    #'   Read-only access to the calibrator parameter namespace.
    calibrator_namespace = function(val) {
      if (!missing(val)) {
        cli_abort("`$calibrator_namespace` is read-only.")
      }

      private$.calibrator_namespace
    },

    #' @field predict_type `character(1)`.
    #'   Fixed to `"prob"`. Assigning any other value errors; assigning `"prob"` is a no-op.
    predict_type = function(val) {
      if (!missing(val) && !identical(val, "prob")) {
        cli_abort("`predict_type` is fixed to 'prob' for this PipeOp.")
      }
      "prob"
    }
  ),
  private = list(
    .learner = NULL,
    .calibrator = NULL,
    .resampling = NULL,
    .rr = NULL,
    .rr_hash = NULL,
    .learner_namespace = NULL,
    .calibrator_namespace = NULL,

    .train = function(inputs) {
      task = inputs[[1L]]
      class_names = task_class_names(task)
      components = private$.components()
      strategy = private$.current_strategy()
      rr = calibration_resample_result(
        task = task,
        learner = components$learner,
        resampling = private$.resampling,
        rr = private$.rr
      )

      if (strategy == "oof") {
        # Fit one calibrator on the pooled out-of-fold predictions, then refit a single
        # uncalibrated model on the full task. At predict time that single model is
        # calibrated with the fitted calibrator, mirroring scikit-learn's
        # CalibratedClassifierCV(ensemble = FALSE).
        data = pooled_calibration_data(rr = rr, class_names = class_names)
        calibrator = components$calibrator
        calibrator$train(truth = data$truth, prob = data$prob, class_names = class_names)
        final_learner = components$learner$clone(deep = TRUE)
        final_learner$train(task)
        self$state = list(
          strategy = strategy,
          class_names = class_names,
          learner = final_learner,
          calibrator = calibrator
        )
      } else {
        self$state = list(
          strategy = strategy,
          class_names = class_names,
          learners = rr$learners,
          calibrators = private$.train_per_fold_calibrators(
            rr = rr,
            calibrator = components$calibrator,
            class_names = class_names
          )
        )
      }

      list(NULL)
    },

    .predict = function(inputs) {
      task = inputs[[1L]]
      class_names = task_class_names(task)

      if (!setequal(class_names, self$state$class_names)) {
        cli_abort("Prediction task classes must match the classes used during calibration training.")
      }

      prob = if (self$state$strategy == "oof") {
        private$.predict_oof(task = task, class_names = class_names)
      } else {
        private$.predict_per_fold(task = task, class_names = class_names)
      }

      list(prediction_from_probability_matrix(task, prob, class_names = class_names))
    },

    .additional_phash_input = function() {
      list(
        private$.learner$hash,
        private$.calibrator$hash,
        private$.current_strategy(),
        if (is.null(private$.rr)) {
          resampling_phash_input(private$.resampling)
        } else {
          private$.rr_hash
        }
      )
    },

    .current_strategy = function() {
      strategy = self$param_set$values$strategy

      if (is.null(strategy)) {
        return("oof")
      }

      strategy
    },

    .train_per_fold_calibrators = function(rr, calibrator, class_names) {
      lapply(rr$predictions(predict_sets = "test"), function(prediction) {
        fold_calibrator = calibrator$clone(deep = TRUE)
        data = prediction_calibration_data(prediction, class_names = class_names)
        fold_calibrator$train(
          truth = data$truth,
          prob = data$prob,
          class_names = data$class_names
        )
        fold_calibrator
      })
    },

    .predict_oof = function(task, class_names) {
      prob = prediction_probability_matrix(
        self$state$learner$predict(task),
        class_names = class_names
      )
      self$state$calibrator$predict(prob = prob, class_names = class_names)
    },

    .predict_per_fold = function(task, class_names) {
      predictions = lapply(self$state$learners, function(learner) learner$predict(task))
      prob = Map(
        function(prediction, calibrator) {
          calibrator$predict(
            prob = prediction_probability_matrix(prediction, class_names = class_names),
            class_names = class_names
          )
        },
        predictions,
        self$state$calibrators
      )
      aggregated = Reduce(`+`, prob) / length(prob)
      colnames(aggregated) = class_names
      aggregated
    },

    .components = function() {
      learner = private$.learner$clone(deep = TRUE)
      calibrator = private$.calibrator$clone(deep = TRUE)

      learner_values = if (is.null(private$.rr)) {
        component_param_values(
          values = self$param_set$values,
          param_set = learner$param_set,
          namespace = private$.learner_namespace
        )
      } else {
        list()
      }
      calibrator_values = calibrator_component_values(
        values = self$param_set$values,
        calibrator = calibrator,
        namespace = private$.calibrator_namespace
      )

      learner$param_set$values = modifyList(
        learner$param_set$values,
        learner_values,
        keep.null = TRUE
      )
      calibrator$param_set$values = modifyList(
        calibrator$param_set$values,
        calibrator_values,
        keep.null = TRUE
      )

      list(learner = learner, calibrator = calibrator)
    },

    deep_clone = function(name, value) {
      if (name == "state") {
        return(deep_clone_r6_objects(value))
      }

      super$deep_clone(name, value)
    }
  )
)

task_class_names = function(task) {
  assert_r6(task, classes = "TaskClassif")

  if (length(task$class_names) == 2L) {
    c(task$positive, setdiff(task$class_names, task$positive))
  } else {
    task$class_names
  }
}

prediction_from_probability_matrix = function(
  task,
  prob,
  class_names = NULL,
  row_ids = task$row_ids,
  truth = task$truth()
) {
  assert_r6(task, classes = "TaskClassif")

  if (is.null(class_names)) {
    class_names = task_class_names(task)
  }

  prob = normalize_probability_matrix(prob, class_names = class_names)
  prob_task = prob[, task$class_names, drop = FALSE]
  response = colnames(prob_task)[max.col(prob_task, ties.method = "first")]

  PredictionClassif$new(
    task = task,
    row_ids = row_ids,
    truth = truth,
    prob = prob_task,
    response = response
  )
}

prediction_calibration_data = function(prediction, class_names, truth = NULL) {
  assert_r6(prediction, classes = "PredictionClassif")
  assert_character(class_names, min.len = 2L, any.missing = FALSE)

  if (is.null(truth)) {
    truth = prediction$truth
  }

  list(
    truth = factor(as.character(truth), levels = class_names),
    prob = prediction_probability_matrix(prediction, class_names = class_names),
    class_names = class_names
  )
}

calibration_pipeop_learner = function(learner = NULL, rr = NULL) {
  if (is.null(learner) && is.null(rr)) {
    cli_abort("Either learner or rr object must be provided.")
  }

  if (!is.null(rr)) {
    assert_r6(rr, classes = "ResampleResult")

    if (!is.null(learner)) {
      cli_abort("'learner' and 'rr' cannot be supplied together.")
    }

    if (length(rr$learners) == 0L) {
      cli_abort("'rr' must contain stored learners.")
    }

    learners = rr$learners
  } else {
    learners = list(coerce_calibration_learner(learner))
  }

  if (any(vapply(learners, inherits, logical(1L), what = "AutoTuner"))) {
    cli_abort(paste0(
      "'learner' must be a fixed Learner, GraphLearner, or Graph. ",
      "Use an outer AutoTuner around po('calibrate') for joint tuning, ",
      "or tune_then_calibrate() for the sequential convenience workflow."
    ))
  }

  # Deep clone as in as_learner(x, clone = TRUE): the stored prototype must not
  # alias the caller's param_set or model state.
  learners[[1L]]$clone(deep = TRUE)
}

calibration_resample_result = function(task, learner, resampling, rr = NULL) {
  if (!is.null(rr)) {
    validate_calibration_rr(rr, task)
    return(rr)
  }

  # Stratify by the target so every fold retains class representation when feasible.
  # Clone first: never mutate the caller's task or its column roles.
  strat_task = task$clone()
  strat_task$set_col_roles(strat_task$target_names, add_to = "stratum")
  rr = resample(strat_task, learner, resampling, store_models = TRUE)
  validate_calibration_rr(rr, task, check_task_hash = FALSE)
  rr
}

validate_calibration_rr = function(rr, task, check_task_hash = TRUE) {
  assert_r6(rr, classes = "ResampleResult")
  assert_r6(task, classes = "TaskClassif")
  assert_flag(check_task_hash)

  if (!inherits(rr$task, "TaskClassif") || (check_task_hash && !identical(rr$task$hash, task$hash))) {
    cli_abort("The task used to create 'rr' must match the task supplied for calibration.")
  }

  class_names = task_class_names(task)

  if (!identical(task_class_names(rr$task), class_names)) {
    cli_abort("The class order in 'rr' must match the calibration task.")
  }

  if (
    length(rr$learners) == 0L ||
      any(vapply(
        rr$learners,
        function(learner) {
          learner$predict_type != "prob"
        },
        logical(1L)
      ))
  ) {
    cli_abort("'rr' must contain stored probability learners.")
  }

  predictions = rr$predictions(predict_sets = "test")

  if (length(predictions) != length(rr$learners)) {
    cli_abort("'rr' must contain one test prediction for every stored learner.")
  }

  prediction_row_ids = unlist(
    lapply(predictions, function(prediction) {
      assert_atomic_vector(prediction$row_ids, any.missing = FALSE)

      if (any(!(prediction$row_ids %in% task$row_ids))) {
        cli_abort("Test predictions in 'rr' contain row IDs outside the calibration task.")
      }
      expected_truth = as.character(task$truth(rows = prediction$row_ids))
      if (!identical(as.character(prediction$truth), expected_truth)) {
        cli_abort("Test prediction truth in 'rr' does not match the calibration task.")
      }

      prediction$row_ids
    }),
    use.names = FALSE
  )

  if (length(prediction_row_ids) == 0L) {
    cli_abort("'rr' must contain at least one test prediction.")
  }

  invisible(rr)
}

pooled_calibration_data = function(rr, class_names) {
  assert_r6(rr, classes = "ResampleResult")
  assert_character(class_names, min.len = 2L, any.missing = FALSE, unique = TRUE)

  prediction_data = lapply(rr$predictions(predict_sets = "test"), function(prediction) {
    data = prediction_calibration_data(prediction, class_names = class_names)
    data$row_ids = prediction$row_ids
    data
  })
  row_ids = unlist(lapply(prediction_data, function(data) data$row_ids), use.names = FALSE)
  truth = unlist(lapply(prediction_data, function(data) as.character(data$truth)), use.names = FALSE)
  prob = as.matrix(rbindlist(
    lapply(prediction_data, function(data) as.data.table(data$prob)),
    use.names = TRUE
  ))
  unique_row_ids = unique(row_ids)
  row_group = match(row_ids, unique_row_ids)
  first_prediction = match(seq_along(unique_row_ids), row_group)
  pooled_truth = truth[first_prediction]

  if (!identical(truth, pooled_truth[row_group])) {
    cli_abort("Repeated test predictions for the same row in 'rr' have inconsistent truth.")
  }

  pooled_prob = rowsum(prob, group = row_group, reorder = FALSE) /
    tabulate(row_group, nbins = length(unique_row_ids))
  rownames(pooled_prob) = NULL
  colnames(pooled_prob) = class_names

  list(
    row_ids = unique_row_ids,
    truth = factor(pooled_truth, levels = class_names),
    prob = pooled_prob,
    class_names = class_names
  )
}

calibration_resample_result_hash = function(rr) {
  assert_r6(rr, classes = "ResampleResult")

  predictions = lapply(rr$predictions(predict_sets = "test"), function(prediction) {
    list(
      row_ids = prediction$row_ids,
      truth = as.character(prediction$truth),
      prob = prediction$prob
    )
  })
  learner_hashes = vapply(
    rr$learners,
    function(learner) {
      calculate_hash(list(
        class = class(learner),
        id = learner$id,
        configuration = learner$hash,
        state = learner$state
      ))
    },
    character(1L)
  )

  calculate_hash(list(
    task = rr$task$hash,
    resampling = list(
      class = class(rr$resampling),
      id = rr$resampling$id,
      instance = rr$resampling$instance
    ),
    predictions = predictions,
    learners = learner_hashes
  ))
}
