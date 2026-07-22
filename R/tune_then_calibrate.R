#' @title Tune-then-calibrate convenience helper
#'
#' @description
#' Returns a [`Learner`][mlr3::Learner] (a `GraphLearner`) that, in one `$train()` call,
#' tunes an [`AutoTuner`][mlr3tuning::AutoTuner] on the full training task and then
#' calibrates that fixed tuned learner with `po("calibrate")`.
#'
#' @details
#' Training steps:
#'
#' 1. tune the supplied `AutoTuner` on the full training task,
#' 2. extract the tuned predictive object,
#' 3. build a calibrated learner with those fixed hyperparameters, and
#' 4. train that calibrated learner on the same task.
#'
#' Hyperparameters are not jointly tuned with calibration.
#' Prefer wrapping `po("calibrate", ...)` in an outer `AutoTuner`.
#'
#' @param learner [`AutoTuner`][mlr3tuning::AutoTuner]
#'   AutoTuner to train on the full training task before calibration.
#'   The tuned predictive object must predict probabilities.
#' @param calibrator [Calibrator]
#'   Calibrator prototype. Defaults to `clb("platt")`.
#' @param strategy `character(1)`
#'   Calibration strategy. One of `"oof"` or `"per_fold"`.
#' @param resampling [`Resampling`][mlr3::Resampling]
#'   Resampling strategy used by the subsequent calibration step.
#'   The object is cloned during construction.
#'   When that step executes an uninstantiated resampling itself, it stratifies by the task target by default,
#'   matching [PipeOpCalibrate].
#'   If the resampling is already instantiated, its stored train/test splits are preserved.
#'
#' @return A [`Learner`][mlr3::Learner], currently a `GraphLearner`.
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3tuning", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   set.seed(1)
#'
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3tuning::auto_tuner(
#'     tuner = mlr3tuning::tnr("grid_search", resolution = 2),
#'     learner = mlr3::lrn(
#'       "classif.rpart",
#'       predict_type = "prob",
#'       cp = paradox::to_tune(0.001, 0.1)
#'     ),
#'     resampling = mlr3::rsmp("cv", folds = 2),
#'     measure = mlr3::msr("classif.bbrier"),
#'     term_evals = 4
#'   )
#'
#'   learner_cal = tune_then_calibrate(
#'     learner = learner,
#'     calibrator = clb("platt"),
#'     strategy = "oof",
#'     resampling = mlr3::rsmp("cv", folds = 3)
#'   )
#'
#'   learner_cal$train(task)
#'   learner_cal$predict(task)$score(mlr3::msr("classif.bbrier"))
#' }
#'
#' @export
tune_then_calibrate = function(learner, calibrator = NULL, strategy = "oof", resampling = NULL) {
  assert_autotuner_learner(learner)

  as_learner(PipeOpTuneThenCalibrate$new(
    learner = learner,
    calibrator = calibrator,
    strategy = strategy,
    resampling = resampling
  ))
}

PipeOpTuneThenCalibrate = R6Class(
  "PipeOpTuneThenCalibrate",
  inherit = PipeOp,
  public = list(
    autotuner = NULL,
    calibrator = NULL,
    strategy = NULL,
    resampling = NULL,

    initialize = function(learner, calibrator = NULL, strategy = "oof", resampling = NULL) {
      assert_choice(strategy, c("oof", "per_fold"))
      assert_autotuner_learner(learner)

      self$autotuner = learner$clone(deep = TRUE)
      self$calibrator = resolve_calibrator(calibrator = calibrator)
      self$strategy = strategy
      self$resampling = calibration_pipeop_resampling(resampling = resampling)

      super$initialize(
        id = "tune_then_calibrate",
        packages = unique(c(
          "mlr3calibration",
          "mlr3tuning",
          self$autotuner$packages,
          self$calibrator$packages
        )),
        param_set = ps(),
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
    .train = function(inputs) {
      task = inputs[[1L]]
      autotuner = self$autotuner$clone(deep = TRUE)
      autotuner$train(task)

      tuned_learner = coerce_calibration_learner(autotuner$learner$clone(deep = TRUE))
      assert_probability_learner(tuned_learner)

      calibrated_learner = as_learner(mlr3pipelines::po(
        "calibrate",
        learner = tuned_learner,
        calibrator = self$calibrator$clone(deep = TRUE),
        strategy = self$strategy,
        resampling = self$resampling$clone(deep = TRUE)
      ))
      calibrated_learner$id = self$id
      calibrated_learner$train(task)

      self$state = list(
        autotuner = autotuner,
        tuned_learner = tuned_learner,
        calibrated_learner = calibrated_learner
      )

      list(NULL)
    },
    .predict = function(inputs) {
      task = inputs[[1L]]
      list(self$state$calibrated_learner$predict(task))
    },
    .additional_phash_input = function() {
      list(
        self$autotuner$hash,
        self$calibrator$hash,
        self$strategy,
        resampling_phash_input(self$resampling)
      )
    },
    deep_clone = function(name, value) {
      if (name == "state") {
        return(deep_clone_r6_objects(value))
      }

      super$deep_clone(name, value)
    }
  )
)

assert_autotuner_learner = function(learner) {
  assert_r6(learner, classes = "AutoTuner")
  assert_probability_learner(learner)

  invisible(learner)
}
