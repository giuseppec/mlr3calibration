#' @title Tune-then-Out-of-Fold Calibration Pipeline Operator
#'
#' @description
#' `PipeOpCalibrationTuneFirstOOF` first **hyper-parameter tunes** an
#' `mlr3tuning::AutoTuner` on the full training task and subsequently
#' performs **out-of-fold (OOF) probability calibration** on the tuned base
#' learner by concatenating the OOF predictions of all resampling folds and
#' fitting **one single** calibration model.
#' Hence it combines the predictive performance of a tuned learner with the
#' *average-then-calibrate* strategy of
#' [`PipeOpCalibrationOOF`], i.e.~low-variance global calibration.
#'
#' Supported methods:
#' * **Platt scaling** (`"platt"`, logistic regression),
#' * **Isotonic regression** (`"isotonic"`),
#' * **Beta calibration** (`"beta"`; parameters `"abm"`, `"ab"`, …).
#'
#' @param learner [`AutoTuner`]\cr
#'   Tuner that produces a probability learner (`predict_type == "prob"`).
#' @param rr [`ResampleResult`][mlr3::ResampleResult]\cr
#'   Optional pre-computed result; skips the internal resampling step.
#' @param method `character(1)`\cr
#'   Calibration method. One of `"platt"`, `"isotonic"`, `"beta"`.
#' @param rsmp [`Resampling`][mlr3::Resampling]\cr
#'   Outer resampling scheme for OOF predictions (default `rsmp("cv", folds = 5)`).
#' @param parameters `character(1)`\cr
#'   Parameterisation for beta-calibration (`"abm"` by default).
#' @param param_vals `list`\cr
#'   Optional parameter values passed to the underlying learner.
#'
#' @field learner      Tuned base learner (`LearnerClassif`).
#' @field calibrator   Fitted calibration model (single object).
#' @field learners     List of OOF base learners (size *K*).
#' @field rsmp         Resampling used for OOF generation.
#'
#' @examples
#' \donttest{
#' library(mlr3verse)
#' set.seed(1)
#'
#' task <- tsk("sonar")
#'
#' at <- auto_tuner(
#'   tuner      = tnr("random_search"),
#'   learner    = lrn("classif.ranger", predict_type = "prob"),
#'   resampling = rsmp("cv", folds = 2),
#'   measure    = msr("classif.bbrier"),
#'   term_evals = 10
#' )
#'
#' po_cal <- PipeOpCalibrationTuneFirstOOF$new(
#'   learner = at,
#'   method  = "platt",
#'   rsmp    = rsmp("cv", folds = 5)
#' )
#'
#' graph <- po("scale") %>>% po_cal
#' learner_cal <- as_learner(graph)
#' learner_cal$train(task)
#' learner_cal$predict(task)$score(msr("classif.bbrier"))
#' }
#' @export
PipeOpCalibrationTuneFirstOOF <- R6::R6Class(
  "PipeOpCalibrationTuneFirstOOF",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner     = NULL,
    method      = NULL,
    rsmp        = NULL,
    learners    = NULL,
    calibrator  = NULL,
    rr          = NULL,
    parameters  = NULL,

    initialize = function(learner   = NULL,
      method    = "platt",
      rsmp      = NULL,
      rr        = NULL,
      parameters = NULL,
      param_vals = list()) {

      if (is.null(learner) && is.null(rr))
        stop("Either 'learner' (AutoTuner) or 'rr' must be supplied.")

      if (!is.null(rr)) self$rr <- rr

      if (!is.null(learner)) {
        if (!inherits(learner, "AutoTuner"))
          stop("'learner' must be an AutoTuner.")
        self$learner <- learner$clone()
        id <- self$learner$base_learner()$id
      } else {
        self$learner <- self$rr$learners[[1]]$clone()
      }

      self$rsmp <- rsmp("cv", folds = 5)
      self$method     <- method
      self$parameters <- parameters
      self$learners   <- list()

      super$initialize(
        id         = id,
        param_set  = alist(self$learner$param_set),
        param_vals = param_vals,
        input  = data.table(name = "input",  train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL",
          predict = "PredictionClassif")
      )
    }
  ),

  active = list(
    predict_type = function(val) "prob"
  ),

  private = list(

    .train = function(inputs) {
      on.exit(lgr::get_logger("mlr3")$set_threshold("info"))
      lgr::get_logger("mlr3")$set_threshold("warn")

      task     <- inputs[[1]]
      positive <- task$positive

      ## ── 1. tune on full task ────────────────────────────────────────────────
      at <- self$learner$clone()
      at$train(task)                       # triggers tuning
      tuned_learner <- at$learner          # best found learner

      ## ── 2. obtain OOF predictions ──────────────────────────────────────────
      rr <- if (is.null(self$rr)) {
        resample(task, tuned_learner, self$rsmp, store_models = TRUE)
      } else self$rr

      self$learners <- rr$learners                       # K fitted models
      oof_preds     <- lapply(rr$predictions("test"), as.data.table)
      oof_dt        <- data.table::rbindlist(oof_preds, use.names = TRUE)


      calibration_data <- data.table(
        truth    = oof_dt$truth,
        response = as.numeric(oof_dt[[paste0("prob.", positive)]])
      )
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      task_for_calibrator = as_task_classif(calibration_data,
                                            target = "truth",
                                            positive = positive,
                                            id = "task_cal")

      ## ── 3. fit single calibrator ───────────────────────────────────────────
      if(is.null(self$parameters)){
        calibrator <- clb(self$method)
      }else{
        calibrator <- clb(self$method, self$parameters)
      }

      calibrator$train(task_for_calibrator)
      self$calibrator = calibrator


      list(NULL)
    },

    .predict = function(inputs) {
      task     <- inputs[[1]]
      positive <- task$positive

      ## ensemble predictions
      base_preds <- lapply(self$learners, function(lrn) as.data.table(lrn$predict(task)))

      prob_pos <- rowMeans(sapply(base_preds, function(p)
        as.numeric(p[[paste0("prob.", positive)]])))
      prob_neg <- 1 - prob_pos

      task_for_calibration  <- as_task_classif(data.table(response = prob_pos,
                                              truth    = task$truth()),
                                   target = "truth", positive = positive,
                                   id = "task_cal")

      prob_pos = self$calibrator$predict(task_for_calibration)
      prob_neg = 1- prob_pos

      response <- ifelse(prob_pos > 0.5, positive, task$negative)

      prob = cbind(prob_pos, prob_neg)
      colnames(prob) = c(task$positive, task$negative)

      pred <- PredictionClassif$new(
        task     = task,
        row_ids  = task$row_ids,
        truth    = task$truth(),
        prob     = prob,
        response = response
      )
      list(pred)
    },

    .additional_phash_input = function() list(self$learner$hash)
  )
)

## register in mlr3pipelines
mlr3pipelines::mlr_pipeops$add("calibration_tune_first_oof",
  PipeOpCalibrationTuneFirstOOF)
