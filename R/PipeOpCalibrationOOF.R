#' @title Out-of-fold Calibration Pipeline Operator
#'
#' @description
#' Out-of-fold Pipeline operator for calibrating classification learner using different calibration methods.
#' Supports Platt scaling, isotonic regression, and beta calibration.
#' Further own scalings can be implemented
#'
#' @param learner [`Learner`][mlr3::Learner]\cr Base learner to be calibrated. predict_type has to be `"prob"`.
#' @param method `character(1)`\cr Calibration method to use. One of `"platt"`, `"isotonic"`, or `"beta"`. Default is `"platt"`.
#' @param rsmp [`Resampling`][mlr3::Resampling]\cr Resampling strategy for cross-validation. Default is `rsmp("cv", folds = 5)`.
#' @param parameters `character(1)`\cr Parameters for beta calibration. Default is `"abm"`.
#' @param param_vals `list`\cr param_vals, copied from base learner
#'
#' @field learner [`Learner`][mlr3::Learner]\cr Base learner to be calibrated.
#' @field method `character(1)`\cr Calibration method used.
#' @field rsmp [`Resampling`][mlr3::Resampling]\cr Resampling strategy.
#' @field learners `list`\cr List of learners obtained from resampling.
#' @field calibrator `list`\cr List of calibrator models.
#' @field parameters `character(1)`\cr Parameters for beta calibration.
#' @field predict_type `character(1)`\cr Set predict_type to `"prob"`.
#'
#' @references
#' Filho TMS, Kull M (2017). betacal: Beta Calibration_. R package version 0.1.0, <https://CRAN.R-project.org/package=betacal>.
#'
#' @examples
#' # Example usage
#' set.seed(1)
#' library(mlr3verse)
#'
#' # Load the task
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#'
#' # Initialize the base learner
#' learner_uncal <- lrn("classif.ranger", predict_type = "prob")
#'
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(PipeOpCalibrationOOF$new(learner = learner_uncal,
#'                                                 method = "platt",
#'                                                 rsmp = rsmp))
#'
#' # Set ID's for the learners
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the calibrated learner
#' learner_cal$train(task)
#' learner_cal$predict(task)$score(msr("classif.bbrier"))
#'
#' @export

PipeOpCalibrationOOF <- R6::R6Class(
  "PipeOpCalibrationOOF",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner = NULL,
    method = NULL,
    rsmp = NULL,
    learners = NULL,
    calibrator = NULL,
    parameters = NULL,

    #' @description
    #' Creates a new `PipeOpCalibrationPerFold` object.
    #' @param learner Base learner to be calibrated. predict_type has to be `"prob"`.
    #' @param method Calibration method to use. One of `"platt"`, `"isotonic"`, or `"beta"`. Default is `"platt"`.
    #' @param rsmp Resampling strategy for cross-validation. Default is `rsmp("cv", folds = 5)`.
    #' @param parameters Parameters for beta calibration. Default is `"abm"`.
    #' @param param_vals param_vals, copied from base learner
    initialize = function(#id = paste0(self$learner$id, ".calibrated_oof_", method),
      learner,
      method = "platt",
      rsmp,
      parameters = "abm",
      param_vals = list()) {
      self$learner = learner
      self$method = method
      self$rsmp = rsmp
      self$learners = list()
      self$parameters = parameters
      super$initialize(id = self$learner$base_learner()$id,
        param_set = alist(self$learner$param_set),
        param_vals = param_vals,
        input = data.table(name = "input", train = "Task",
          predict = "Task"),
        output = data.table(name = "output", train = "NULL",
          predict = "PredictionClassif"),
      )
    }
  ),
  active = list(
    predict_type = function(val) {
      if (!missing(val)) {
        assert_subset(val, names(mlr_reflections$learner_predict_types[[self$learner$task_type]]))
        self$learner$predict_type = val
      }
      self$learner$predict_type
    }
  ),
  private = list(
    .train = function(inputs) {
      on.exit(lgr::get_logger("mlr3")$set_threshold("info"))

      lgr::get_logger("mlr3")$set_threshold("warn")

      task = inputs[[1]]
      positive = task$positive


      rr = resample(task, self$learner, self$rsmp, store_models = TRUE)
      self$learners = rr$learners

      preds = rr$predictions(predict_sets = "test")
      preds = lapply(preds, as.data.table)
      # TODO: do not use dplyr functions
      #pred = bind_rows(preds)
      pred_data = data.table::rbindlist(preds, use.names = TRUE)

      #pred_data = as.data.table(pred)
      calibration_data = data.table(truth = pred_data$truth,
        response = with(pred_data, get(paste0("prob.", positive))))
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      task_for_calibrator = as_task_classif(calibration_data,
                                            target = "truth",
                                            positive = positive,
                                            id = "task_cal")

      self$calibrator <- clb(self$method)
      self$calibrator$train(task_for_calibrator)

      return(list(NULL))
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      predictions = list()
      for (learner_index in seq_along(self$learners)) {
        learner = self$learners[[learner_index]]
        pred = learner$predict(task)
        predictions[[length(predictions) + 1]] = as.data.table(pred)
      }
      response = rowMeans(sapply(predictions, function(x) as.numeric(x$response)))
      response = ifelse(response < 1.5, positive, task$negative)
      prob = as.matrix(data.frame(
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", positive))))),
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", task$negative)))))
      ))
      colnames(prob) = c(task$positive, task$negative)
      pred_uncal = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = response
      )
      pred_uncal = as.data.table(pred_uncal)

      calibration_data = data.table(truth = pred_uncal$truth,
                                    response = with(pred_uncal, get(paste0("prob.", positive))))
      colnames(calibration_data) = c("truth", "response")
      calibration_data$response = as.numeric(calibration_data$response)
      task_for_calibrator = as_task_classif(calibration_data,
                                            target = "truth",
                                            positive = positive,
                                            id = "task_cal")

      pred_calibrated = self$calibrator$predict(task_for_calibrator)

      return(list(pred_calibrated))
    },

    .additional_phash_input = function() {
      list(self$learner$hash, self$calibration_ratio)
    }
  )
)

# Register the new PipeOp
mlr3pipelines::mlr_pipeops$add("calibration_oof", PipeOpCalibrationOOF)
