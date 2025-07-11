#' @title Tuning before Calibration Pipeline Operator
#'
#' @description
#' This R6 class was designed to receive a learner of the type auto_tuner and
#' tune this learner before calibrating it.
#'
#' @param learner [`Learner`][mlr3::Learner]\cr auto tuner learner to be calibrated. predict_type has to be `"prob"`.
#' @param rr [`ResampleResult`][mlr3::ResampleResult]\cr Resample result object, either that or learner has to be provided.
#' @param method `character(1)`\cr Calibration method to use. One of `"platt"`, `"isotonic"`, or `"beta"`. Default is `"platt"`.
#' @param rsmp [`Resampling`][mlr3::Resampling]\cr Resampling strategy for cross-validation. Default is `rsmp("cv", folds = 5)`.
#' @param parameters `character(1)`\cr Parameters for beta calibration. Default is `"abm"`.
#' @param param_vals `list`\cr param_vals, copied from base learner
#'
#' @field learner [`Learner`][mlr3::Learner]\cr Base learner to be calibrated.
#' @field method `character(1)`\cr Calibration method used.
#' @field rsmp [`Resampling`][mlr3::Resampling]\cr Resampling strategy.
#' @field learners `list`\cr List of learners obtained from resampling.
#' @field calibrators `list`\cr List of calibrator models.
#' @field rr [`ResampleResult`][mlr3::ResampleResult]\cr Resample result object.
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
#' at <- auto_tuner(tuner = tnr("random_search"),
#'  learner = learner_uncal,
#'  resampling = rsmp("cv", folds = 2),
#'  measure = msr("classif.bbrier"),
#'  term_evals = 10)
#' learner_cal <- as_learner(PipeOpCalibrationTuneFirst$new(
#'  learner = at, rsmp = rsmp, method = "platt"))
#'
#' # Set ID's for the learners
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the calibrated learner
#' learner_cal$train(task)
#' learner_cal$predict(task)$score(msr("classif.bbrier"))
#' @export

PipeOpCalibrationTuneFirst <- R6::R6Class(
  "PipeOpCalibrationTuneFirst",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner = NULL,
    method = NULL,
    rsmp = NULL, # rename in resampling
    learners = NULL,
    calibrators = NULL,
    rr = NULL,
    parameters = NULL,

    #' @description
    #' Creates a new `PipeOpCalibrationPerFold` object.
    #' @param learner Base learner to be calibrated. predict_type has to be `"prob"`.
    #' @param rr Resample result object, if provided.
    #' @param method Calibration method to use. One of `"platt"`, `"isotonic"`, or `"beta"`. Default is `"platt"`.
    #' @param rsmp Resampling strategy for cross-validation. Default is `rsmp("cv", folds = 5)`.
    #' @param parameters Parameters for beta calibration. Default is `"abm"`.
    #' @param param_vals param_vals, copied from base learner
    initialize = function(#id = "Calibrated",
      learner = NULL,
      method = "platt",
      rsmp = NULL,
      rr = NULL,
      parameters = NULL,
      param_vals = list()) {

      if (is.null(learner) && is.null(rr)) {
        stop("Either learner or rr object must be provided.")
      }
      if (!is.null(rr)) {
        self$rr = rr
      }
      if (!is.null(learner)) {
        self$learner = learner$clone()
        id = self$learner$base_learner()$id
      }else{
        self$learner = self$rr$learners[[1]]$clone()
      }
      if (!is.null(rsmp)) {
        self$rsmp = rsmp
      }else if (is.null(rsmp) && is.null(rr)){
        self$rsmp = rsmp("cv", folds = 5)
      }
      if (self$learner$predict_type != "prob"){
        stop("predict_type has to be 'prob'")
      }
      self$method = method
      self$parameters = parameters
      self$learners = list()
      self$calibrators = list()
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
      "prob"
    }
  ),
  private = list(
    .train = function(inputs) {
      on.exit(lgr::get_logger("mlr3")$set_threshold("info"))

      lgr::get_logger("mlr3")$set_threshold("warn")

      task = inputs[[1]]
      positive = task$positive

      at = self$learner$clone()

      at$train(task)

      if(is.null(self$rr)){
        #rr = resample(task, at$learner, self$rsmp, store_models = TRUE)
        # TODO is this correct?
        rr = resample(task, at, self$rsmp, store_models = TRUE)
      }else{
        rr = self$rr
      }

      self$learners = rr$learners

      preds = rr$predictions(predict_sets = "test")

      for (pred in preds) {
        pred_data = as.data.table(pred)
        calibration_data = data.table(truth = pred_data$truth,
          response = with(pred_data, get(paste0("prob.", positive))))
        colnames(calibration_data) = c("truth", "response")
        calibration_data$response = as.numeric(calibration_data$response)
        task_for_calibrator = as_task_classif(calibration_data,
                                              target = "truth",
                                              positive = positive,
                                              id = "task_cal")


        if(is.null(self$parameters)){
          calibrator <- clb(self$method)
        }else{
          calibrator <- clb(self$method, self$parameters)
        }

        calibrator$train(task_for_calibrator)
        self$calibrators[[length(self$calibrators) + 1]] = calibrator

      }
      return(list(NULL))
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      predictions = list()
      # Loop over learners using their indices
      for (learner_index in seq_along(self$learners)) {
        learner = self$learners[[learner_index]]
        pred = learner$predict(task)
        pred_data = as.data.table(pred)
        calibration_data = data.table(truth = task$truth(),
          response = with(pred_data, get(paste0("prob.", positive))))
        colnames(calibration_data) = c("truth", "response")
        calibration_data$response = as.numeric(calibration_data$response)
        task_for_calibrator = as_task_classif(calibration_data,
                                              target = "truth",
                                              positive = positive,
                                              id = "task_cal")

        pred_calibrated = self$calibrators[[learner_index]]$predict(
          task_for_calibrator)

        predictions[[length(predictions) + 1]] = as.data.table(pred_calibrated)
      }

      response = rowMeans(sapply(predictions, function(x) as.numeric(x$response)))
      response = ifelse(response < 1.5, positive, task$negative)

      prob = as.matrix(data.frame(
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", positive))))),
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", task$negative)))))
      ))
      colnames(prob) = c(task$positive, task$negative)
      pred_calibrated = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = response
      )
      return(list(pred_calibrated))
    },

    .additional_phash_input = function() {
      list(self$learner$hash)
    }
  )
)

# Register the new PipeOp
mlr3pipelines::mlr_pipeops$add("calibration_tune_first", PipeOpCalibrationTuneFirst)
