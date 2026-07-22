utils::globalVariables(c("bin", "learner_id", "mean_res", "mean_truth", "res", "truth"))

#' @title Calibration plot
#'
#' @description
#' Plot reliability curves for one or more probabilistic classification learners
#' or predictions.
#'
#' @param learners `list` of [`Learner`][mlr3::Learner]
#'   Trained learners to evaluate.
#'   Use this together with `task`.
#' @param task [`TaskClassif`][mlr3::TaskClassif]
#'   Task used for prediction and evaluation when `learners` is supplied.
#' @param bins `integer(1)`
#'   Number of bins used to aggregate predicted probabilities.
#' @param smooth `logical(1)`
#'   Whether to draw LOESS-smoothed curves instead of bin-wise line segments.
#' @param ci `logical(1)`
#'   Whether to draw confidence intervals for the smoothed curves.
#' @param rug `logical(1)`
#'   Whether to add a rug for the raw predicted probabilities.
#' @param predictions named `list` of [`PredictionClassif`][PredictionClassif]
#'   Predictions to evaluate.
#'   Use the list names as plot labels.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Supply either `learners` with `task`, or a named `predictions` list.
#' For each learner or prediction, the function groups predicted probabilities
#' into `bins`, and compares the mean predicted probability with the empirical
#' event frequency in each bin.
#'
#' @references
#' `r format_bib("niculescu2005")`
#'
#' @family plotting functions
#'
#' @examples
#' if (mlr3misc::require_namespaces("mlbench", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("mlr3learners", quietly = TRUE) &&
#'   mlr3misc::require_namespaces("rpart", quietly = TRUE)) {
#'   data("Sonar", package = "mlbench")
#'   task = mlr3::as_task_classif(Sonar, target = "Class", positive = "M")
#'   learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#'   learner$train(task)
#'   prediction = learner$predict(task)
#'
#'   calibration_plot(list(learner), task, smooth = TRUE)
#'   calibration_plot(predictions = list(rpart = prediction), smooth = TRUE)
#' }
#'
#' @export
calibration_plot = function(
  learners = NULL,
  task = NULL,
  bins = 10L,
  smooth = FALSE,
  ci = FALSE,
  rug = FALSE,
  predictions = NULL
) {
  assert_int(bins, lower = 1L)
  assert_flag(smooth)
  assert_flag(ci)
  assert_flag(rug)

  predictions = calibration_plot_predictions(learners = learners, task = task, predictions = predictions)
  grouped_predictions = lapply(names(predictions), function(id) {
    calibration_plot_prediction_data(
      prediction = predictions[[id]],
      label = id,
      bins = bins
    )
  })

  all_raw_data = rbindlist(lapply(grouped_predictions, function(prediction) prediction$raw))
  all_data = rbindlist(lapply(grouped_predictions, function(prediction) prediction$aggregated))
  perfect = data.table(mean_res = c(0, 1), mean_truth = c(0, 1))

  plot = ggplot() +
    geom_line(
      data = perfect,
      mapping = aes(x = mean_res, y = mean_truth),
      linewidth = 0.8,
      linetype = "dashed",
      color = "grey40"
    ) +
    labs(
      x = "Mean predicted probability",
      y = "Empirical event rate",
      color = "Learner"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    ggtitle("Reliability curve") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.background = element_rect(color = "black", linewidth = 0.5)
    )

  if (smooth) {
    plot = plot + geom_smooth(
      data = all_data,
      mapping = aes(x = mean_res, y = mean_truth, color = learner_id),
      method = "loess",
      formula = y ~ x,
      se = ci
    )
  } else {
    plot = plot +
      geom_line(
        data = all_data,
        mapping = aes(x = mean_res, y = mean_truth, color = learner_id)
      ) +
      geom_point(
        data = all_data,
        mapping = aes(x = mean_res, y = mean_truth, color = learner_id)
      )
  }

  if (rug) {
    plot = plot + geom_rug(
      data = all_raw_data,
      mapping = aes(x = res, color = learner_id),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.2,
      show.legend = FALSE
    )
  }

  plot
}

calibration_plot_predictions = function(learners = NULL, task = NULL, predictions = NULL) {
  if (!is.null(predictions)) {
    if (!is.null(learners) || !is.null(task)) {
      cli_abort("Supply either `learners` with `task`, or `predictions`.")
    }

    assert_list(predictions, min.len = 1L, any.missing = FALSE, names = "unique")

    if (any(names(predictions) == "")) {
      cli_abort("`predictions` must be a named list.")
    }

    for (prediction in predictions) {
      assert_r6(prediction, classes = "PredictionClassif")

      if (is.null(prediction$prob) || ncol(prediction$prob) != 2L) {
        cli_abort("calibration_plot() currently supports binary classification predictions only.")
      }
    }

    return(predictions)
  }

  assert_list(learners, min.len = 1L, any.missing = FALSE)
  assert_r6(task, classes = "TaskClassif")

  if (length(task$class_names) != 2L) {
    cli_abort("calibration_plot() currently supports binary classification tasks only.")
  }

  for (learner in learners) {
    assert_r6(learner, classes = "Learner")
  }

  labels = make.unique(vapply(learners, function(learner) learner$id, character(1L)))
  predictions = setNames(
    lapply(learners, function(learner) learner$predict(task)),
    labels
  )

  if (any(vapply(predictions, function(prediction) is.null(prediction$prob), logical(1L)))) {
    cli_abort("calibration_plot() requires probability predictions; set learner predict_type to 'prob'.")
  }

  predictions
}

calibration_plot_prediction_data = function(prediction, label, bins) {
  inputs = binary_prediction_inputs(prediction)
  data = data.frame(
    res = as.numeric(inputs$predicted),
    truth = inputs$actual,
    learner_id = label,
    stringsAsFactors = FALSE
  )
  data = data[order(data$res), , drop = FALSE]
  data$bin = cut(
    data$res,
    breaks = seq(0, 1, length.out = bins + 1L),
    include.lowest = TRUE
  )

  list(
    raw = as.data.table(data[c("res", "truth", "learner_id")]),
    aggregated = as.data.table(aggregate(
      cbind(mean_res = res, mean_truth = truth) ~ bin + learner_id,
      data = data,
      FUN = mean
    ))
  )
}
