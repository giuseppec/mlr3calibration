assert_probability_learner = function(learner) {
  assert_r6(learner, classes = "Learner")

  if (learner$predict_type != "prob") {
    cli_abort("predict_type has to be 'prob'")
  }

  invisible(learner)
}

coerce_calibration_learner = function(learner) {
  if (inherits(learner, "Graph")) {
    learner = as_learner(learner)
  }

  assert_r6(learner, classes = "Learner")
  learner
}

# Resamplings are deep-cloned on purpose, stricter than AutoTuner's shallow clone:
# the stored object must not alias the caller's param_set or instance.
calibration_pipeop_resampling = function(resampling = NULL, rr = NULL) {
  if (!is.null(rr)) {
    assert_r6(rr, classes = "ResampleResult")

    if (!is.null(resampling)) {
      cli_abort("'resampling' and 'rr' cannot be supplied together.")
    }

    return(rr$resampling$clone(deep = TRUE))
  }

  if (is.null(resampling)) {
    return(rsmp("cv", folds = 5L))
  }

  assert_r6(resampling, classes = "Resampling")
  resampling$clone(deep = TRUE)
}

# Resampling$hash is NA_character_ until instantiation, so hash the configuration
# components explicitly to keep the PipeOp phash sensitive to the resampling setup.
resampling_phash_input = function(resampling) {
  assert_r6(resampling, classes = "Resampling")

  list(
    class = class(resampling),
    id = resampling$id,
    param_values = resampling$param_set$values,
    instance = resampling$instance
  )
}
