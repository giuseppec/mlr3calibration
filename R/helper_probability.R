binary_probability_matrix = function(prob_pos, class_names) {
  assert_numeric(prob_pos, any.missing = FALSE, null.ok = FALSE)
  assert_character(class_names, len = 2L, any.missing = FALSE, unique = TRUE)

  prob = cbind(as.numeric(prob_pos), 1 - as.numeric(prob_pos))
  colnames(prob) = class_names
  prob
}

normalize_probability_matrix = function(prob, class_names = NULL) {
  tolerance = 1e-12

  if (is.null(dim(prob))) {
    assert_numeric(prob, any.missing = FALSE, null.ok = FALSE)
    assert_character(class_names, len = 2L, any.missing = FALSE, unique = TRUE)
    prob = binary_probability_matrix(prob_pos = prob, class_names = class_names)
  } else {
    prob = as.matrix(prob)

    if (is.null(class_names)) {
      class_names = colnames(prob)
    }
  }

  assert_character(class_names, min.len = 2L, any.missing = FALSE, unique = TRUE)

  if (is.null(colnames(prob))) {
    if (ncol(prob) != length(class_names)) {
      cli_abort("Probability matrix does not match the provided class names.")
    }
    colnames(prob) = class_names
  } else {
    if (anyNA(colnames(prob)) || anyDuplicated(colnames(prob)) != 0L) {
      cli_abort("Probability matrix column names must be non-missing and unique.")
    }
    missing_classes = setdiff(class_names, colnames(prob))
    extra_classes = setdiff(colnames(prob), class_names)

    if (length(missing_classes) > 0L) {
      cli_abort("Probability matrix columns do not cover the provided class names.")
    }
    if (length(extra_classes) > 0L) {
      cli_abort("Probability matrix contains columns that are not present in the provided class names.")
    }
    prob = prob[, class_names, drop = FALSE]
  }

  storage.mode(prob) = "double"

  if (any(!is.finite(prob))) {
    cli_abort("Probability values must be finite.")
  }

  if (any(prob < -tolerance | prob > 1 + tolerance)) {
    cli_abort("Probability values must be between 0 and 1.")
  }

  prob = pmin(pmax(prob, 0), 1)

  row_sums = rowSums(prob)
  zero_rows = row_sums <= 0

  if (any(zero_rows)) {
    cli_abort("Probability rows must have positive row sums.")
  }

  prob = prob / row_sums

  colnames(prob) = class_names
  prob
}

prediction_probability_matrix = function(prediction, class_names = NULL) {
  assert_r6(prediction, classes = "PredictionClassif")
  normalize_probability_matrix(prediction$prob, class_names = class_names)
}

binary_prediction_inputs = function(prediction) {
  assert_r6(prediction, classes = "PredictionClassif")

  if (is.null(prediction$prob) || ncol(prediction$prob) != 2L) {
    cli_abort("This operation currently supports binary classification predictions only.")
  }

  # Positive-class contract for binary measures and calibrators:
  # the first probability column is treated as the positive event.
  # For task-derived mlr3 predictions, that first column is expected to be task$positive.
  # Do not reorder columns here and do not infer the positive class from factor level order.
  positive = colnames(prediction$prob)[1L]

  list(
    actual = as.integer(prediction$truth == positive),
    predicted = prediction$prob[, 1L],
    positive = positive
  )
}
