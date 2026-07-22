# Shared validator for every binned calibration measure.
# Requires nonempty, equal-length, finite vectors, binary `actual` in {0, 1}
# (without requiring both outcomes), `predicted` in [0, 1], and an integer bin/range count >= 1.
assert_calibration_vectors = function(actual, predicted, bins) {
  assert_numeric(actual, min.len = 1L, any.missing = FALSE, finite = TRUE)
  assert_numeric(predicted, len = length(actual), any.missing = FALSE, finite = TRUE)
  assert_int(bins, lower = 1L)

  if (!all(actual %in% c(0, 1))) {
    cli_abort("actual must contain binary outcomes encoded as 0 and 1.")
  }

  if (any(predicted < 0 | predicted > 1)) {
    cli_abort("predicted probabilities must be between 0 and 1.")
  }

  invisible(TRUE)
}

# Equal-width bin components for one-vs-rest calibration.
# The right-closed intervals ((b - 1) / B, b / B] follow Guo et al. (2017) and Kull et al. (2019).
# `include.lowest = TRUE` is a package edge convention that places an exact 0 in the first bin.
# Empty bins are skipped: they carry zero empirical weight and must not be merged or renormalized.
equal_width_bin_components = function(actual, predicted, bins) {
  breaks = seq(0, 1, length.out = bins + 1L)
  bin_id = cut(predicted, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)

  populated = sort(unique(bin_id))
  count = tabulate(bin_id, nbins = bins)[populated]
  mean_actual = as.numeric(tapply(actual, bin_id, mean))
  mean_predicted = as.numeric(tapply(predicted, bin_id, mean))

  list(
    bin = populated,
    n = count,
    acc = mean_actual,
    conf = mean_predicted,
    gap = abs(mean_actual - mean_predicted)
  )
}

# Equal-width single-class ECE component: sum_b (n_b / N) * |acc_b - conf_b|.
# This is the class-j component of Kull et al. (2019), Eq. 4.
equal_width_calibration_error = function(actual, predicted, bins) {
  assert_calibration_vectors(actual, predicted, bins)
  components = equal_width_bin_components(actual, predicted, bins)
  sum(components$n / length(predicted) * components$gap)
}

# Adaptive equal-count range gaps for Nixon et al. (2019) ACE/TACE.
# Ranges are weighted equally, not by observed size. `row_ids` must be non-missing,
# and sorting by (predicted, row_id) makes the paper's unspecified tie break deterministic and
# independent of incoming row order. The first `N %% R` ranges receive `ceiling(N / R)` observations
# and the remaining ranges receive `floor(N / R)`; both conventions are package choices. Returns the
# unweighted mean of the `R` absolute gaps.
adaptive_range_gaps = function(actual, predicted, row_ids, ranges) {
  assert_calibration_vectors(actual, predicted, ranges)
  assert_atomic_vector(row_ids, len = length(actual), any.missing = FALSE)

  n = length(predicted)

  if (n < ranges) {
    msg = sprintf(
      "adaptive calibration error requires at least %d observations for %d ranges but received %d.",
      ranges,
      ranges,
      n
    )
    cli_abort("{msg}")
  }

  index = order(predicted, row_ids)
  actual = actual[index]
  predicted = predicted[index]
  rest = n %% ranges
  sizes = c(rep.int(ceiling(n / ranges), rest), rep.int(floor(n / ranges), ranges - rest))
  range_id = rep.int(seq_len(ranges), sizes)

  gaps = vapply(
    seq_len(ranges),
    function(r) {
      selected = range_id == r
      abs(mean(actual[selected]) - mean(predicted[selected]))
    },
    numeric(1L)
  )

  mean(gaps)
}

# Macro average of adaptive classwise calibration error (ACE / TACE).
# When `threshold` is set, each class is filtered with strict `>` before forming ranges.
classwise_adaptive_calibration_error = function(prediction, ranges, threshold = NULL) {
  prob = prediction_probability_matrix(prediction)
  truth = as.character(prediction$truth)
  row_ids = prediction$row_ids

  per_class = vapply(
    colnames(prob),
    function(k) {
      actual = as.integer(truth == k)
      predicted = prob[, k]
      if (!is.null(threshold)) {
        keep = predicted > threshold
        if (sum(keep) < ranges) {
          msg = sprintf(
            "TACE class '%s' retained only %d observations above threshold %g but %d ranges were requested.",
            k,
            sum(keep),
            threshold,
            ranges
          )
          cli_abort("{msg}")
        }
        actual = actual[keep]
        predicted = predicted[keep]
        row_ids_k = row_ids[keep]
      } else {
        row_ids_k = row_ids
      }
      adaptive_range_gaps(actual, predicted, row_ids = row_ids_k, ranges = ranges)
    },
    numeric(1L)
  )

  mean(per_class)
}

# Common per-observation top-label summaries shared by confidence-ECE and top-label-ECE.
# `max.col(prob, ties.method = "first")` breaks a top-probability tie in favor of the first
# class in the probability-column order, a deterministic package convention.
prediction_top_label = function(prediction) {
  prob = prediction_probability_matrix(prediction)
  index = max.col(prob, ties.method = "first")
  labels = colnames(prob)[index]

  list(
    prob = prob,
    truth = as.character(prediction$truth),
    label = labels,
    confidence = prob[cbind(seq_len(nrow(prob)), index)]
  )
}

cox_logit_model = function(actual, predicted, epsilon = 1e-7) {
  assert_numeric(actual, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(predicted, any.missing = FALSE, null.ok = FALSE)
  assert_number(epsilon, lower = 0, upper = 1)

  prob = pmin(pmax(predicted, epsilon), 1 - epsilon)
  logit = log(prob / (1 - prob))
  glm(actual ~ logit, family = binomial())
}
