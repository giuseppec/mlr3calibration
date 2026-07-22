test_that("assert_calibration_vectors enforces the shared contract", {
  testthat::expect_error(assert_calibration_vectors(numeric(0L), numeric(0L), 5L), "length")
  testthat::expect_error(assert_calibration_vectors(c(0, 1), c(0.2), 5L), "length")
  testthat::expect_error(assert_calibration_vectors(c(0, NA), c(0.2, 0.3), 5L), "missing")
  testthat::expect_error(assert_calibration_vectors(c(0, 1), c(0.2, Inf), 5L), "finite|missing")
  testthat::expect_error(assert_calibration_vectors(c(0, 2), c(0.2, 0.3), 5L), "binary outcomes")
  testthat::expect_error(assert_calibration_vectors(c(0, 1), c(-0.1, 0.3), 5L), "between 0 and 1")
  testthat::expect_error(assert_calibration_vectors(c(0, 1), c(0.2, 0.3), 0L), "bins")
  # A single outcome value is allowed for a bin calculation.
  testthat::expect_true(assert_calibration_vectors(c(1, 1), c(0.2, 0.3), 5L))
})

test_that("equal_width_bin_components uses right-closed bins, keeps empties out, and allows bins > N", {
  # Two bins: (0, 0.5] and (0.5, 1]; 0 joins the first bin, 1 joins the last.
  components = equal_width_bin_components(
    actual = c(1, 0, 1, 0),
    predicted = c(0, 0.5, 0.75, 1),
    bins = 2L
  )

  testthat::expect_identical(components$bin, c(1L, 2L))
  testthat::expect_identical(components$n, c(2L, 2L))
  testthat::expect_equal(components$conf, c(0.25, 0.875))

  # An interior empty bin is skipped and never renormalizes populated-bin weights.
  sparse = equal_width_bin_components(c(1, 0), c(0.05, 0.95), bins = 10L)
  testthat::expect_identical(sparse$bin, c(1L, 10L))
  testthat::expect_identical(sparse$n, c(1L, 1L))

  # bins > N leaves most bins empty; only populated bins contribute.
  many_bins = equal_width_bin_components(c(1, 0, 1), c(0.1, 0.4, 0.9), bins = 20L)
  testthat::expect_identical(many_bins$n, c(1L, 1L, 1L))
})

test_that("equal_width_calibration_error uses equal-width bins", {
  set.seed(42)
  predicted = round(runif(37L), 3L)
  actual = as.integer(runif(37L) < predicted)

  testthat::expect_equal(
    equal_width_calibration_error(actual, predicted, bins = 10L),
    ref_equal_width_ece(actual, predicted, 10L)
  )
})

test_that("adaptive_range_gaps balances ranges and stays invariant to incoming row order", {
  actual = c(0, 1, 0, 1, 0, 1, 0)
  predicted = c(0.05, 0.15, 0.25, 0.45, 0.55, 0.75, 0.95)
  row_ids = 1:7

  # N = 7, R = 3 -> sizes 3, 2, 2; every observation is assigned exactly once.
  testthat::expect_equal(
    adaptive_range_gaps(actual, predicted, row_ids, ranges = 3L),
    ref_adaptive_gaps(actual, predicted, row_ids, 3L)
  )

  # Reordering rows while preserving row IDs does not change the result.
  perm = c(5L, 1L, 7L, 3L, 2L, 6L, 4L)
  testthat::expect_equal(
    adaptive_range_gaps(actual[perm], predicted[perm], row_ids[perm], ranges = 3L),
    adaptive_range_gaps(actual, predicted, row_ids, ranges = 3L)
  )

  # Divisible counts split evenly.
  testthat::expect_equal(
    adaptive_range_gaps(c(0, 1, 0, 1), c(0.1, 0.4, 0.6, 0.9), 1:4, ranges = 2L),
    0.25
  )
})

test_that("adaptive_range_gaps uses equal range weights, not observed counts", {
  # Ranges 1 and 2 have different sizes but each contributes with weight 1 / R.
  actual = c(0, 0, 1, 1, 1)
  predicted = c(0.1, 0.2, 0.3, 0.8, 0.9)
  row_ids = 1:5
  # R = 2 -> sizes 3, 2. Range1 {0.1,0.2,0.3} acc 1/3 conf 0.2 gap 2/15; Range2 {0.8,0.9} acc 1 conf 0.85 gap 0.15.
  expected = mean(c(abs(1 / 3 - 0.2), abs(1 - 0.85)))
  testthat::expect_equal(adaptive_range_gaps(actual, predicted, row_ids, ranges = 2L), expected)
})

test_that("adaptive_range_gaps rejects too few observations and accepts repeated row IDs", {
  testthat::expect_error(
    adaptive_range_gaps(c(0, 1), c(0.2, 0.8), 1:2, ranges = 3L),
    "at least 3 observations"
  )
  testthat::expect_equal(
    adaptive_range_gaps(c(0, 1, 0), c(0.2, 0.5, 0.8), c(1L, 1L, 2L), ranges = 2L),
    mean(c(abs(0.5 - 0.35), abs(0 - 0.8)))
  )
})
