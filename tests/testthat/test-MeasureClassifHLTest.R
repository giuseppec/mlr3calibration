test_that("MeasureClassifHLTest scores predictions", {
  data = make_binary_prediction_data()
  measure = mlr3::msr("classif.hltest")

  testthat::expect_true(inherits(measure, "MeasureClassifHLTest"))
  testthat::expect_equal(measure$id, "classif.hltest")
  testthat::expect_equal(measure$label, "Hosmer-Lemeshow Test")
  testthat::expect_equal(measure$man, "mlr_measures_classif.hltest")
  testthat::expect_identical(measure$task_properties, "twoclass")
  testthat::expect_identical(measure$param_set$values$bins, 10L)
  testthat::expect_identical(measure$type, "pvalue")
  testthat::expect_equal(measure$range, c(0, 1))
  testthat::expect_identical(measure$minimize, NA)
  testthat::expect_false("type" %in% names(measure$param_set$params))
  checkmate::expect_number(suppressWarnings(data$prediction$score(measure)))
  checkmate::expect_number(suppressWarnings(data$prediction$score(MeasureClassifHLTest$new())))
})

test_that("MeasureClassifHLTest exposes bins through the param_set", {
  predicted = c(0.05, 0.12, 0.22, 0.31, 0.42, 0.53, 0.61, 0.72, 0.84, 0.94, 0.98, 0.99)
  actual = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  prediction = make_manual_binary_prediction(predicted, actual)
  measure_3 = mlr3::msr("classif.hltest", bins = 3L)
  measure_5 = mlr3::msr("classif.hltest", bins = 5L)

  testthat::expect_identical(measure_5$param_set$values$bins, 5L)
  testthat::expect_false(isTRUE(all.equal(prediction$score(measure_3), prediction$score(measure_5))))
})

test_that("MeasureClassifHLTest matches a manually derived three-group statistic", {
  predicted = c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9)
  actual = c(0, 1, 0, 1, 1, 1)
  prediction = make_manual_binary_prediction(predicted, actual)
  measure_fit = mlr3::msr("classif.hltest", bins = 3L, df = "fit")
  measure_validation = mlr3::msr("classif.hltest", bins = 3L, df = "validation")
  measure_stat = mlr3::msr("classif.hltest", type = "statistic", bins = 3L)

  # Groups contain two rows. Expected positive counts are 0.3, 1.0, and 1.7; observed counts are 1, 1, and 2.
  expected_statistic = 0.49 / 0.3 + 0.49 / 1.7 + 0.09 / 1.7 + 0.09 / 0.3
  expected_p_value_fit = stats::pchisq(expected_statistic, df = 1L, lower.tail = FALSE)
  expected_p_value_validation = stats::pchisq(expected_statistic, df = 3L, lower.tail = FALSE)

  testthat::expect_equal(unname(prediction$score(measure_fit)), expected_p_value_fit, tolerance = 1e-12)
  testthat::expect_equal(
    unname(prediction$score(measure_validation)),
    expected_p_value_validation,
    tolerance = 1e-12
  )
  testthat::expect_equal(unname(prediction$score(measure_stat)), expected_statistic, tolerance = 1e-12)
  testthat::expect_identical(measure_stat$type, "statistic")
  testthat::expect_equal(measure_stat$range, c(0, Inf))
  testthat::expect_identical(measure_stat$minimize, TRUE)
  testthat::expect_identical(measure_validation$param_set$values$df, "validation")
})

test_that("MeasureClassifHLTest defaults to validation degrees of freedom", {
  measure = mlr3::msr("classif.hltest")
  testthat::expect_identical(measure$param_set$values$df, "validation")
})

test_that("MeasureClassifHLTest type is constructor-fixed and dictionary-constructible", {
  measure = MeasureClassifHLTest$new(type = "statistic")
  testthat::expect_error(measure$type <- "pvalue", "read-only")
  testthat::expect_error(MeasureClassifHLTest$new(type = "invalid"), "Must be element")
  testthat::expect_identical(mlr3::msr("classif.hltest", type = "pvalue")$type, "pvalue")
  testthat::expect_identical(mlr3::msr("classif.hltest", type = "statistic")$type, "statistic")
  testthat::expect_error(mlr3::msr("classif.hltest", type = "invalid"), "Must be element")
})

test_that("MeasureClassifHLTest hashes include the constructor-fixed type", {
  testthat::expect_false(identical(
    mlr3::msr("classif.hltest", type = "pvalue")$hash,
    mlr3::msr("classif.hltest", type = "statistic")$hash
  ))
})

test_that("MeasureClassifHLTest rejects invalid or degenerate grouping", {
  testthat::expect_error(mlr3::msr("classif.hltest", bins = 2L), "not >=")

  prediction = make_manual_binary_prediction(rep(0.5, 6L), c(0, 1, 0, 1, 0, 1))
  testthat::expect_error(
    suppressWarnings(prediction$score(mlr3::msr("classif.hltest", bins = 3L))),
    "at least three"
  )
})

test_that("MeasureClassifHLTest reports when ties reduce populated groups", {
  prediction = make_manual_binary_prediction(
    rep(c(0.1, 0.5, 0.9), each = 4L),
    c(0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0)
  )

  testthat::expect_warning(
    prediction$score(mlr3::msr("classif.hltest", bins = 10L)),
    "reduced Hosmer-Lemeshow groups"
  )
})

test_that("MeasureClassifHLTest handles exact boundary probabilities by their limiting contribution", {
  prediction = make_manual_binary_prediction(
    c(0, 0, 0.4, 0.6, 1, 1),
    c(0, 0, 0, 1, 1, 1)
  )

  # Every group has observed counts equal to expected counts. Zero-over-zero cells therefore contribute zero.
  testthat::expect_equal(
    unname(prediction$score(mlr3::msr("classif.hltest", bins = 3L))),
    1,
    tolerance = 0
  )
  testthat::expect_equal(
    unname(prediction$score(mlr3::msr("classif.hltest", type = "statistic", bins = 3L))),
    0,
    tolerance = 0
  )
})
