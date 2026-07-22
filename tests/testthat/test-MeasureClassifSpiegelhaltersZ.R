test_that("MeasureClassifSpiegelhaltersZ scores predictions", {
  data = make_binary_prediction_data()
  measure = mlr3::msr("classif.spiegelhaltersz")

  testthat::expect_true(inherits(measure, "MeasureClassifSpiegelhaltersZ"))
  testthat::expect_equal(measure$id, "classif.spiegelhaltersz")
  testthat::expect_equal(measure$label, "Spiegelhalter's Z")
  testthat::expect_equal(measure$man, "mlr_measures_classif.spiegelhaltersz")
  testthat::expect_identical(measure$task_properties, "twoclass")
  testthat::expect_identical(measure$type, "pvalue")
  testthat::expect_equal(measure$range, c(0, 1))
  testthat::expect_identical(measure$minimize, NA)
  testthat::expect_false("type" %in% names(measure$param_set$params))
  checkmate::expect_number(data$prediction$score(measure))
  checkmate::expect_number(data$prediction$score(MeasureClassifSpiegelhaltersZ$new()))
})

test_that("MeasureClassifSpiegelhaltersZ matches a manually derived signed statistic", {
  predicted = c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9)
  actual = c(0, 1, 0, 1, 1, 1)
  prediction = make_manual_binary_prediction(predicted, actual)
  measure = mlr3::msr("classif.spiegelhaltersz")
  measure_stat = mlr3::msr("classif.spiegelhaltersz", type = "statistic")

  numerator = sum((actual - predicted) * (1 - 2 * predicted))
  denominator = sqrt(sum((1 - 2 * predicted)^2 * predicted * (1 - predicted)))
  expected_z = numerator / denominator
  expected_p = 2 * (1 - stats::pnorm(abs(expected_z)))

  testthat::expect_equal(unname(prediction$score(measure)), expected_p, tolerance = 1e-12)
  testthat::expect_equal(unname(prediction$score(measure_stat)), expected_z, tolerance = 1e-12)
  testthat::expect_identical(measure_stat$type, "statistic")
  testthat::expect_equal(measure_stat$range, c(-Inf, Inf))
  testthat::expect_identical(measure_stat$minimize, NA)
})

test_that("MeasureClassifSpiegelhaltersZ type is constructor-fixed and dictionary-constructible", {
  measure = MeasureClassifSpiegelhaltersZ$new(type = "statistic")
  testthat::expect_error(measure$type <- "pvalue", "read-only")
  testthat::expect_error(MeasureClassifSpiegelhaltersZ$new(type = "invalid"), "Must be element")
  testthat::expect_identical(mlr3::msr("classif.spiegelhaltersz", type = "pvalue")$type, "pvalue")
  testthat::expect_identical(mlr3::msr("classif.spiegelhaltersz", type = "statistic")$type, "statistic")
  testthat::expect_error(mlr3::msr("classif.spiegelhaltersz", type = "invalid"), "Must be element")
})

test_that("MeasureClassifSpiegelhaltersZ hashes include the constructor-fixed type", {
  testthat::expect_false(identical(
    mlr3::msr("classif.spiegelhaltersz", type = "pvalue")$hash,
    mlr3::msr("classif.spiegelhaltersz", type = "statistic")$hash
  ))
})

test_that("MeasureClassifSpiegelhaltersZ returns NaN for a zero variance denominator", {
  prediction = make_manual_binary_prediction(rep(0.5, 6L), c(0, 1, 0, 1, 0, 1))

  testthat::expect_identical(unname(prediction$score(mlr3::msr("classif.spiegelhaltersz"))), NaN)
  testthat::expect_identical(
    unname(prediction$score(mlr3::msr("classif.spiegelhaltersz", type = "statistic"))),
    NaN
  )
})
