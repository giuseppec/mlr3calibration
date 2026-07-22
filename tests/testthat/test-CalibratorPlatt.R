test_that("platt_targets match Platt (2000) Laplace smoothing", {
  y = c(1L, 1L, 0L, 0L, 0L)
  targets = platt_targets(y)
  testthat::expect_equal(targets[y == 1L], rep((2 + 1) / (2 + 2), 2L))
  testthat::expect_equal(targets[y == 0L], rep(1 / (3 + 2), 3L))
})

test_that("CalibratorPlatt defaults use soft targets and raw probabilities", {
  calibrator = clb("platt")
  testthat::expect_true(calibrator$param_set$values$label_smoothing)
  testthat::expect_identical(calibrator$param_set$values$input, "probabilities")
})

test_that("CalibratorPlatt hard-label mode differs from default soft targets", {
  set.seed(1L)
  n = 40L
  truth = factor(sample(c("a", "b"), n, replace = TRUE), levels = c("a", "b"))
  prob = cbind(a = runif(n), b = NA_real_)
  prob[, "b"] = 1 - prob[, "a"]

  soft = clb("platt", label_smoothing = TRUE, input = "probabilities")
  hard = clb("platt", label_smoothing = FALSE, input = "probabilities")
  soft$train(truth, prob, colnames(prob))
  hard$train(truth, prob, colnames(prob))

  pred_soft = soft$predict(prob, colnames(prob))
  pred_hard = hard$predict(prob, colnames(prob))
  testthat::expect_false(isTRUE(all.equal(pred_soft, pred_hard)))
})

test_that("CalibratorPlatt logit input uses clipped log-odds", {
  set.seed(2L)
  n = 30L
  truth = factor(rep(c("a", "b"), length.out = n), levels = c("a", "b"))
  p = pmin(pmax(seq(0.05, 0.95, length.out = n), 1e-3), 1 - 1e-3)
  prob = cbind(a = p, b = 1 - p)

  calibrator = clb("platt", input = "logit")
  calibrator$train(truth, prob, colnames(prob))
  calibrated = calibrator$predict(prob, colnames(prob))

  checkmate::expect_matrix(calibrated, nrows = n, ncols = 2L)
  testthat::expect_true(all(abs(rowSums(calibrated) - 1) < 1e-8))
  testthat::expect_identical(calibrator$param_set$values$input, "logit")
})

test_that("platt_predictor respects input mode", {
  p = c(0.2, 0.8)
  testthat::expect_equal(platt_predictor(p, input = "probabilities"), p)
  testthat::expect_equal(
    platt_predictor(p, input = "logit"),
    log(p / (1 - p))
  )
})
