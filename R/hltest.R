#' @title Hosmer Lemeshow Test
#'
#' @description
#' Tests the goodness of calibration for a binary classifier by comparing expected and observed frequencies in data bins.
#' A small p-value indicates that the model is poorly calibrated to the data.
#'
#' @references
#' Hosmer, D, and Lemeshow S (2000). Applied Logistic Regression. 2. ed. New York [u.a.]: Wiley.
#'
#' @examples
#' # Example usage
#' set.seed(1)
#' library(mlr3verse)
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#' learner <- lrn("classif.rpart", predict_type = "prob")
#' learner$train(task_train)
#' preds = learner$predict(task_test)
#' score_hl = preds$score(hltest$new())
#'
#' @export
hltest = R6::R6Class("hltest",
                  inherit = mlr3::MeasureClassif,
                  public = list(
                    #' @description
                    #' Creates a new `hltest` object.
                    initialize = function() {
                      super$initialize(
                        id = "classif.hltest",
                        packages = "CalibratR",
                        properties = character(),
                        predict_type = "prob",
                        range = c(0, 1),
                        minimize = FALSE
                      )
                    }
                  ),
                  private = list(
                    .score = function(prediction, bins = 10, ...) {
                      actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
                      predicted = prediction$prob[, 1]

                      data <- data.frame(predicted, actual)
                      data <- data[order(data$predicted), ]
                      data$bin <- cut(data$predicted, breaks = seq(0, 1, length.out = bins + 1),
                                      include.lowest = TRUE)
                      data <- data %>% group_by(bin) %>% summarise(mean_predicted = mean(predicted),
                                                                   mean_actual = mean(actual), bin_count = sum(!is.na(actual)))

                      # Compute the Hosmer-Lemeshow statistic
                      data$chi_squared = (data$mean_actual - data$mean_predicted)**2 / (data$mean_predicted*(1-data$mean_predicted/data$bin_count))
                      chi_squared = sum(data$chi_squared)
                      # df
                      df = nrow(data)-2
                      # p-value
                      p_value <- 1 - pchisq(chi_squared, df)
                      return(p_value)
                    }
                  )
)

mlr3::mlr_measures$add("classif.hltest", hltest)
