#' @title Cox Calibration Slope and Intercept
#'
#' @description
#' A logistic regression model with log-odds as the independent variable and the outcome
#' as the dependent variable is fitted. Perfect calibration is indicated by a slope of 1 and intercept of 0.
#' Slope >1 indicates overconfidence at high probabilities and underconfidence at low probabilities, while a slope <1 indicates the opposite.
#' A positive intercept indicates general overconfidence.
#'
#' @references
#' Cox, D. R. (1958)
#'
#' @examples
#' # Example usage
#' set.seed(1)
#'
#' # Load the task
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#'
#' # Initialize the base learner
#' learner_uncal <- lrn("classif.ranger", predict_type = "prob")
#'
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(PipeOpCalibrationPerFold$new(learner = learner_uncal,
#' method = "platt", rsmp = rsmp))
#'
#' # Set ID's for the learners
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the calibrated learner
#' learner_cal$train(task_train)
#'
#' # Predict the learner
#' prediction <- learner_cal$predict(task_test)
#'
#' # Calculate the ECE
#' cox <- prediction$score(cox$new())
#'
#' @export
cox_slope = R6::R6Class("cox_slope",
                  inherit = mlr3::MeasureClassif,
                  public = list(
                    #' @description
                    #' Creates a new `cox_slope` object.
                    initialize = function() {
                      super$initialize(
                        id = "classif.cox",
                        packages = "CalibratR",
                        properties = character(),
                        predict_type = "prob",
                        range = c(-Inf, Inf),
                        minimize = TRUE
                      )
                    }
                  ),
                  private = list(
                    .score = function(prediction, ...) {
                      actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
                      predicted = prediction$prob[, 1]

                      # clip probabilities to prevent numerical instabilities
                      epsilon = 1e-7
                      proba <- pmin(pmax(predicted, epsilon), 1 - epsilon)

                      # Create design matrix with intercept
                      logit <- log(proba / (1 - proba))
                      X <- cbind(1, logit)  # Adds a constant (intercept) term

                      # Fit logistic regression model
                      logit_model <- glm(actual ~ logit, family = binomial)

                      # Extract coefficients and confidence intervals
                      coefs <- coef(logit_model)
                      conf_int <- confint(logit_model)

                      return(coefs[2])
                    }
                  )
)

mlr3::mlr_measures$add("classif.cox_slope", cox_slope)


cox_intercept = R6::R6Class("cox_intercept",
                        inherit = mlr3::MeasureClassif,
                        public = list(
                          #' @description
                          #' Creates a new `cox_intercept` object.
                          initialize = function() {
                            super$initialize(
                              id = "classif.cox",
                              packages = "CalibratR",
                              properties = character(),
                              predict_type = "prob",
                              range = c(-Inf, Inf),
                              minimize = TRUE
                            )
                          }
                        ),
                        private = list(
                          .score = function(prediction, ...) {
                            actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
                            predicted = prediction$prob[, 1]

                            # clip probabilities to prevent numerical instabilities
                            epsilon = 1e-7
                            proba <- pmin(pmax(predicted, epsilon), 1 - epsilon)

                            # Create design matrix with intercept
                            logit <- log(proba / (1 - proba))
                            X <- cbind(1, logit)  # Adds a constant (intercept) term

                            # Fit logistic regression model
                            logit_model <- glm(actual ~ logit, family = binomial)

                            # Extract coefficients and confidence intervals
                            coefs <- coef(logit_model)
                            conf_int <- confint(logit_model)

                            return(coefs[1])
                          }
                        )
)

mlr3::mlr_measures$add("classif.cox_intercept", cox_intercept)
