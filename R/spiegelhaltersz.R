#' @title Spiegelhalters Z-Statistics
#'
#' @description
#' Spiegelhalters test compares predicted probabilities to observed outcomes and tests if the model is well calibrated.
#' A small p-value (<0.05) indicates bad calibration.
#'
#' @references
#' Spiegelhalter, D. (1986)
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
#' # Calculate p-value
#' cox <- prediction$score(spiegelhaltersz$new())
#'
#' @export
spiegelhaltersz = R6::R6Class("spiegelhaltersz",
                        inherit = mlr3::MeasureClassif,
                        public = list(
                          #' @description
                          #' Creates a new `cox_slope` object.
                          initialize = function() {
                            super$initialize(
                              id = "classif.spiegelhaltersz",
                              packages = "CalibratR",
                              properties = character(),
                              predict_type = "prob",
                              range = c(0,1),
                              minimize = FALSE
                            )
                          }
                        ),
                        private = list(
                          .score = function(prediction, ...) {
                            actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
                            predicted = prediction$prob[, 1]

                            # Calculate numerator
                            numerator <- sum((actual - predicted) * (1 - 2 * predicted))

                            # Calculate denominator
                            denominator <- sqrt(sum(((1 - 2 * predicted)^2) * predicted * (1 - predicted)))

                            # Compute z-score and p-value
                            z_score <- numerator / denominator
                            p_value <- 2 * (1 - pnorm(abs(z_score)))  # Two-tailed test

                            return(p_value)
                          }
                        )
)

mlr3::mlr_measures$add("classif.spiegelhaltersz", spiegelhaltersz)


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
