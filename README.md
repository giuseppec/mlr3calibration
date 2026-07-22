
# mlr3calibration

mlr3calibration extends mlr3 with probability calibration workflows,
calibration measures, and reliability plots. The central entry point is
`po("calibrate")`, which wraps a probabilistic Learner, GraphLearner, or
full Graph.

## Installation

Install the released version from CRAN:

``` r
install.packages("mlr3calibration")
```

Install the development version from GitHub:

``` r
remotes::install_github("giuseppec/mlr3calibration")
```

## Core workflows

`po("calibrate")` supports two workflows:

1.  Calibrate a fixed learner or full graph/pipeline.
2.  Jointly tune the calibrated object with an outer `AutoTuner`.

Two calibration strategies are available, mirroring scikit-learn’s
`CalibratedClassifierCV`:

  - `strategy = "oof"` (default, like `ensemble = FALSE`) fits one
    calibrator on the pooled out-of-fold predictions and refits a single
    uncalibrated model on the full training task; at prediction time
    that single model is calibrated.
  - `strategy = "per_fold"` (like `ensemble = TRUE`) fits one calibrator
    per resampling fold and averages the fold-wise calibrated
    probabilities, yielding a calibrated bagged ensemble.

Preprocessing that should participate in calibration resampling should
be part of `learner = ...`, not placed upstream from `po("calibrate")`.

`tune_then_calibrate()` is a sequential one-call alternative: it tunes
on the full training task, then calibrates that fixed learner. Prefer an
outer `AutoTuner` around `po("calibrate")` for joint tuning.

### 1\. Calibrate a fixed learner or full graph

``` r
set.seed(1)

library(mlr3)
library(mlr3calibration)
library(mlr3learners)
library(mlr3pipelines)

data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

base_graph = po("scale") %>>%
  po("learner", learner = lrn("classif.rpart", predict_type = "prob"))

learner_uncal = as_learner(base_graph$clone(deep = TRUE))
learner_cal = as_learner(po(
  "calibrate",
  learner = base_graph$clone(deep = TRUE),
  calibrator = clb("platt"),
  strategy = "oof",
  resampling = rsmp("cv", folds = 3)
))

learner_uncal$train(task_train)
learner_cal$train(task_train)

prediction_uncal = learner_uncal$predict(task_test)
prediction_cal = learner_cal$predict(task_test)
prediction_cal$score(msr("classif.bbrier"))
```

### 2\. Jointly tune the calibrated object with an outer AutoTuner

``` r
library(mlr3tuning)

learner_joint = as_learner(po(
  "calibrate",
  learner = lrn("classif.rpart", predict_type = "prob"),
  calibrator = clb("selector", choices = c("platt", "beta")),
  strategy = "oof",
  resampling = rsmp("cv", folds = 2)
))

search_space = paradox::ps(
  "calibrate.classif.rpart.cp" = paradox::p_dbl(0.001, 0.1)
)
search_space = paradox::ParamSetCollection$new(c(
  list(search_space),
  list(calibrator_selector_search_space(choices = c("platt", "beta")))
))

autotuner_joint = AutoTuner$new(
  learner = learner_joint,
  resampling = rsmp("cv", folds = 2),
  measure = msr("classif.bbrier"),
  search_space = search_space,
  terminator = bbotk::trm("evals", n_evals = 4),
  tuner = tnr("grid_search", resolution = 2)
)

suppressWarnings(autotuner_joint$train(task_train))
autotuner_joint$predict(task_test)$score(msr("classif.bbrier"))
```

### Convenience helper: tune\_then\_calibrate

``` r
inner_at = auto_tuner(
  tuner = tnr("grid_search", resolution = 2),
  learner = lrn(
    "classif.rpart",
    predict_type = "prob",
    cp = paradox::to_tune(0.001, 0.1)
  ),
  resampling = rsmp("cv", folds = 2),
  measure = msr("classif.bbrier"),
  term_evals = 4
)

learner_seq = tune_then_calibrate(
  learner = inner_at,
  calibrator = clb("platt"),
  strategy = "oof",
  resampling = rsmp("cv", folds = 2)
)

learner_seq$train(task_train)
learner_seq$predict(task_test)$score(msr("classif.bbrier"))
```

## Measures and plots

Binary measures: `classif.ece` (binary positive-class ECE),
`classif.ici`, `classif.hltest`, `classif.cox_intercept`,
`classif.cox_slope`, and `classif.spiegelhaltersz`. Multiclass
calibration-error measures: `classif.conf_ece`, `classif.sce`,
`classif.ace`, `classif.tace`, and `classif.tl_ece`. See
`vignette("mlr3calibration")` for sources, edge conventions, and
selection guidance.

``` r
measures = msrs(c(
  "classif.bbrier",
  "classif.ece",
  "classif.ici",
  "classif.hltest",
  "classif.cox_intercept",
  "classif.cox_slope",
  "classif.spiegelhaltersz"
))

prediction_uncal$score(measures)
prediction_cal$score(measures)

calibration_plot(list(learner_uncal, learner_cal), task_test, smooth = TRUE)
calibration_plot(predictions = list(
  uncalibrated = prediction_uncal,
  calibrated = prediction_cal
), smooth = TRUE)
```

## Reusing a precomputed resample result

``` r
rr = resample(
  task_train,
  lrn("classif.rpart", predict_type = "prob"),
  rsmp("cv", folds = 3),
  store_models = TRUE
)

learner_platt = as_learner(po(
  "calibrate",
  rr = rr,
  calibrator = clb("platt"),
  strategy = "oof"
))

learner_platt$train(task_train)
learner_platt$predict(task_test)$score(msr("classif.ece"))
```

The precomputed result must be from the same calibration task and
contain stored probability learners with complete out-of-fold test
predictions. `learner`, `resampling`, and learner hyperparameters cannot
be supplied in this mode. With `strategy = "oof"`, a single uncalibrated
model is still refitted on the full training task using the stored
learners’ configuration.

## Multiclass support

Use `clb("ovr")` to wrap a binary-capable calibrator with one-vs-rest
fitting and row normalization. Direct `clb("platt")` is binary-only;
multiclass Platt is `clb("ovr")`.

``` r
task_iris = as_task_classif(iris, target = "Species")
splits_iris = partition(task_iris)
task_iris_train = task_iris$clone()$filter(splits_iris$train)
task_iris_test = task_iris$clone()$filter(splits_iris$test)

learner_multiclass = as_learner(po(
  "calibrate",
  learner = lrn("classif.rpart", predict_type = "prob"),
  calibrator = clb("ovr", calibrator = clb("isotonic")),
  strategy = "oof",
  resampling = rsmp("cv", folds = 3)
))

learner_multiclass$train(task_iris_train)
prediction_iris = learner_multiclass$predict(task_iris_test)
head(prediction_iris$prob)

prediction_iris$score(msrs(c(
  "classif.conf_ece",
  "classif.sce",
  "classif.ace",
  "classif.tace",
  "classif.tl_ece"
)))
```

## References

  - Platt, J. (1999). Probabilistic outputs for support vector machines
    and comparisons to regularized likelihood methods. *Advances in
    Large Margin Classifiers*, MIT Press.
  - Zadrozny, B., & Elkan, C. (2002). Transforming classifier scores
    into accurate multiclass probability estimates. *KDD*.
  - Niculescu-Mizil, A., & Caruana, R. (2005). Predicting good
    probabilities with supervised learning. *ICML*.
  - Kull, M., Silva Filho, T. M., & Flach, P. (2017). Beta calibration.
    *AISTATS*.
  - scikit-learn:
    [`CalibratedClassifierCV`](https://scikit-learn.org/stable/modules/calibration.html)
    — `ensemble = FALSE` / `ensemble = TRUE` correspond to `strategy =
    "oof"` / `"per_fold"`.
