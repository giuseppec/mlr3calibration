## ----installation, message=FALSE, warning=FALSE-------------------------------
#remotes::install_github("giuseppec/mlr3calibration")
library(mlr3calibration)
library(mlr3verse)

## ----task---------------------------------------------------------------------
# Load a binary classification task
set.seed(1)
data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")
splits = partition(task)
task_train = task$clone()$filter(splits$train)
task_test = task$clone()$filter(splits$test)

