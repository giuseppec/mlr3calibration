#' @importFrom utils getFromNamespace data
"_PACKAGE"

mlr3_calibrators = mlr3misc::Dictionary$new()

register_calibrator = function(name, calibrator){
  mlr3_calibrators$add(name, calibrator)
}


clb = function(name){
  return(mlr3_calibrators$get(name))
}
