parameter_namespaces = function(ids) {
  assert_character(ids, any.missing = FALSE, null.ok = FALSE)

  namespaces = make.unique(make.names(ids), sep = ".")
  setNames(namespaces, ids)
}

parameter_namespace = function(id) {
  parameter_namespaces(id)[[id]]
}

namespace_param_values = function(values, namespace = NULL) {
  if (length(values) == 0L || is.null(namespace)) {
    return(values)
  }

  names(values) = paste0(namespace, ".", names(values))
  values
}

component_param_values = function(values, param_set, namespace = NULL) {
  if (length(values) == 0L) {
    return(list())
  }

  if (is.null(namespace)) {
    selector = names(values) %in% param_set$ids()
    return(values[selector])
  }

  prefix = paste0(namespace, ".")
  selector = startsWith(names(values), prefix)
  values = values[selector]

  if (length(values) == 0L) {
    return(list())
  }

  names(values) = substring(names(values), nchar(prefix) + 1L)
  values[names(values) %in% param_set$ids()]
}

namespaced_param_set_entries = function(param_set, namespace = NULL) {
  entry = list(param_set$clone(deep = TRUE))

  if (!is.null(namespace)) {
    names(entry) = namespace
  }

  entry
}

learner_param_namespace = function(learner) {
  assert_r6(learner, classes = "Learner")

  if (inherits(learner, "GraphLearner")) {
    return(NULL)
  }

  parameter_namespace(learner$id)
}

calibrator_param_namespace = function(calibrator) {
  assert_r6(calibrator, classes = "Calibrator")

  if (inherits(calibrator, "CalibratorSelector")) {
    return(NULL)
  }

  parameter_namespace(calibrator$id)
}

calibrator_component_values = function(values, calibrator, namespace = NULL) {
  if (!inherits(calibrator, "CalibratorSelector")) {
    return(component_param_values(values, calibrator$param_set, namespace = namespace))
  }

  choice = values$calibrator

  if (is.null(choice)) {
    choice = calibrator$choices[[1L]]
  }

  selector = names(values) == "calibrator"
  namespace_prefix = paste0(calibrator$namespaces[[choice]], ".")
  selector = selector | startsWith(names(values), namespace_prefix)

  modifyList(
    list(calibrator = choice),
    values[selector & names(values) != "calibrator"],
    keep.null = TRUE
  )
}
