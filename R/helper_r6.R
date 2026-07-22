deep_clone_r6_objects = function(value) {
  if (inherits(value, "R6")) {
    return(value$clone(deep = TRUE))
  }

  if (is.list(value)) {
    cloned = lapply(value, deep_clone_r6_objects)
    attributes(cloned) = attributes(value)
    return(cloned)
  }

  value
}
