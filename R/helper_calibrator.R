assert_calibrator_class_support = function(properties, class_names, id) {
  assert_character(class_names, min.len = 2L, any.missing = FALSE)

  if (length(class_names) == 2L) {
    if (!("twoclass" %in% properties) && !("multiclass" %in% properties)) {
      msg = sprintf("Calibrator '%s' does not declare binary classification support.", id)
      cli_abort("{msg}")
    }
    return(invisible(class_names))
  }

  if (!("multiclass" %in% properties)) {
    msg = sprintf("Calibrator '%s' does not support multiclass calibration.", id)
    cli_abort("{msg}")
  }

  invisible(class_names)
}

validate_calibrator_generator = function(name, calibrator, require_default = FALSE) {
  assert_string(name, min.chars = 1L)
  assert_class(calibrator, "R6ClassGenerator")
  assert_flag(require_default)

  generator = calibrator
  inherits_calibrator = FALSE

  while (!is.null(generator)) {
    if (identical(generator, Calibrator)) {
      inherits_calibrator = TRUE
      break
    }

    generator = generator$get_inherit()
  }

  if (!inherits_calibrator) {
    cli_abort("'calibrator' must be an R6 class generator that inherits from Calibrator.")
  }

  if (require_default) {
    prototype = tryCatch(
      calibrator$new(),
      error = function(e) {
        msg = sprintf(
          paste(
            "Calibrator '%s' must support default construction for built-in registration",
            "and selector choices: %s"
          ),
          name,
          conditionMessage(e)
        )
        cli_abort("{msg}")
      }
    )
    assert_calibrator(prototype)

    if (!identical(prototype$id, name)) {
      msg = sprintf(
        "Default-constructed calibrator id ('%s') must equal dictionary key '%s'.",
        prototype$id,
        name
      )
      cli_abort("{msg}")
    }
  }

  invisible(name)
}

assert_calibrator = function(calibrator) {
  assert_r6(calibrator, classes = "Calibrator")
  invisible(calibrator)
}

resolve_calibrator = function(calibrator = NULL) {
  if (is.null(calibrator)) {
    return(clb("platt"))
  }

  assert_calibrator(calibrator)
  calibrator$clone(deep = TRUE)
}
