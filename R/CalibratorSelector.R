#' @include Calibrator.R
#'
#' @title Calibrator selector
#'
#' @name mlr3_calibrators_selector
#'
#' @description
#' Selects one registered calibrator and exposes a joint parameter space for
#' calibrator choice and calibrator-specific parameters.
#' Include custom calibrators registered with [register_calibrator()] in `choices`.
#' Use [calibrator_selector_search_space()] for the matching dependency-aware
#' tuning space in an outer `AutoTuner`.
#' Declared packages are the union over choices; supported properties are the
#' intersection.
#' Changing selector parameters after training requires retraining before prediction.
#'
#' @param choices `character()`
#'   Registered calibrator keys to expose.
#'   Defaults to the built-in calibrators `"platt"`, `"beta"`, and
#'   `"isotonic"`.
CalibratorSelector = R6Class(
  "CalibratorSelector",
  inherit = Calibrator,
  public = list(
    #' @field choices `character()`.
    #'   Registered calibrator keys exposed by the selector.
    choices = NULL,
    #' @field namespaces `character()`.
    #'   Mapping from calibrator keys to valid parameter namespaces.
    namespaces = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6Class] class.
    initialize = function(choices = NULL) {
      available_choices = setdiff(mlr3_calibrators$keys(), "selector")
      built_in_choices = c("platt", "beta", "isotonic")

      if (is.null(choices)) {
        choices = built_in_choices[built_in_choices %in% available_choices]
      }

      assert_character(choices, min.len = 1L, any.missing = FALSE, null.ok = FALSE, unique = TRUE)

      if ("selector" %in% choices) {
        cli_abort("'selector' cannot be nested inside CalibratorSelector.")
      }

      assert_subset(choices, choices = available_choices)

      namespaces = parameter_namespaces(choices)
      calibrators = setNames(lapply(choices, clb), choices)
      param_sets = c(
        list(ps(
          calibrator = p_fct(
            levels = choices,
            default = choices[[1L]],
            tags = "train"
          )
        )),
        unlist(
          Filter(
            Negate(is.null),
            lapply(choices, function(choice) {
              param_set = calibrators[[choice]]$param_set

              if (length(param_set$ids()) == 0L) {
                return(NULL)
              }

              param_set_entry = list(param_set$clone(deep = TRUE))
              param_set_entry[[1L]]$values = list()
              names(param_set_entry) = namespaces[[choice]]
              param_set_entry
            })
          ),
          recursive = FALSE,
          use.names = TRUE
        )
      )

      param_set = ParamSetCollection$new(param_sets)

      for (choice in choices) {
        namespace = namespaces[[choice]]
        param_ids = calibrators[[choice]]$param_set$ids()

        if (length(param_ids) == 0L) {
          next
        }

        for (param_id in param_ids) {
          param_set$add_dep(
            id = paste0(namespace, ".", param_id),
            on = "calibrator",
            cond = CondEqual$new(choice)
          )
        }
      }

      supports_twoclass = vapply(
        calibrators,
        function(calibrator) {
          any(c("twoclass", "multiclass") %in% calibrator$properties)
        },
        logical(1L)
      )
      supports_multiclass = vapply(
        calibrators,
        function(calibrator) {
          "multiclass" %in% calibrator$properties
        },
        logical(1L)
      )
      properties = c(
        if (all(supports_twoclass)) "twoclass",
        if (all(supports_multiclass)) "multiclass"
      )
      private$.choice_hashes = map_chr(calibrators, function(calibrator) calibrator$hash)
      packages = as.character(unique(unlist(
        map(calibrators, function(calibrator) calibrator$packages),
        use.names = FALSE
      )))

      super$initialize(
        id = "selector",
        param_set = param_set,
        properties = properties,
        packages = packages,
        label = "Calibrator selector"
      )

      self$choices = choices
      self$namespaces = namespaces
      self$param_set$values$calibrator = choices[[1L]]
    }
  ),
  private = list(
    .calibrator = NULL,
    .choice_hashes = NULL,
    .reset = function() {
      private$.calibrator = NULL
    },
    .selected_calibrator = function() {
      choice = self$param_set$values$calibrator

      if (is.null(choice)) {
        choice = self$choices[[1L]]
      }

      calibrator = clb(choice)
      namespace = self$namespaces[[choice]]
      values = component_param_values(
        values = self$param_set$values,
        param_set = calibrator$param_set,
        namespace = namespace
      )
      calibrator$param_set$values = modifyList(
        calibrator$param_set$values,
        values,
        keep.null = TRUE
      )

      calibrator
    },
    .additional_hash_input = function() {
      list(
        choices = self$choices,
        namespaces = self$namespaces,
        choice_hashes = private$.choice_hashes
      )
    },
    .assert_supported = function(class_names) {
      calibrator = private$.selected_calibrator()
      assert_calibrator_class_support(calibrator$properties, class_names, calibrator$id)
    },
    .train = function(truth, prob, class_names) {
      calibrator = private$.selected_calibrator()
      calibrator$train(truth = truth, prob = prob, class_names = class_names)
      private$.calibrator = calibrator
    },
    .predict = function(prob, class_names) {
      private$.calibrator$predict(prob = prob, class_names = class_names)
    }
  )
)
