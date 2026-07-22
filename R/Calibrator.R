#' @title Calibrator base class
#'
#' @description
#' R6 base class for probability calibration.
#' Calibrators train on class labels and class-probability matrices and predict calibrated probabilities.
#' Training requires every declared class to be observed.
#' Prediction requires the same named class set as training; column order may differ.
#'
#' `Calibrator` handles labels, probability matrices, and class names.
#' [PipeOpCalibrate] owns tasks, held-out predictions, learners, resampling, and prediction objects.
#'
#' ## Lifecycle
#'
#' - `$train()` validates inputs and class support, requires packages, calls `$reset()`,
#'   dispatches to the subclass hook, then records fitted class order and configuration hash.
#' - `$predict()` rejects untrained or stale objects (configuration `$hash` mismatch),
#'   requires packages, dispatches, and normalizes output.
#' - `$reset()` clears fitted and backend state without changing the configuration `$hash`.
#'
#' Input-validation or missing-dependency failures leave an existing valid fit untouched.
#' A backend fitting failure after `$reset()` leaves the object untrained with backend state cleared.
#'
#' ## Properties
#'
#' `properties` are unique values from `"twoclass"` and `"multiclass"`.
#' Declaring `"multiclass"` also permits two-class input.
#' Declaring only `"twoclass"` rejects more than two classes.
#'
#' ## Subclass hooks
#'
#' Subclasses supply:
#'
#' - public `initialize()` (defaults required only for built-in / selector choices);
#' - private `.train(truth, prob, class_names)` and `.predict(prob, class_names)`;
#' - private `.reset()` when they own fitted state;
#' - `.additional_hash_input()` when configuration is not fully covered by base fields and parameters.
#'
#' The base passes factor truth aligned with rows, finite normalized probabilities,
#' probability columns that exactly match unique class names in a defined order,
#' and at least one training sample per declared class.
#'
#' Private `.predict()` may return a numeric vector (binary only; first-class probability)
#' or a numeric matrix over the fitted class set.
#' Values must be finite and in `[0, 1]` with positive row sums.
#' The base reorders columns and normalizes rows; do not rely on normalization to hide invalid output.
#'
#' ## State, packages, and registration
#'
#' Fitted state is private, cleared by `.reset()`, and excluded from configuration hashes.
#' Nested R6 fields must deep-clone independently.
#' Subclasses with environments, external pointers, or other reference state need a `deep_clone()` hook.
#'
#' Optional packages go in `packages`; constructors must not load them.
#' Objects from `clb(key, ...)` must have `id == key`.
#' Registration lasts for the session; use [register_calibrator()].
#'
#' @export
Calibrator = R6Class(
  "Calibrator",
  public = list(
    #' @field id `character(1)`.
    #'   Identifier for the calibrator.
    id = NULL,
    #' @field label `character(1)`.
    #'   Human-readable calibrator label.
    label = NULL,
    #' @field param_set [`paradox::ParamSet`].
    #'   Calibrator parameter set.
    param_set = NULL,
    #' @field properties `character()`.
    #'   Supported task kinds.
    #'   Unique subset of `"twoclass"` and `"multiclass"`.
    #'   Declaring `"multiclass"` also permits binary input.
    properties = NULL,
    #' @field packages `character()`.
    #'   Required packages, enforced by the base class at train and predict time.
    packages = NULL,

    #' @description
    #' Creates a new `Calibrator` object.
    #' @param id `character(1)`
    #'   Identifier for the calibrator.
    #' @param param_set [`paradox::ParamSet`]
    #'   Calibrator parameter set.
    #' @param properties `character()`
    #'   Unique subset of `"twoclass"` and `"multiclass"`.
    #' @param packages `character()`
    #'   Required packages.
    #' @param label `character(1)`
    #'   Human-readable label.
    initialize = function(
      id,
      param_set = ps(),
      properties = character(),
      packages = character(),
      label = id
    ) {
      assert_string(id, min.chars = 1L)
      assert_r6(param_set, classes = "ParamSet")
      assert_character(properties, any.missing = FALSE, null.ok = FALSE, unique = TRUE)
      assert_subset(properties, choices = c("twoclass", "multiclass"))
      assert_character(packages, any.missing = FALSE, null.ok = FALSE)
      assert_string(label, min.chars = 1L)

      self$id = id
      self$label = label
      self$param_set = param_set
      self$properties = properties
      self$packages = packages
    },

    #' @description
    #' Train the calibrator.
    #' @param truth `factor()`
    #'   Ground-truth labels.
    #' @param prob `matrix()`
    #'   Class-probability matrix.
    #' @param class_names `character()`
    #'   Class order used in `prob`.
    #' @return (`Calibrator`) Invisibly returns `self`.
    train = function(truth, prob, class_names = NULL) {
      inputs = calibrator_train_inputs(truth = truth, prob = prob, class_names = class_names)
      private$.assert_supported(inputs$class_names)
      private$.require_packages()
      self$reset()

      tryCatch(
        {
          private$.train(
            truth = inputs$truth,
            prob = inputs$prob,
            class_names = inputs$class_names
          )
          private$.class_names = inputs$class_names
          private$.trained_hash = self$hash
        },
        error = function(e) {
          self$reset()
          msg = conditionMessage(e)
          cli_abort("{msg}")
        }
      )

      invisible(self)
    },

    #' @description
    #' Predict calibrated probabilities.
    #' @param prob `matrix()`
    #'   Class-probability matrix.
    #' @param class_names `character()`
    #'   Class order used in `prob`.
    #' @return (`matrix()`) Calibrated probabilities in the caller's requested column order.
    predict = function(prob, class_names = NULL) {
      if (!self$is_trained) {
        msg = sprintf("Calibrator '%s' must be trained before prediction.", self$id)
        cli_abort("{msg}")
      }

      private$.require_packages()

      if (!identical(private$.trained_hash, self$hash)) {
        msg = sprintf(
          "Calibrator '%s' configuration changed after training; retrain before prediction.",
          self$id
        )
        cli_abort("{msg}")
      }

      inputs = calibrator_predict_inputs(prob = prob, class_names = class_names)

      if (!setequal(inputs$class_names, private$.class_names)) {
        cli_abort("Prediction probabilities must contain the same classes used during training.")
      }

      requested_class_names = inputs$class_names
      prob = inputs$prob[, private$.class_names, drop = FALSE]
      prob_calibrated = private$.predict(
        prob = prob,
        class_names = private$.class_names
      )
      prob_calibrated = normalize_probability_matrix(
        prob_calibrated,
        class_names = private$.class_names
      )

      prob_calibrated[, requested_class_names, drop = FALSE]
    },

    #' @description
    #' Clear fitted state without changing the configuration `$hash`.
    #' @return (`Calibrator`) Invisibly returns `self`.
    reset = function() {
      private$.trained_hash = NULL
      private$.class_names = NULL
      private$.reset()
      invisible(self)
    }
  ),
  active = list(
    #' @field hash `character(1)`.
    #'   Hash of the calibrator configuration.
    #'   Does not include fitted state.
    hash = function(val) {
      if (!missing(val)) {
        cli_abort("`$hash` is read-only.")
      }

      calculate_hash(list(
        class(self),
        self$id,
        self$param_set$values,
        self$properties,
        private$.additional_hash_input()
      ))
    },

    #' @field is_trained `logical(1)`.
    #'   Whether the calibrator currently holds a fitted configuration fingerprint.
    is_trained = function(val) {
      if (!missing(val)) {
        cli_abort("`$is_trained` is read-only.")
      }

      !is.null(private$.trained_hash)
    },

    #' @field class_names `character()` or `NULL`.
    #'   Fitted class order, or `NULL` before training and after `$reset()`.
    class_names = function(val) {
      if (!missing(val)) {
        cli_abort("`$class_names` is read-only.")
      }

      private$.class_names
    }
  ),
  private = list(
    .class_names = NULL,
    .trained_hash = NULL,
    .train = function(truth, prob, class_names) {
      cli_abort("Calibrator subclasses must implement private$.train().")
    },
    .predict = function(prob, class_names) {
      cli_abort("Calibrator subclasses must implement private$.predict().")
    },
    .reset = function() {
      invisible()
    },
    .additional_hash_input = function() {
      NULL
    },
    .assert_supported = function(class_names) {
      assert_calibrator_class_support(self$properties, class_names, self$id)
    },
    .require_packages = function() {
      if (length(self$packages) > 0L) {
        require_namespaces(self$packages)
      }
      invisible()
    }
  )
)

calibrator_train_inputs = function(truth, prob, class_names = NULL) {
  assert_factor(truth, min.len = 1L)

  if (is.null(class_names)) {
    if (is.null(colnames(prob))) {
      class_names = levels(truth)
    } else {
      class_names = colnames(prob)
    }
  }

  prob = normalize_probability_matrix(prob, class_names = class_names)
  truth = factor(as.character(truth), levels = class_names)

  if (length(truth) != nrow(prob)) {
    cli_abort("truth and prob must contain the same number of observations.")
  }

  if (anyNA(truth)) {
    cli_abort("truth contains levels that are not present in class_names.")
  }

  missing_classes = setdiff(class_names, unique(as.character(truth)))

  if (length(missing_classes) > 0L) {
    cli_abort("truth must contain at least one observed sample for every class.")
  }

  list(truth = truth, prob = prob, class_names = class_names)
}

calibrator_predict_inputs = function(prob, class_names = NULL) {
  if (is.null(class_names)) {
    class_names = colnames(prob)
  }

  list(
    prob = normalize_probability_matrix(prob, class_names = class_names),
    class_names = class_names
  )
}
