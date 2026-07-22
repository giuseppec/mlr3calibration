#' @include Calibrator.R
#'
#' @title One-vs-rest calibrator
#'
#' @name mlr3_calibrators_ovr
#'
#' @description
#' Applies a binary [Calibrator] separately to each class and returns class scores that are
#' row-normalized by the base [Calibrator] lifecycle.
#'
#' For two classes, the wrapper fits exactly one wrapped calibrator on the original probability
#' matrix and preserves the package convention that column 1 is the modeled positive event.
#' For more than two classes, it fits one independent calibrator per class against a collision-safe
#' rest label and extracts the calibrated score of the original class.
#'
#' Simple normalization follows Zadrozny and Elkan (2002).
#' Normalized one-vs-rest outputs are not guaranteed to be fully multiclass-calibrated
#' (`r format_bib("kull2019")`; `r format_bib("arad2025")`).
#'
#' Direct [CalibratorPlatt] is binary-only; multiclass Platt uses `clb("ovr")` (Platt by default).
#' Other binary calibrators such as isotonic or beta may be supplied.
#'
#' @param calibrator [Calibrator]
#'   Binary-capable calibrator prototype.
#'   Defaults to a deep clone of `clb("platt")`.
#'
#' @references
#' `r format_bib("zadrozny2002")`
#'
#' `r format_bib("kull2019")`
#'
#' `r format_bib("arad2025")`
CalibratorOneVsRest = R6Class(
  "CalibratorOneVsRest",
  inherit = Calibrator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6Class] class.
    initialize = function(calibrator = NULL) {
      if (is.null(calibrator)) {
        calibrator = clb("platt")
      } else {
        assert_calibrator(calibrator)
        calibrator = calibrator$clone(deep = TRUE)
      }

      if (inherits(calibrator, "CalibratorOneVsRest")) {
        cli_abort("CalibratorOneVsRest cannot wrap another CalibratorOneVsRest.")
      }

      assert_calibrator_class_support(calibrator$properties, c("yes", "no"), calibrator$id)

      namespace = parameter_namespace(calibrator$id)
      if (length(calibrator$param_set$ids()) == 0L) {
        param_set = ps()
      } else {
        param_set = ParamSetCollection$new(
          namespaced_param_set_entries(calibrator$param_set, namespace)
        )
        param_set$values = namespace_param_values(calibrator$param_set$values, namespace)
      }

      super$initialize(
        id = "ovr",
        param_set = param_set,
        properties = c("twoclass", "multiclass"),
        packages = calibrator$packages,
        label = "One-vs-rest calibrator"
      )

      private$.prototype = calibrator
      private$.prototype_hash = calibrator$hash
    }
  ),
  private = list(
    .prototype = NULL,
    .prototype_hash = NULL,
    .calibrators = NULL,
    .binary_class_names = NULL,
    .reset = function() {
      private$.calibrators = NULL
      private$.binary_class_names = NULL
    },
    .additional_hash_input = function() {
      list(
        prototype_class = class(private$.prototype),
        prototype_id = private$.prototype$id,
        prototype_hash = private$.prototype_hash
      )
    },
    .configured_prototype = function() {
      calibrator = private$.prototype$clone(deep = TRUE)
      namespace = parameter_namespace(calibrator$id)
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
    .train = function(truth, prob, class_names) {
      if (length(class_names) == 2L) {
        calibrator = private$.configured_prototype()
        calibrator$train(truth = truth, prob = prob, class_names = class_names)
        private$.calibrators = list(calibrator)
        private$.binary_class_names = list(class_names)
        names(private$.calibrators) = class_names[[1L]]
        names(private$.binary_class_names) = class_names[[1L]]
        return(invisible())
      }

      calibrators = vector("list", length(class_names))
      binary_class_names = vector("list", length(class_names))
      names(calibrators) = class_names
      names(binary_class_names) = class_names
      rest_name = tail(make.unique(c(class_names, ".rest")), 1L)

      for (class_name in class_names) {
        binary_names = c(class_name, rest_name)
        binary_truth = factor(
          ifelse(truth == class_name, class_name, rest_name),
          levels = binary_names
        )
        binary_prob = binary_probability_matrix(
          prob_pos = prob[, class_name],
          class_names = binary_names
        )
        calibrator = private$.configured_prototype()
        calibrator$train(truth = binary_truth, prob = binary_prob, class_names = binary_names)
        calibrators[[class_name]] = calibrator
        binary_class_names[[class_name]] = binary_names
      }

      private$.calibrators = calibrators
      private$.binary_class_names = binary_class_names
    },
    .predict = function(prob, class_names) {
      if (length(class_names) == 2L) {
        return(private$.calibrators[[1L]]$predict(prob = prob, class_names = class_names))
      }

      scores = vapply(
        class_names,
        function(class_name) {
          binary_names = private$.binary_class_names[[class_name]]
          binary_prob = binary_probability_matrix(
            prob_pos = prob[, class_name],
            class_names = binary_names
          )
          private$.calibrators[[class_name]]$predict(
            prob = binary_prob,
            class_names = binary_names
          )[, 1L]
        },
        numeric(nrow(prob))
      )
      matrix(scores, nrow = nrow(prob), dimnames = list(NULL, class_names))
    },
    deep_clone = function(name, value) {
      deep_clone_r6_objects(value)
    }
  )
)
