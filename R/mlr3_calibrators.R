#' @title Dictionary of Calibrators
#'
#' @description
#' A [mlr3misc::Dictionary] mapping calibrator keys to `R6ClassGenerator` objects
#' that inherit from [Calibrator].
#'
#' Discover available keys with `mlr3_calibrators$keys()` and construct instances
#' with [clb()]:
#'
#' ```r
#' mlr3_calibrators$keys()
#' clb("platt")
#' ```
#'
#' Register third-party calibrators with [register_calibrator()].
#' Direct dictionary `$add()` bypasses validation and is unsupported.
#'
#' @export
mlr3_calibrators = Dictionary$new()

#' Register a calibrator
#'
#' @description
#' Register a calibrator class in [mlr3_calibrators] for construction via [clb()].
#' A rejected generator is never added.
#' Generators with required arguments can be registered and built with `clb(key, ...)`,
#' but [CalibratorSelector] needs `clb(key)` to work without arguments.
#' Training configuration should live in `param_set` with a `"train"` tag and be covered by the configuration hash.
#'
#' @param name `character(1)`
#'   Dictionary key of the calibrator.
#' @param calibrator `R6ClassGenerator`
#'   R6 class generator inheriting from [Calibrator].
#'
#' @return Invisibly returns `name`.
#'
#' @examples
#' # Register a minimal pass-through calibrator, then build it with clb().
#' CalibratorIdentity = R6::R6Class("CalibratorIdentity",
#'   inherit = Calibrator,
#'   public = list(
#'     initialize = function() {
#'       super$initialize(id = "identity", properties = c("twoclass", "multiclass"),
#'         label = "Identity calibrator")
#'     }
#'   ),
#'   private = list(
#'     .train = function(truth, prob, class_names) NULL,
#'     .predict = function(prob, class_names) prob
#'   )
#' )
#' register_calibrator("identity", CalibratorIdentity)
#' clb("identity")
#' @export
register_calibrator = function(name, calibrator) {
  if (name %in% mlr3_calibrators$keys()) {
    msg = sprintf("A calibrator with key '%s' is already registered.", name)
    cli_abort("{msg}")
  }

  validate_calibrator_generator(name, calibrator, require_default = FALSE)
  mlr3_calibrators$add(name, calibrator)
  invisible(name)
}

#' Get a registered calibrator
#'
#' @description
#' Retrieve a calibrator from the package dictionary by key.
#' The constructed object's `id` must equal `.key`.
#'
#' @param .key `character(1)`
#'   Dictionary key of the calibrator.
#' @param ... `any`
#'   Additional arguments passed to the dictionary sugar helper.
#'
#' @return A calibrator instance.
#'
#' @examples
#' calibrator = clb("platt")
#' calibrator$id
#' @export
clb = function(.key, ...) {
  assert_string(.key, min.chars = 1L)
  calibrator = dictionary_sugar_get(dict = mlr3_calibrators, .key, ...)
  assert_calibrator(calibrator)

  if (!identical(calibrator$id, .key)) {
    msg = sprintf(
      "Constructed calibrator id ('%s') must equal dictionary key '%s'.",
      calibrator$id,
      .key
    )
    cli_abort("{msg}")
  }

  calibrator
}
