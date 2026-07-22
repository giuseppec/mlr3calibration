#' mlr3calibration package
#'
#' @import checkmate
#' @importFrom R6 R6Class
#' @importFrom data.table as.data.table data.table rbindlist
#' @importFrom ggplot2 aes element_rect element_text geom_line geom_point geom_rug
#'   geom_smooth ggplot ggtitle labs theme theme_minimal xlim ylim
#' @importFrom mlr3 as_learner MeasureClassif PredictionClassif resample rsmp
#' @importFrom mlr3misc calculate_hash dictionary_sugar_get Dictionary format_bib iwalk map map_chr
#'   register_namespace_callback require_namespaces walk
#' @importFrom mlr3pipelines PipeOp
#' @importFrom mgcv gam s
#' @importFrom paradox CondEqual ParamSetCollection p_dbl p_fct p_int p_lgl ps
#' @importFrom stats aggregate approx binomial coef glm isoreg loess pchisq pnorm predict quantile setNames
#' @importFrom utils modifyList tail
#' @importFrom cli cli_abort cli_warn
"_PACKAGE"
