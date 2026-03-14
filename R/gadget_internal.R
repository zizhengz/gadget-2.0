# Internal C++ wrappers and package symbols (not for direct use).
# risk_from_stats(n, s1, s2): interval-wise SSE; 0 if n <= 1.
# default_predict_fun: mlr3-style prediction; model$predict_newdata(data) -> numeric.

risk_from_stats = function(n, s1, s2) ifelse(n <= 1L, 0.0, s2 - (s1 * s1) / n)

assert_ale_effect_list = function(Y, var_name = "Y") {
  checkmate::assert_list(Y, min.len = 1, .var.name = var_name)
  checkmate::assert_true(all(mlr3misc::map_lgl(Y, is.data.frame)), .var.name = var_name)
}

default_predict_fun = function(model, data) {
  pred = model$predict_newdata(data)
  if (inherits(pred, "Prediction")) pred$response else pred
}

#' Internal C++ helpers and package symbols
#'
#' Functions and symbols used internally by the package. Not intended for direct use.
#'
#' @name gadget_internal
#' @aliases ale_sweep_cpp calculate_ale_heterogeneity_list_cpp
#'   calculate_ale_heterogeneity_single_cpp re_mean_center_ice_cpp
#'   search_best_split_cpp default_predict_fun risk_from_stats assert_ale_effect_list
#'   d_l interval_index level x x_grid x_left x_right y
#' @keywords internal
NULL
