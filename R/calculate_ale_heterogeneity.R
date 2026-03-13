#' Calculate ALE Heterogeneity
#'
#' @param Y List or data.frame. ALE effect data.
#'
#' @return Numeric. If Y is a list, returns a list of heterogeneity values for each feature.
#'   If Y is a data.frame, returns a single heterogeneity value.
#'
calculate_ale_heterogeneity_cpp = function(Y) {
  # Handle both data.frame and list cases
  if (is.data.frame(Y)) {
    d_l = Y$d_l
    interval_index = Y$interval_index
    calculate_ale_heterogeneity_single_cpp(d_l, interval_index)
  } else if (is.list(Y)) {
    calculate_ale_heterogeneity_list_cpp(Y)
  } else {
    stop("Y must be either a data.frame or a list")
  }
}

#' Sum-of-squares ALE heterogeneity from list of effect data.tables (internal).
#' @param y_list List of ALE data.tables per feature.
#' @keywords internal
ss_ale_dt = function(y_list) {
  lapply(y_list, function(feat) {
    delta_aggr = feat[, list(d_l = mean(d_l, na.rm = TRUE),
        interval_n = .N), by = c("interval_index", "x_left", "x_right")]
    df = merge(feat, delta_aggr, by = "interval_index")
    sum((df$d_l.x - df$d_l.y)^2, na.rm = TRUE)
  })
}
