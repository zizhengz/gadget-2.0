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
    dL = Y$dL
    interval_index = Y$interval.index
    calculate_ale_heterogeneity_single_cpp(dL, interval_index)
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
    delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
      interval.n = .N), by = c("interval.index", "x.left", "x.right")]
    df = merge(feat, delta.aggr, by = "interval.index")
    sum((df$dL.x - df$dL.y)^2, na.rm = TRUE)
  })
}
