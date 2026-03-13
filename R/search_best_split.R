#' Find best split across all columns of Z (R fallback)
#'
#' Given Z, Y, min_node_size, n_quantiles: loops over each column, calls
#' \code{search_best_split_point}; rbind results; adds split_feature,
#' is_categorical, best_split, split_runtime.
#' Returns data frame (one row per feature).
#'
#' @param Z Data frame of split features.
#' @param Y List of effect matrices.
#' @param min_node_size Integer.
#' @param n_quantiles Integer or NULL.
#' @return Data frame with \code{split_feature}, \code{split_point}, \code{split_objective}, \code{best_split}, etc.
#' @keywords internal
search_best_split = function(Z, Y, min_node_size, n_quantiles) {
  t1 = proc.time()
  res = data.table::rbindlist(
    lapply(Z, function(z) {
      search_best_split_point(z = z, Y = Y, n_quantiles = n_quantiles,
        is_categorical = is.factor(z),
        min_node_size = min_node_size)
    })
  )
  t2 = proc.time()
  res$split_feature = colnames(Z)
  res$is_categorical = unname(sapply(Z, is.factor))
  res$best_split = res$split_objective == min(res$split_objective, na.rm = TRUE)
  res$split_runtime = (t2 - t1)[[3]]
  res[, c("split_feature", "is_categorical", "split_point",
        "split_objective", "split_runtime", "best_split")]
}
