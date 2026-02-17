#' Find best split across all columns of Z (R fallback)
#'
#' Calls \code{search_best_split_point} per column and returns a data frame
#' with one row per feature. Used when C++ \code{search_best_split_cpp} is not available.
#'
#' @param Z Data frame of split features.
#' @param Y List of effect matrices.
#' @param min.node.size Integer.
#' @param n.quantiles Integer or NULL.
#' @return Data frame with \code{split.feature}, \code{split.point}, \code{split.objective}, \code{best.split}, etc.
#' @keywords internal
search_best_split = function(Z, Y, min.node.size, n.quantiles) {
  t1 = proc.time()
  res = data.table::rbindlist(
    lapply(Z, function(z) {
      search_best_split_point(z = z, Y = Y, n.quantiles = n.quantiles,
        is.categorical = is.factor(z),
        min.node.size = min.node.size)
    })
  )
  t2 = proc.time()
  res$split.feature = colnames(Z)
  res$is.categorical = unname(sapply(Z, is.factor))
  res$best.split = res$split.objective == min(res$split.objective, na.rm = TRUE)
  res$split.runtime = (t2 - t1)[[3]]
  return(res[, c("split.feature", "is.categorical", "split.point", "split.objective",
    "split.runtime", "best.split")])
}
