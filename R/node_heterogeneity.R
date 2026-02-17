#' Compute heterogeneity (sum of variances) per feature from effect matrices
#'
#' Used by \code{pdStrategy$heterogeneity()}. For each matrix in \code{Y},
#' returns the sum over columns of (sum of squares - (sum)^2/n).
#'
#' @param Y List of numeric matrices (e.g., ICE effect matrices per feature).
#' @return Numeric vector of length \code{length(Y)}.
#' @keywords internal
node_heterogeneity = function(Y) {
  node.heterogeneity = vapply(Y, function(Y.i) {
    sum(Rfast::colsums(as.matrix(Y.i)^2) - Rfast::colsums(as.matrix(Y.i))^2 / nrow(Y.i), na.rm = TRUE)
  }, NA_real_)
  return(node.heterogeneity)
}
