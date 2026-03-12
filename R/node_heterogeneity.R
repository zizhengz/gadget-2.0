#' Compute heterogeneity per feature from effect matrices
#'
#' Given Y (list of numeric matrices): for each matrix, computes sum over columns of (sum of squares - (sum)^2/n) via \code{node_heterogeneity_cpp}. 
#' Returns numeric vector of length \code{length(Y)}.
#'
#' @param Y List of numeric matrices (e.g., ICE effect matrices per feature).
#' @return Numeric vector of length \code{length(Y)}.
#' @keywords internal
node_heterogeneity = function(Y) {
  Y_mat = lapply(Y, function(x) as.matrix(x))
  node_heterogeneity_cpp(Y_mat)
}
