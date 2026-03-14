#' Compute heterogeneity per feature from effect matrices
#'
#' Given Y (list of numeric matrices): for each matrix, computes sum over
#' columns of (sum of squares - (sum)^2/n) via \code{node_heterogeneity_cpp}.
#' Returns numeric vector of length \code{length(Y)}.
#'
#' @param Y (`list()`) \cr
#'   List of numeric matrices (ICE effect per feature).
#'
#' @return (`numeric()`) \cr
#'   Heterogeneity per feature, length \code{length(Y)}.
#' @keywords internal
node_heterogeneity = function(Y) {
  y_mat = mlr3misc::map(Y, as.matrix)
  node_heterogeneity_cpp(y_mat)
}
