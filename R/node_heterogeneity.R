node_heterogeneity = function(Y) {
  checkmate::assert_list(Y)
  node.heterogeneity = vapply(Y, function(Y_i) {
    sum(Rfast::colsums(as.matrix(Y_i)^2) - Rfast::colsums(as.matrix(Y_i))^2 / nrow(Y_i), na.rm = TRUE)
  }, NA_real_)
  return(node.heterogeneity)
}
