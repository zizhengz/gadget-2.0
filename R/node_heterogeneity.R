node_heterogeneity = function(Y) {
  node.heterogeneity = vapply(Y, function(Y.i) {
    sum(Rfast::colsums(as.matrix(Y.i)^2) - Rfast::colsums(as.matrix(Y.i))^2 / nrow(Y.i), na.rm = TRUE)
  }, NA_real_)
  return(node.heterogeneity)
}
