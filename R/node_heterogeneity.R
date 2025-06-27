node_heterogeneity = function(Y) {
  checkmate::assert_list(Y)
  node_heterogeneity = sum(vapply(Y, function(Y_i) {
    sum(Rfast::colsums(as.matrix(Y_i)^2) - Rfast::colsums(as.matrix(Y_i))^2 / nrow(Y_i), na.rm = TRUE)
  }, NA_real_), na.rm = TRUE)
  return(node_heterogeneity)
}
