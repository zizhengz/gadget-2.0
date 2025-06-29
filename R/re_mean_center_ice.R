re_mean_center_ice = function(Y, idx, grid) {
  checkmate::assert_list(Y)
  checkmate::assert_list(grid)

  feature.names = names(Y)
  Y.centered = lapply(seq_along(Y), function(i) {
    Y[[i]] = Y[[i]][idx, ]
    Y[[i]][, which(!(colnames(Y[[i]]) %in% grid[[i]]))] = NA
    Y[[i]] - rowMeans(Y[[i]], na.rm = TRUE)
  })
  names(Y.centered) = feature.names
  return(Y.centered)
}
