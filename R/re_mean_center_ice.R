#' mean-centering of ICE matrices
#'
#' @param Y List of data.frame (one per feature)
#' @param grid List of grid column names (one per feature)
#' @param idx Integer vector of row indices to keep
#' @return List of mean-centered matrices
re_mean_center_ice = function(Y, grid, idx) {
  feature.names = names(Y)
  Y.centered = lapply(seq_along(Y), function(i) {
    Y[[i]] = Y[[i]][idx, ]
    Y[[i]][, which(!(colnames(Y[[i]]) %in% grid[[i]]))] = NA
    Y[[i]] - rowMeans(Y[[i]], na.rm = TRUE)
  })
  names(Y.centered) = feature.names
  return(Y.centered)
}
