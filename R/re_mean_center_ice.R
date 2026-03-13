#' Mean-center ICE matrices for a node. Given Y (list of effect matrices), grid (column names per feature),
#' idx: subsets rows by idx; sets non-grid columns to NA; subtracts row means.
#' Returns list of mean-centered matrices.
#'
#' @param Y List of data.frame (one per feature)
#' @param grid List of grid column names, i.e. character vectors (one per feature)
#' @param idx Integer vector of row indices to keep
#' @return List of mean-centered matrices
re_mean_center_ice = function(Y, grid, idx) {
  feature_names = names(Y)
  Y_centered = lapply(seq_along(Y), function(i) {
    Y[[i]] = Y[[i]][idx, ]
    Y[[i]][, which(!(colnames(Y[[i]]) %in% grid[[i]]))] = NA
    Y[[i]] - rowMeans(Y[[i]], na.rm = TRUE)
  })
  names(Y_centered) = feature_names
  return(Y_centered)
}
