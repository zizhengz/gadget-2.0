#' Prepare PD plot data per node (internal).
#'
#' Given effect, tree, depth, feature_set, mean_center: calls
#' \code{mean_center_ice} for Y and grid; assigns each row to a node at
#' that depth; for nodes with subset grids, sets out-of-grid cells to NA
#' and optionally mean-centers. Returns list of effect matrices (one per
#' feature) with \code{node} column added.
#' @param effect,tree,depth,feature_set,mean_center See PD plotting helpers.
#' @keywords internal
prepare_plot_data_pd = function(effect, tree, depth, feature_set, mean_center) {
  wide_mean_center = mean_center_ice(effect, feature_set = feature_set, mean_center = mean_center)
  Y = wide_mean_center$Y
  feature_name = names(Y)
  grid_total = wide_mean_center$grid
  nodes = tree[[depth]]

  Y = lapply(seq_along(Y), function(i) {
    Y_i = Y[[i]]
    Y_i$node = NA_integer_
    grid_i_total = grid_total[[i]]
    for (node_idx in seq_along(nodes)) {
      node = nodes[[node_idx]]
      subset_idx = node$subset_idx
      grid_i_curr = node$grid[[i]]
      Y_i$node[subset_idx] = node_idx
      if (length(grid_i_curr) < length(grid_i_total)) {
        Y_i[subset_idx, which(!(grid_i_total %in% grid_i_curr))] = NA
        if (mean_center && length(grid_i_curr) > 1) {
          Y_i[subset_idx, grid_i_total] = Y_i[subset_idx, grid_i_total] -
            rowMeans(Y_i[subset_idx, grid_i_total], na.rm = TRUE)
        }
      }
    }
    Y_i
  })
  names(Y) = feature_name
  return(Y)
}
