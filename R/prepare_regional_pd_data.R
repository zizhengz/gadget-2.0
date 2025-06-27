prepare_regional_pd_data = function(effect, tree, depth) {
  effect.results = effect$results
  wide.mean.center = mean_center_ice(effect)
  Y = wide.mean.center$Y
  feature.name = names(Y)
  grid_total = wide.mean.center$grid
  nodes = tree[[depth]]

  Y = lapply(seq_along(Y), function(i) {
    Y_i = Y[[i]]
    Y_i$node = NA_integer_
    grid_i_total = grid_total[[i]]
    for (node_idx in 1:length(nodes)) {
      node = nodes[[node_idx]]
      subset.idx = node$subset.idx
      grid_i_curr = node$grid[[i]]
      Y_i$node[subset.idx] = node_idx
      if (length(grid_i_curr) < length(grid_i_total)) {
        Y_i[subset.idx, which(!(grid_i_total %in% grid_i_curr))] = NA
        Y_i[subset.idx, grid_i_total] = Y_i[subset.idx, grid_i_total] - rowMeans(Y_i[subset.idx, grid_i_total], na.rm = TRUE)
      }
    }
    Y_i
  })
  names(Y) = feature.name
  return(Y)
}
