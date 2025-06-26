prepare_regional_pd_data = function(effect, tree, depth) {

  nodes = tree[[depth]]
  effect.results = effect$results
  wide.mean.center = mean_center_ice(effect)
  Y = wide.mean.center$Y
  grid_total = wide.mean.center$grid

  if (class(effect.results) == "data.frame") {
    Y$node = NA_integer_
    for (node_idx in 1:length(nodes)) {
      node = nodes[[node_idx]]
      subset.idx = node$subset.idx
      grid_curr = node$grid
      Y$node[subset.idx] = node_idx
      if (length(grid_curr) < length(grid_total)) {
        Y[subset.idx, which(!(grid_total %in% grid_curr))] = NA
        Y[subset.idx, grid_total] = Y[subset.idx, grid_total] - rowMeans(Y[subset.idx, grid_total], na.rm = TRUE)
      }
    }
    Y = list(Y)
    names(Y) = wide.mean.center$feature.name
  } else if (class(effect.results) == "list") {
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
    names(Y) = names(effect.results)
  }
  return(Y)
}
