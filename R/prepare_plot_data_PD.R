prepare_plot_data_pd = function(effect, tree, depth, feature.set, mean.center) {
  #effect.results = effect$results
  wide.mean.center = mean_center_ice(effect, feature.set = feature.set, mean.center = mean.center)
  Y = wide.mean.center$Y
  feature.name = names(Y)
  grid.total = wide.mean.center$grid
  nodes = tree[[depth]]

  Y = lapply(seq_along(Y), function(i) {
    Y.i = Y[[i]]
    Y.i$node = NA_integer_
    grid.i.total = grid.total[[i]]
    for (node.idx in seq_along(nodes)) {
      node = nodes[[node.idx]]
      subset.idx = node$subset.idx
      grid.i.curr = node$grid[[i]]
      Y.i$node[subset.idx] = node.idx
      if (length(grid.i.curr) < length(grid.i.total)) {
        Y.i[subset.idx, which(!(grid.i.total %in% grid.i.curr))] = NA
        if (mean.center && length(grid.i.curr) > 1){
          Y.i[subset.idx, grid.i.total] = Y.i[subset.idx, grid.i.total] - rowMeans(Y.i[subset.idx, grid.i.total], na.rm = TRUE)
        }
      }
    }
    Y.i
  })
  names(Y) = feature.name
  return(Y)
}
