build_tree = function(effect, data, effect.method = "PD", split.feature = NULL, target.feature.name, n.split = 2,
  impr.par = 0.1, min.node.size = 10, n.quantiles = NULL) {

  checkmate::assert_data_frame(data)

  if (effect.method == "PD") {
    prepared_data = prepare_data_for_ice_splitting(effect = effect, data = data,
      target.feature.name = target.feature.name,
      split.feature = split.feature)
  }
  # Initialize the parent node of the tree
  parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(prepared_data$Z)),
    grid = prepared_data$grid, improvement.met = FALSE, intImp = 0)

  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    leaves = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(leaves)) {

      node.to.split = leaves[[node.idx]]

      if (!is.null(node.to.split)) {

        node.to.split$single_split(Z = prepared_data$Z,
          Y = prepared_data$Y,
          min.node.size = min.node.size,
          n.quantiles = n.quantiles,
          impr.par = impr.par)

        node.to.split$create_children(Z = prepared_data$Z, Y = prepared_data$Y)

        tree[[depth + 1]] = c(tree[[depth + 1]],
                              list(node.to.split$children[[1]],
                                   node.to.split$children[[2]]))
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))
      }
    }
  }
  return(tree)
}
