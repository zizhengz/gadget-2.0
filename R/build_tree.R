build_tree = function(effect, data, effect.method = "PD", split.feature = NULL,
  target.feature.name, n.split = 2, impr.par = 0.1,
  min.node.size = 10, n.quantiles = NULL) {

  checkmate::assert_data_frame(data)

  if (effect.method == "PD") {
    prepared_data = prepare_data_for_ice_splitting(effect = effect, data = data,
      target.feature.name = target.feature.name,
      split.feature = split.feature)

    Z = prepared_data$Z
    Y = prepared_data$Y
    grid = prepared_data$grid

    objective.value.root = sum(unlist(lapply(Y, function(Y_i) {
      sum(Rfast::colsums(as.matrix(Y_i)^2) - Rfast::colsums(as.matrix(Y_i))^2 / nrow(Y_i), na.rm = TRUE)
    })), na.rm = TRUE)
  }
  # Initialize the parent node of the tree
  parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)),
    grid = grid, improvement.met = FALSE, intImp = NULL)

  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    leaves = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(leaves)) {

      node.to.split = leaves[[node.idx]]

      if (!is.null(node.to.split)) {

        node.to.split$do_single_split(Z = Z, Y = Y,
          effect.method = effect.method,
          objective.value.root = objective.value.root,
          min.node.size = min.node.size,
          n.quantiles = n.quantiles,
          impr.par = impr.par)

        node.to.split$create_children(Z = Z, Y = Y)

        tree[[depth + 1]] = c(tree[[depth + 1]], list(node.to.split$children[[1]], node.to.split$children[[2]]))
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))
      }
    }
  }
  return(tree)
}
