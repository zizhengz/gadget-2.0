build_tree = function(effect, data, effect.method = "pd", feature.set = NULL, split.feature = NULL,
  target.feature.name, n.split = 2, impr.par = 0.1,
  min.node.size = 10, n.quantiles = NULL) {

  checkmate::assert_data_frame(data)

  if (effect.method == "pd") {
    prepared.data = prepare_split_data_pd(effect = effect, data = data,
      target.feature.name = target.feature.name,
      feature.set = feature.set, split.feature = split.feature)
    Z = prepared.data$Z
    Y = prepared.data$Y
    grid = prepared.data$grid
  }
  objective.value.root.j = node_heterogeneity(Y)
  objective.value.root = sum(objective.value.root.j, na.rm = TRUE)
  #semi.objective.value.root.j = node_heterogeneity(Y, full.formula = FALSE)
  #semi.objective.value.root = sum(semi.objective.value.root.j, na.rm = TRUE)
  # Initialize the parent node of the tree
  parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
    objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
    objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL)

  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    nodes = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(nodes)) {

      node.to.split = nodes[[node.idx]]

      if (!is.null(node.to.split)) {
        node.to.split$split_node(Z = Z, Y = Y,
          effect.method = effect.method,
          objective.value.root.j = objective.value.root.j,
          objective.value.root = objective.value.root,
          min.node.size = min.node.size,
          n.quantiles = n.quantiles,
          impr.par = impr.par)

        tree[[depth + 1]] = c(tree[[depth + 1]], list(node.to.split$children[[1]], node.to.split$children[[2]]))
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))
      }
    }
  }
  return(tree)
}
