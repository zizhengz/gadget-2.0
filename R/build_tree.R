#' Build a Feature Effect Tree
#'
#' Constructs a binary tree based on feature effects, splitting nodes according to heterogeneity in the effect profiles.
#' The tree is built recursively up to a specified depth or until improvement thresholds are met.
#'
#' @param effect (`R6` object or list)\cr
#'   An object containing feature effect results, typically from `FeatureEffect` or `FeatureEffects`.
#' @param data (`data.frame`)\cr
#'   Data frame containing the features and the target variable.
#' @param effect.method (`character(1)`)\cr
#'   The method for effect calculation. Currently only `'pd'` (partial dependence) is supported.
#' @param feature.set (`character` or `NULL`)\cr
#'   Optional. Subset of features to use for effect calculation. If `NULL`, all features are used.
#' @param split.feature (`character` or `NULL`)\cr
#'   Optional. Features to consider for splitting at each node. If `NULL`, all features are considered.
#' @param target.feature.name (`character(1)`)\cr
#'   The name of the target feature in the data to explain.
#' @param n.split (`integer(1)`)\cr
#'   The maximum depth of the tree (number of splits to perform).
#' @param impr.par (`numeric(1)`)\cr
#'   Improvement threshold parameter for splitting. Controls minimum improvement required to split a node.
#' @param min.node.size (`integer(1)`)\cr
#'   Minimum number of samples required in a node to allow further splitting.
#' @param n.quantiles (`integer(1)` or `NULL`)\cr
#'   Optional. Number of quantiles to use for candidate split points (for numeric features). If `NULL`, use default.
#'
#' @return (`list`)\cr
#'   A list of lists representing the tree structure. Each element is a list of Node objects (or `NULL` for terminal nodes) at a given depth.
#'
#' @details
#'   The function prepares the data, initializes the root node, and recursively splits nodes based on reduction in heterogeneity of the effect profiles. Splitting stops when the improvement is below a threshold or the minimum node size is reached.
#'
#' @examples
#' # Example usage (assuming effect and data are prepared):
#' # tree <- build_tree(effect, data, target.feature.name = "target")
#' @export
build_tree = function(effect, data, effect.method = "pd", feature.set = NULL, split.feature = NULL,
  target.feature.name, n.split = 2, impr.par = 0.1,
  min.node.size = 10, n.quantiles = NULL) {

  checkmate::assert_data_frame(data)
  if (is.null(target.feature.name) || target.feature.name == "") {
    stop("Please choose a feature name from your data as 'target.feature.name'.")
  }

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

  # Initialize the parent node of the tree
  parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
    objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
    objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL)

  max_depth = n.split + 1
  tree = vector("list", max_depth)
  for (d in seq_len(max_depth)) {
    tree[[d]] = vector("list", 2^(d - 1))
  }

  tree[[1]][[1]] = parent
  for (depth in seq_len(n.split)) {
    for (node.idx in seq_len(length(tree[[depth]]))) {
      node.to.split = tree[[depth]][[node.idx]]
      if (!is.null(node.to.split)) {
        node.to.split$split_node(Z = Z, Y = Y,
          effect.method = effect.method,
          objective.value.root.j = objective.value.root.j,
          objective.value.root = objective.value.root,
          min.node.size = min.node.size,
          n.quantiles = n.quantiles,
          impr.par = impr.par)
        tree[[depth + 1]][[2 * node.idx - 1]] = node.to.split$children[[1]]
        tree[[depth + 1]][[2 * node.idx]] = node.to.split$children[[2]]
      }
    }
  }
  return(tree)
}
