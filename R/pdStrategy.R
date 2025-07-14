#' pdStrategy (partial dependence strategy)
#'
#' Implements the effectStrategy interface for partial dependence (PD).
pdStrategy = R6::R6Class(
  "pdStrategy",
  inherit = effectStrategy,
  public = list(
    tree_ref = NULL,
    initialize = function() {
      self$name = "pd"
    },
    preprocess = function(effect, data, target.feature, feature.set = NULL, split.feature = NULL) {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature, len = 1, .var.name = "target.feature")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      prepare_split_data_pd(effect = effect, data = data, target.feature.name = target.feature,
        feature.set = feature.set, split.feature = split.feature)
    },
    node_transform = function(Y, grid, idx) {
      checkmate::assert_list(Y)
      checkmate::assert_list(grid)
      re_mean_center_ice(Y = Y, grid = grid, idx = idx)
    },
    heterogeneity = function(Y) {
      checkmate::assert_list(Y)
      node_heterogeneity(Y)
    },
    find_best_split = function(Z, Y, min.node.size, n.quantiles) {
      checkmate::assert_data_frame(Z)
      checkmate::assert_list(Y)
      checkmate::assert_integerish(min.node.size, len = 1, any.missing = FALSE, .var.name = "min.node.size")
      checkmate::assert_integerish(n.quantiles, len = 1, null.ok = TRUE, .var.name = "n.quantiles")
      search_best_split_cpp(Z = Z, Y = Y, min_node_size = min.node.size, n_quantiles = n.quantiles)
    },
    plot = function(tree, effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...) {
      checkmate::assert_list(tree)
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      plot_tree_pd(tree = tree, effect = effect, data = data, target.feature.name = target.feature.name,
        depth = depth, node.id = node.id, features = features, ...)
    },
    fit = function(tree_instance, effect, data, target.feature.name, feature.set = NULL, split.feature = NULL) {
      self$tree_ref = tree_instance
      prepared.data = self$preprocess(effect = effect, data = data, target.feature = target.feature.name,
                                     feature.set = feature.set, split.feature = split.feature)
      Z = prepared.data$Z
      Y = prepared.data$Y
      grid = prepared.data$grid

      objective.value.root.j = self$heterogeneity(Y)
      objective.value.root = sum(objective.value.root.j, na.rm = TRUE)

      parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
        objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
        objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL,
        strategy = self)

      max_depth = tree_instance$n.split + 1
      tree = vector("list", max_depth)
      for (d in seq_len(max_depth)) {
        tree[[d]] = vector("list", 2^(d - 1))
      }
      tree[[1]][[1]] = parent
      for (depth in seq_len(tree_instance$n.split)) {
        for (node.idx in seq_len(length(tree[[depth]]))) {
          node.to.split = tree[[depth]][[node.idx]]
          if (!is.null(node.to.split)) {
            node.to.split$split_node(
              Z = Z, Y = Y,
              objective.value.root.j = objective.value.root.j,
              objective.value.root = objective.value.root,
              min.node.size = tree_instance$min.node.size,
              n.quantiles = tree_instance$n.quantiles,
              impr.par = tree_instance$impr.par
            )
            tree[[depth + 1]][[2 * node.idx - 1]] = node.to.split$children[[1]]
            tree[[depth + 1]][[2 * node.idx]] = node.to.split$children[[2]]
          }
        }
      }
      tree_instance$tree = tree
      tree_instance$root = parent
      invisible(tree_instance)
    }
  )
)
