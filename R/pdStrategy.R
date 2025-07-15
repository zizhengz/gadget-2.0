#' pdStrategy: Partial Dependence Tree Strategy (R6 class)
#'
#' Implements the effectStrategy interface for building and analyzing Partial Dependence (PD) trees.
#' This strategy supports data preprocessing, node transformation, heterogeneity calculation, best split search, tree fitting, and visualization.
#'
#' @section Fields:
#' \describe{
#'   \item{tree_ref}{Reference to the associated tree instance.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize()}{Initialize the strategy with name "pd".}
#'   \item{preprocess(effect, data, target.feature, feature.set = NULL, split.feature = NULL)}{
#'     Preprocess input data to generate the split feature set Z, effect list Y, and grid.
#'     @param effect (`R6` object or list)\cr
#'       An object containing feature effect results, typically from `FeatureEffect` or `FeatureEffects`.
#'     @param data (`data.frame`)\cr
#'       Data frame containing all features and the target variable.
#'     @param target.feature (`character(1)`)\cr
#'       The name of the target feature in the data to explain.
#'     @param feature.set (`character` or `NULL`)\cr
#'       Optional. Subset of features to use for effect calculation. If `NULL`, all features are used.
#'     @param split.feature (`character` or `NULL`)\cr
#'       Optional. Features to consider for splitting at each node. If `NULL`, all features are considered.
#'     @return (`list`)\cr
#'       A list with Z (split feature set), Y (effect list), and grid (feature grid).
#'   }
#'   \item{node_transform(Y, grid, idx)}{
#'     Mean-center the effect list Y within the node and return the centered list.
#'     @param Y (`list`)\cr
#'       Each element is an effect matrix for a feature.
#'     @param grid (`list`)\cr
#'       Feature grids.
#'     @param idx (`integer`)\cr
#'       Sample indices for the current node.
#'     @return (`list`)\cr
#'       List of centered effects.
#'   }
#'   \item{heterogeneity(Y)}{
#'     Calculate the heterogeneity (sum of variances) of effects within the node.
#'     @param Y (`list`)\cr
#'       List of effect matrices.
#'     @return (`numeric`)\cr
#'       Numeric vector, heterogeneity for each feature.
#'   }
#'   \item{find_best_split(Z, Y, min.node.size, n.quantiles)}{
#'     Find the best split point for a node.
#'     @param Z (`data.frame`)\cr
#'       Split feature set.
#'     @param Y (`list`)\cr
#'       Effect matrices.
#'     @param min.node.size (`integer(1)`)\cr
#'       Minimum node size.
#'     @param n.quantiles (`integer(1)` or `NULL`)\cr
#'       Optional. Number of quantiles to use for candidate split points (for numeric features). If `NULL`, use default.
#'     @return (`list`)\cr
#'       List with best split feature, split point, etc.
#'   }
#'   \item{plot(tree, effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...)}{
#'     Visualize the PD tree structure and partial dependence plots for each node.
#'     @param tree (`list`)\cr
#'       Tree structure as a list of Node objects.
#'     @param effect (`R6` object or list)\cr
#'       Model effect object.
#'     @param data (`data.frame`)\cr
#'       Data frame.
#'     @param target.feature.name (`character(1)`)\cr
#'       Target feature name.
#'     @param depth (`integer` or `NULL`)\cr
#'       Depth(s) to visualize (optional).
#'     @param node.id (`integer` or `NULL`)\cr
#'       Node id(s) to visualize (optional).
#'     @param features (`character` or `NULL`)\cr
#'       Features to visualize (optional).
#'     @param ... Additional plotting arguments.
#'     @return (`list`)\cr
#'       List of ggplot2 objects for different depths and nodes.
#'   }
#'   \item{fit(tree_instance, effect, data, target.feature.name, feature.set = NULL, split.feature = NULL)}{
#'     Fit a PD tree using the provided data and effect object.
#'     @param tree_instance (`gadgetTree` object)\cr
#'       Tree object instance.
#'     @param effect (`R6` object or list)\cr
#'       Model effect object.
#'     @param data (`data.frame`)\cr
#'       Data frame.
#'     @param target.feature.name (`character(1)`)\cr
#'       Target feature name.
#'     @param feature.set (`character` or `NULL`)\cr
#'       Feature subset (optional).
#'     @param split.feature (`character` or `NULL`)\cr
#'       Split feature (optional).
#'     @return (`gadgetTree` object, invisibly)\cr
#'       The fitted tree object.
#'   }
#' }
#'
#' @details
#' This class is used internally by the gadgetTree framework to implement partial dependence tree growing, splitting, and visualization. It is not intended to be used directly by end users, but can be instantiated for advanced customization.
#'
#' @examples
#' # Example: Fit and plot a PD tree using pdStrategy and gadgetTree
#' # (Assuming effect and data are prepared)
#' pd_strat <- pdStrategy$new()
#' tree <- gadgetTree$new(strategy = pd_strat, n.split = 2)
#' tree$fit(effect, data, target.feature.name = "target")
#' tree$plot(effect, data, target.feature.name = "target")
#'
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
