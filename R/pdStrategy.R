#' @title pdStrategy: Generalized additive decomposition based on PD effects.
#'
#' @description
#' PD-based strategy: given effect and data, preprocesses to Z/Y/grid; mean-centers effects per node; computes sum-of-variances heterogeneity; 
#' finds best split via C++; fits tree and plots PD/ICE.
#'
#' @field name Character. Strategy name (e.g., \code{"pd"}).
#' @field tree_ref Reference to the associated tree instance.
#' @field fit_timing Named numeric vector with fit times (seconds), or NULL.
#'
#' @details
#' This class is used internally by the gadgetTree framework to implement partial dependence
#' tree growing, splitting, and visualization. It is not intended to be used directly by end users,
#' but can be instantiated for advanced customization.
#'
#' @examples
#' \dontrun{
#' # Example: Fit and plot a PD tree using pdStrategy and gadgetTree
#' # (Assuming effect and data are prepared)
#' pd_strat = pdStrategy$new()
#' tree = gadgetTree$new(strategy = pd_strat, n.split = 2)
#' tree$fit(effect, data, target.feature.name = "target")
#' tree$plot(effect, data, target.feature.name = "target")
#' }
#'
#' @export
pdStrategy = R6::R6Class(
  "pdStrategy",
  public = list(
    name = NULL,
    tree_ref = NULL,
    fit_timing = NULL,

    #' @description
    #' Sets \code{name = "pd"}. Returns the strategy instance.
    initialize = function() {
      self$name = "pd"
    },

    #' @description
    #' Given effect, data, target.feature, and optional feature/split sets: validates features; converts character to factor; 
    #' builds Z (split features) and mean-centered Y via \code{prepare_split_data_pd}. Returns list \code{Z}, \code{Y}, \code{grid}.
    #' @param effect R6 object or list.\cr
    #'   An object containing feature effect results, typically from FeatureEffect or FeatureEffects.
    #' @param data Data frame.\cr
    #'   Data frame containing all features and the target variable.
    #' @param target.feature Character(1).\cr
    #'   The name of the target feature in the data to explain.
    #' @param feature.set Character or NULL. \cr
    #'   Optional. Subset of features to use for effect calculation. If NULL, all features are used.
    #' @param split.feature Character or NULL. \cr
    #'   Optional. Features to consider for splitting at each node. If NULL, all features are considered.
    #' @return List. \cr
    #'   A list with Z (split feature set), Y (effect list), and grid (feature grid).
    preprocess = function(effect, data, target.feature, feature.set = NULL,
      split.feature = NULL) {
      prepare_split_data_pd(effect = effect, data = data, target.feature.name = target.feature,
        feature.set = feature.set, split.feature = split.feature)
    },

    #' @description
    #' Given Y (list of effect matrices), grid, and idx: subset rows by idx, mean-center each matrix per row, 
    #' return list of centered matrices via \code{re_mean_center_ice_cpp}.
    #' @param Y List. Each element is an effect matrix for a feature.
    #' @param grid List. Feature grids.
    #' @param idx Integer. Sample indices for the current node.
    #' @return List. List of centered effects.
    node_transform = function(Y, grid, idx) {
      checkmate::assert_list(Y)
      checkmate::assert_list(grid)
      # Ensure Y elements are numeric matrices (avoid tibble -> C++ type mismatch)
      Y = lapply(Y, function(m) {
        m = as.matrix(m)
        storage.mode(m) = "double"
        m
      })
      re_mean_center_ice_cpp(Y = Y, grid = grid, idx = idx)
    },

    #' @description
    #' Given Y (list of effect matrices): computes sum of column variances per matrix via \code{node_heterogeneity}. 
    #' Returns numeric vector of length \code{length(Y)}.
    #' @param Y List. List of effect matrices.
    #' @return Numeric. Numeric vector, heterogeneity for each feature.
    heterogeneity = function(Y) {
      checkmate::assert_list(Y)
      node_heterogeneity(Y)
    },

    #' @description
    #' Given Z, Y, min.node.size, n.quantiles: calls \code{search_best_split_cpp} to evaluate all features and returns data frame with \code{split.feature}, 
    #' \code{split.point}, \code{split.objective}, \code{best.split}, etc.
    #' @param Z Data frame. Split feature set.
    #' @param Y List. Effect matrices.
    #' @param min.node.size Integer(1). Minimum node size.
    #' @param n.quantiles Integer(1) or NULL. \cr
    #'   Optional. Number of quantiles to use for candidate split points (for numeric features).
    #'   If NULL, use default.
    #' @return List. List with best split feature, split point, etc.
    find_best_split = function(Z, Y, min.node.size, n.quantiles) {
      checkmate::assert_data_frame(Z)
      checkmate::assert_list(Y)
      checkmate::assert_integerish(min.node.size, len = 1, any.missing = FALSE, .var.name = "min.node.size")
      checkmate::assert_integerish(n.quantiles, len = 1, null.ok = TRUE, .var.name = "n.quantiles")
      search_best_split_cpp(Z = Z, Y = Y, min_node_size = min.node.size, n_quantiles = n.quantiles)
    },

    #' @description
    #' Given tree, effect, data, target.feature.name, and optional depth/node.id/features: prepares plot data and calls \code{plot_tree_pd}. 
    #' Returns list of ggplot2 objects (by depth and node).
    #' @param tree List. Tree structure as a list of Node objects.
    #' @param effect R6 object or list. Model effect object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param depth Integer or NULL. Depth(s) to visualize (optional).
    #' @param node.id Integer or NULL. Node id(s) to visualize (optional).
    #' @param features Character or NULL. Features to visualize (optional).
    #' @param ... Additional plotting arguments.
    #' @return List. List of ggplot2 objects for different depths and nodes.
    plot = function(tree, effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...) {
      checkmate::assert_list(tree)
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      plot_tree_pd(tree = tree, effect = effect, data = data,
        target.feature.name = target.feature.name,
        depth = depth, node.id = node.id, features = features, ...)
    },

    #' @description
    #' Given tree, effect, data, target.feature.name: preprocesses Z/Y/grid; creates root Node; recursively splits; records fit time in \code{fit_timing}. 
    #' Returns tree invisibly.
    #' @param tree gadgetTree object. Tree object instance.
    #' @param effect R6 object or list. Model effect object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param feature.set Character or NULL. Feature subset (optional).
    #' @param split.feature Character or NULL. Split feature (optional).
    #' @param ... Additional arguments (ignored).
    #' @return gadgetTree object, invisibly. The fitted tree object.
    fit = function(tree, effect, data, target.feature.name,
      feature.set = NULL, split.feature = NULL, ...) {
      if (missing(effect)) stop("pdStrategy requires 'effect' to be passed.", call. = FALSE)
      checkmate::assert_true(is.list(effect) || inherits(effect, "R6"), .var.name = "effect")

      self$tree_ref = tree
      prepared.data = self$preprocess(effect = effect, data = data,
        target.feature = target.feature.name,
        feature.set = feature.set,
        split.feature = split.feature)
      Z = prepared.data$Z
      Y = prepared.data$Y
      grid = prepared.data$grid
      objective.value.root.j = self$heterogeneity(Y)
      objective.value.root = sum(objective.value.root.j, na.rm = TRUE)
      parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
        objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
        objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL,
        strategy = self)
      # Recursively build the tree from the root node (regional part, timed)
      t_regional = system.time({
        parent$split_node(
          Z = Z, Y = Y,
          objective.value.root.j = objective.value.root.j,
          objective.value.root = objective.value.root,
          min.node.size = tree$min.node.size,
          n.quantiles = tree$n.quantiles,
          impr.par = tree$impr.par,
          depth = 1,
          max.depth = tree$n.split + 1
        )
        tree$root = parent
      })[["elapsed"]]
      self$fit_timing = list(regional = t_regional)
      invisible(tree)
    }
  )
)
