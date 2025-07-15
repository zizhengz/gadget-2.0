#' gadgetTree: Generalized Additive Decision Tree (R6 class)
#'
#' Main class for growing, managing, and visualizing effect-based decision trees using a modular strategy (e.g., pdStrategy).
#'
#' @section Fields:
#' \describe{
#'   \item{strategy}{Strategy object (e.g., pdStrategy) that implements effect-specific logic.}
#'   \item{root}{Root node of the tree (Node object).}
#'   \item{tree}{List of lists representing the tree structure at each depth.}
#'   \item{n.split}{Maximum number of splits (tree depth minus one).}
#'   \item{impr.par}{Improvement threshold for splitting.}
#'   \item{min.node.size}{Minimum number of samples required in a node to allow further splitting.}
#'   \item{n.quantiles}{Number of quantiles for candidate split points (numeric features).}
#'   \item{split_benchmark}{List of timing information for each split (for benchmarking).}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(strategy, n.split = 2, impr.par = 0.1, min.node.size = 10, n.quantiles = NULL)}{
#'     Constructor for the tree object.
#'     @param strategy (R6 object)\cr
#'       Strategy object implementing effect-specific logic (e.g., pdStrategy).
#'     @param n.split (integer(1))\cr
#'       Maximum number of splits (tree depth minus one).
#'     @param impr.par (numeric(1))\cr
#'       Improvement threshold for splitting.
#'     @param min.node.size (integer(1))\cr
#'       Minimum number of samples required in a node to allow further splitting.
#'     @param n.quantiles (integer(1) or NULL)\cr
#'       Number of quantiles for candidate split points (numeric features). If NULL, use default.
#'   }
#'   \item{fit(effect, data, target.feature.name, feature.set = NULL, split.feature = NULL)}{
#'     Fit the tree to the provided data and effect object.
#'     @param effect (R6 object or list)\cr
#'       Object containing feature effect results.
#'     @param data (data.frame)\cr
#'       Data frame containing features and the target variable.
#'     @param target.feature.name (character(1))\cr
#'       Name of the target feature to explain.
#'     @param feature.set (character or NULL)\cr
#'       Optional. Subset of features to use for effect calculation. If NULL, all features are used.
#'     @param split.feature (character or NULL)\cr
#'       Optional. Features to consider for splitting at each node. If NULL, all features are considered.
#'     @return (gadgetTree object, invisibly)\cr
#'       The fitted tree object.
#'   }
#'   \item{plot(effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...)}{
#'     Visualize the tree structure and effect plots for each node.
#'     @param effect (R6 object or list)\cr
#'       Object containing feature effect results.
#'     @param data (data.frame)\cr
#'       Data frame.
#'     @param target.feature.name (character(1))\cr
#'       Name of the target feature to explain.
#'     @param depth (integer or NULL)\cr
#'       Depth(s) to visualize (optional).
#'     @param node.id (integer or NULL)\cr
#'       Node id(s) to visualize (optional).
#'     @param features (character or NULL)\cr
#'       Features to visualize (optional).
#'     @param ... Additional plotting arguments.
#'     @return (list)\cr
#'       List of ggplot2 objects for different depths and nodes.
#'   }
#'   \item{plot_tree_structure()}{Visualize the tree structure as a graph.}
#'   \item{extract_split_info()}{Extract split information for all nodes in the tree.}
#' }
#'
#' @details
#' This class manages the overall tree structure and delegates effect-specific operations (such as splitting and plotting) to the provided strategy object. It is the main entry point for fitting and visualizing effect-based decision trees in the gadget package.
#'
#' @examples
#' # Example: Fit and plot a PD tree using pdStrategy and gadgetTree
#' # (Assuming effect and data are prepared)
#' pd_strat <- pdStrategy$new()
#' tree <- gadgetTree$new(strategy = pd_strat, n.split = 2)
#' tree$fit(effect, data, target.feature.name = "target")
#' tree$plot(effect, data, target.feature.name = "target")
#' tree$plot_tree_structure()
#' split_info <- tree$extract_split_info()
#'
gadgetTree = R6::R6Class(
  "gadgetTree",
  public = list(
    strategy = NULL,
    root = NULL,
    tree = NULL,
    n.split = NULL,
    impr.par = NULL,
    min.node.size = NULL,
    n.quantiles = NULL,
    split_benchmark = NULL,

    initialize = function(strategy, n.split = 2, impr.par = 0.1, min.node.size = 10, n.quantiles = NULL) {
      checkmate::assert_integerish(n.split, len = 1, any.missing = FALSE, .var.name = "n.split")
      checkmate::assert_numeric(impr.par, lower = 0, len = 1, any.missing = FALSE, .var.name = "impr.par")
      checkmate::assert_integerish(min.node.size, len = 1, any.missing = FALSE, .var.name = "min.node.size")
      checkmate::assert_integerish(n.quantiles, len = 1, null.ok = TRUE, .var.name = "n.quantiles")
      self$strategy = strategy
      self$n.split = n.split
      self$impr.par = impr.par
      self$min.node.size = min.node.size
      self$n.quantiles = n.quantiles
      self$split_benchmark = list()
    },
    fit = function(effect, data, target.feature.name, feature.set = NULL, split.feature = NULL) {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")

      result = self$strategy$fit(tree_instance = self, effect = effect, data = data,
        target.feature.name = target.feature.name,
        feature.set = feature.set, split.feature = split.feature)

      invisible(result)
    },
    plot = function(effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...) {
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")

      self$strategy$plot(tree = self$tree, effect = effect, data = data,
        target.feature.name = target.feature.name,
        depth = depth, node.id = node.id, features = features, ...)
    },
    plot_tree_structure = function() {
      plot_tree_structure(self$tree)
    },
    extract_split_info = function() {
      extract_split_info(self$tree)
    }
    # flatten_to_dt = function() {
    #   nodes <- list()
    #   for (depth in seq_along(self$tree)) {
    #     for (node in self$tree[[depth]]) {
    #       if (!is.null(node)) {
    #         nodes[[length(nodes) + 1]] <- list(
    #           id = node$id,
    #           depth = node$depth,
    #           id.parent = node$id.parent,
    #           split.feature = node$split.feature,
    #           split.value = node$split.value,
    #           subset.idx = list(node$subset.idx),
    #           grid = list(node$grid)
    #         )
    #       }
    #     }
    #   }
    #   data.table::rbindlist(nodes, fill = TRUE)
    # }
  )
)
