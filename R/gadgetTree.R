#' gadgetTree: Generalized Additive Decomposition of Global Effects Tree (R6 class)
#'
#' Main class for growing, managing, and visualizing effect-based decision trees
#' using a modular strategy (e.g., pdStrategy).
#'
#' @field strategy Strategy object (e.g., pdStrategy) that implements effect-specific logic.
#' @field root Node object. Root node of the tree.
#' @field n.split Integer. Maximum number of splits (tree depth minus one).
#' @field impr.par Numeric. Improvement threshold for splitting.
#' @field min.node.size Integer. Minimum number of samples required in a node to allow further splitting.
#' @field n.quantiles Integer or NULL. Number of quantiles for candidate split points (numeric features).
#' @field split_benchmark List. Timing information for each split (for benchmarking).
#'
#' @details
#' This class manages the overall tree structure and delegates effect-specific operations
#' (such as splitting and plotting) to the provided strategy object. It is the main entry
#' point for fitting and visualizing effect-based decision trees in the gadget package.
#'
#' @examples
#' # Example: Fit and plot a PD tree using pdStrategy and gadgetTree
#' pd_strat = pdStrategy$new()
#' tree = gadgetTree$new(strategy = pd_strat, n.split = 2)
#' tree$fit(effect, data, target.feature.name = "target")
#' tree$plot(effect, data, target.feature.name = "target")
#' tree$plot_tree_structure()
#' split_info = tree$extract_split_info()
#'
#' @export
gadgetTree = R6::R6Class(
  "gadgetTree",
  public = list(
    strategy = NULL,
    root = NULL,
    n.split = NULL,
    impr.par = NULL,
    min.node.size = NULL,
    n.quantiles = NULL,
    split_benchmark = NULL,

    #' @description
    #' Constructor for the gadgetTree object.
    #' @param strategy Strategy object (e.g., pdStrategy) that implements effect-specific logic.
    #' @param n.split Integer. Maximum number of splits (tree depth minus one).
    #' @param impr.par Numeric. Improvement threshold for splitting.
    #' @param min.node.size Integer. Minimum number of samples required in a node to allow further splitting.
    #' @param n.quantiles Integer or NULL. Number of quantiles for candidate split points (numeric features).
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

    #' @description
    #' Fit the tree to the provided data and effect object.
    #' @param data Data frame. Data frame containing features and the target variable.
    #' @param target.feature.name Character(1). Name of the target feature to explain.
    #' @param feature.set Character or NULL. \cr
    #'   Optional. Subset of features to use for effect calculation.
    #'   If NULL, all features are used.
    #' @param split.feature Character or NULL. \cr
    #'   Optional. Features to consider for splitting at each node.
    #'   If NULL, all features are considered.
    #' @param ... Additional arguments passed to the strategy's fit method.
    #' @return gadgetTree object, invisibly. The fitted tree object.
    fit = function(data, target.feature.name, feature.set = NULL, split.feature = NULL, ...) {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")
      self$split_benchmark = list()
      # Common arguments for all strategies
      common.args = list(
        tree = self,
        data = data,
        target.feature.name = target.feature.name,
        feature.set = feature.set,
        split.feature = split.feature
      )
      .dots = list(...)
      # Strategy-specific validation and argument preparation
      if (inherits(self$strategy, "aleStrategy")) {
        if (is.null(.dots$model)) stop("aleStrategy requires 'model' to be passed via ...", call. = FALSE)
        if (is.null(.dots$n.intervals)) stop("aleStrategy requires 'n.intervals' to be passed via ...", call. = FALSE)
        checkmate::assert_integerish(.dots$n.intervals, len = 1, lower = 1, .var.name = "n.intervals")
        checkmate::assert_function(.dots$predict.fun, null.ok = TRUE, .var.name = "predict.fun")
      } else if (inherits(self$strategy, "pdStrategy")) {
        if (is.null(.dots$effect)) stop("pdStrategy requires 'effect' to be passed via ...", call. = FALSE)
        checkmate::assert_true(is.list(.dots$effect) || inherits(.dots$effect, "R6"), .var.name = "effect")
      } else {
        stop("Unknown strategy type: please ensure the strategy implements a compatible 'fit' signature.", call. = FALSE)
      }
      # Call strategy's fit method with combined arguments
      result = do.call(self$strategy$fit, c(common.args, .dots))

      # Clean up large objects from strategy to reduce object size
      if (inherits(self$strategy, "aleStrategy")) {
        # Explicitly clean to remove data and model references
        self$strategy$clean()
      }
      invisible(result)
    },

    #' @description
    #' Visualize the tree structure and effect plots for each node.
    #' @param effect R6 object or list. Object containing feature effect results.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Name of the target feature to explain.
    #' @param depth Integer or NULL. Depth(s) to visualize (optional).
    #' @param node.id Integer or NULL. Node id(s) to visualize (optional).
    #' @param features Character or NULL. Features to visualize (optional).
    #' @param ... Additional plotting arguments.
    #' @return List. List of ggplot2 objects for different depths and nodes.
    plot = function(effect, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...) {
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      tree_for_plot = convert_tree_to_list(self$root, self$n.split + 1)
      self$strategy$plot(tree = tree_for_plot, effect = effect, data = data,
        target.feature.name = target.feature.name,
        depth = depth, node.id = node.id, features = features, ...)
    },

    #' @description
    #' Visualize the tree structure as a graph.
    #' @return NULL
    plot_tree_structure = function() {
      tree_for_structure = convert_tree_to_list(self$root, self$n.split + 1)
      plot_tree_structure(tree_for_structure)
    },

    #' @description
    #' Extract split information for all nodes in the tree.
    #' @return Data frame. Split information for all nodes.
    extract_split_info = function() {
      tree_for_info = convert_tree_to_list(self$root, self$n.split + 1)
      extract_split_info(tree_for_info, split_benchmark = self$split_benchmark)
    }
  )
)
