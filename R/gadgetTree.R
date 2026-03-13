#' GadgetTree: Generalized Additive Decomposition of Global Effects Tree (R6 class)
#'
#' @description
#' Wrapper for effect-based trees: given a strategy (pd/ale), fits tree via
#' \code{$fit()}, plots effects via \code{$plot()}, extracts splits via
#' \code{$extract_split_info()}.
#' Delegates all effect logic to the strategy.
#'
#' @field strategy Strategy object (e.g., PdStrategy) that implements effect-specific logic.
#' @field root Node object. Root node of the tree.
#' @field n_split Integer. Maximum number of splits (tree depth minus one).
#' @field impr_par Numeric. Improvement threshold for splitting.
#' @field min_node_size Integer. Minimum number of samples required in a node to allow further splitting.
#' @field n_quantiles Integer or NULL. Number of quantiles for candidate split points (numeric features).
#' @field split_benchmark List. Timing information for each split (for benchmarking).
#'
#' @details
#' This class manages the overall tree structure and delegates effect-specific operations
#' (such as splitting and plotting) to the provided strategy object. It is the main entry
#' point for fitting and visualizing effect-based decision trees in the gadget package.
#'
#' @examples
#' \dontrun{
#' # Example: Fit and plot a PD tree using PdStrategy and GadgetTree
#' pd_strat = PdStrategy$new()
#' tree = GadgetTree$new(strategy = pd_strat, n_split = 2)
#' tree$fit(effect, data, target_feature_name = "target")
#' tree$plot(effect, data, target_feature_name = "target")
#' tree$plot_tree_structure()
#' split_info = tree$extract_split_info()
#' }
#'
#' @export
GadgetTree = R6::R6Class(
  "GadgetTree",
  public = list(
    strategy = NULL,
    root = NULL,
    n_split = NULL,
    impr_par = NULL,
    min_node_size = NULL,
    n_quantiles = NULL,
    split_benchmark = NULL,

    #' @description
    #' Given strategy and tree params (n_split, impr_par, min_node_size,
    #' n_quantiles): stores them and initializes empty \code{split_benchmark}.
    #' Returns the GadgetTree instance.
    #' @param strategy Strategy object (e.g., PdStrategy) that implements effect-specific logic.
    #' @param n_split Integer. Maximum number of splits (tree depth minus one).
    #' @param impr_par Numeric. Improvement threshold for splitting.
    #' @param min_node_size Integer. Minimum number of samples required in a node to allow further splitting.
    #' @param n_quantiles Integer or NULL. Number of quantiles for candidate split points (numeric features).
    initialize = function(strategy, n_split = 2, impr_par = 0.1, min_node_size = 10, n_quantiles = NULL) {
      checkmate::assert_integerish(n_split, len = 1, any.missing = FALSE, .var.name = "n_split")
      checkmate::assert_numeric(impr_par, lower = 0, len = 1, any.missing = FALSE, .var.name = "impr_par")
      checkmate::assert_integerish(min_node_size, len = 1, any.missing = FALSE, .var.name = "min_node_size")
      checkmate::assert_integerish(n_quantiles, len = 1, null.ok = TRUE, .var.name = "n_quantiles")
      self$strategy = strategy
      self$n_split = n_split
      self$impr_par = impr_par
      self$min_node_size = min_node_size
      self$n_quantiles = n_quantiles
      self$split_benchmark = list()
    },

    #' @description
    #' Given data, target_feature_name, and optional feature/split sets:
    #' calls \code{strategy$fit()} with ... (effect/model as required by
    #' strategy); clears \code{split_benchmark}; optionally runs
    #' \code{strategy$clean()}.
    #' Returns tree invisibly.
    #' @param data Data frame. Data frame containing features and the target variable.
    #' @param target_feature_name Character(1). Name of the target feature to explain.
    #' @param feature_set Character or NULL. \cr
    #'   Optional. Subset of features to use for effect calculation.
    #'   If NULL, all features are used.
    #' @param split_feature Character or NULL. \cr
    #'   Optional. Features to consider for splitting at each node.
    #'   If NULL, all features are considered.
    #' @param ... Additional arguments passed to the strategy's fit method.
    #' @return GadgetTree object, invisibly. The fitted tree object.
    fit = function(data, target_feature_name, feature_set = NULL, split_feature = NULL, ...) {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_character(feature_set, null.ok = TRUE, .var.name = "feature_set")
      checkmate::assert_character(split_feature, null.ok = TRUE, .var.name = "split_feature")
      self$split_benchmark = list()

      # The strategy is responsible for validating and handling its own arguments (via ...)
      result = self$strategy$fit(
        tree = self,
        data = data,
        target_feature_name = target_feature_name,
        feature_set = feature_set,
        split_feature = split_feature,
        ...
      )

      # Cleanup hook
      if (exists("clean", envir = self$strategy) && is.function(self$strategy$clean)) {
        self$strategy$clean()
      }
      invisible(result)
    },

    #' @description
    #' Given effect (or NULL for ale cached effect), data,
    #' target_feature_name, and optional depth/node_id/features: converts
    #' root to depth-list; calls \code{strategy$plot()}.
    #' Returns list of ggplot2 objects.
    #' @param effect R6 object or list. Object containing feature effect results.
    #' @param data Data frame. Data frame.
    #' @param target_feature_name Character(1). Name of the target feature to explain.
    #' @param depth Integer or NULL. Depth(s) to visualize (optional).
    #' @param node_id Integer or NULL. Node id(s) to visualize (optional).
    #' @param features Character or NULL. Features to visualize (optional).
    #' @param ... Additional plotting arguments.
    #' @return List. List of ggplot2 objects for different depths and nodes.
    plot = function(effect = NULL, data, target_feature_name, depth = NULL, node_id = NULL, features = NULL, ...) {
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node_id, lower = 1, null.ok = TRUE, .var.name = "node_id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      tree_for_plot = convert_tree_to_list(self$root, self$n_split + 1)
      self$strategy$plot(tree = tree_for_plot, effect = effect, data = data,
        target_feature_name = target_feature_name,
        depth = depth, node_id = node_id, features = features, ...)
    },

    #' @description
    #' Converts root to depth-list and calls \code{plot_tree_structure()}. Prints graph.
    plot_tree_structure = function() {
      tree_for_structure = convert_tree_to_list(self$root, self$n_split + 1)
      plot_tree_structure(tree_for_structure)
    },

    #' @description
    #' Converts root to depth-list and calls \code{extract_split_info()} with \code{split_benchmark}.
    #' Returns data frame (depth, id, split_feature, split_value, intImp, etc.).
    #' @return Data frame. Split information for all nodes.
    extract_split_info = function() {
      tree_for_info = convert_tree_to_list(self$root, self$n_split + 1)
      extract_split_info(tree_for_info, split_benchmark = self$split_benchmark)
    }
  )
)
