#' aleStrategy: Accumulated Local Effects Tree Strategy (R6 class)
#'
#' Implements the effectStrategy interface for building and analyzing Accumulated Local Effects (ALE) trees.
#' This strategy supports data preprocessing, node transformation, heterogeneity calculation,
#' best split search, tree fitting, and visualization.
#'
#' @field tree_ref Reference to the associated tree instance.
#'
#' @details
#' This class is used internally by the gadgetTree framework to implement ALE-based
#' tree growing, splitting, and visualization. It is not intended to be used directly by end users,
#' but can be instantiated for advanced customization.
#'
#' @examples
#' # Example: Fit and plot an ALE tree using aleStrategy and gadgetTree
#' # (Assuming model and data are prepared)
#' ale_strat = aleStrategy$new()
#' tree = gadgetTree$new(strategy = ale_strat, n.split = 2)
#' tree$fit(model = model, data = data, target.feature.name = "target")
#' tree$plot(model = model, data = data, target.feature.name = "target")
#'
#' @keywords internal
default_predict_fun = function(model, data) {
  pred = model$predict_newdata(data)
  if (inherits(pred, "Prediction")) pred$response else pred
}

aleStrategy = R6::R6Class(
  "aleStrategy",
  inherit = effectStrategy,
  public = list(
    tree_ref = NULL,
    # Persistent context fields (declared to allow assignment outside initialize)
    model = NULL,
    data = NULL,
    target.feature.name = NULL,
    n.intervals = NULL,
    predict.fun = NULL,

    #' @description
    #' Initialize the strategy with name "ale".
    initialize = function() {
      self$name = "ale"
    },

    #' @description
    #' Preprocess input data to generate the split feature set Z, ALE effect list Y, and grid.
    #' @param model Fitted model object.
    #' @param data Data frame containing all features and the target variable.
    #' @param target.feature.name Character(1). The name of the target feature in the data to explain.
    #' @param n.intervals Integer. Number of intervals for numeric features.
    #' @param feature.set Character or NULL. Optional. Subset of features to use for effect calculation. If NULL, all features are used.
    #' @param split.feature Character or NULL. Optional. Features to consider for splitting at each node. If NULL, all features are considered.
    #' @param predict.fun Function or NULL. Prediction function. If NULL, uses default mlr3-compatible function.
    #' @return List. A list with Z (split feature set), Y (ALE effect list), and grid (feature grid).
    preprocess = function(model, data, target.feature.name, n.intervals, feature.set = NULL, split.feature = NULL, predict.fun = NULL) {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_integerish(n.intervals, len = 1, lower = 1, .var.name = "n.intervals")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")
      checkmate::assert_function(predict.fun, null.ok = TRUE, .var.name = "predict.fun")

      prepare_split_data_ale(model = model, data = data, target.feature.name = target.feature.name,
        n.intervals = n.intervals, feature.set = feature.set, split.feature = split.feature, predict.fun = predict.fun)
    },

    #' @description
    #' Transform ALE effects for a specific node (e.g., after splitting).
    #' This method applies node-specific preprocessing including single value handling
    #' and categorical feature recalculation.
    #' @param Y List. List of ALE effect data for each feature.
    #' @param idx Integer vector. Indices of samples in the current node.
    #' @param split.feature Character(1) or NULL. Name of the feature used for splitting.
    #' @return List. List of transformed ALE effects.
    node_transform = function(Y, idx, split.feature = NULL) {
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")
      checkmate::assert_integerish(idx, min.len = 1, .var.name = "idx")
      checkmate::assert_character(split.feature, len = 1, null.ok = TRUE, .var.name = "split.feature")

      node_transform_ale(
        Y = Y,
        idx = idx,
        split.feature = split.feature,
        model = self$model,
        data = self$data,
        target.feature.name = self$target.feature.name,
        n.intervals = self$n.intervals,
        predict.fun = self$predict.fun
      )
    },

    #' @description
    #' Calculate the heterogeneity (variance of ALE derivatives) within the node.
    #' @param Y List. List of ALE effect data.
    #' @return Numeric. Numeric vector, heterogeneity for each feature.
    #' @seealso \code{\link{calculate_ale_heterogeneity_cpp}} for the underlying calculation function
    heterogeneity = function(Y) {
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")

      unlist(calculate_ale_heterogeneity_cpp(Y))
    },

    #' @description
    #' Find the best split point for a node based on ALE heterogeneity reduction.
    #' @param Z Data frame. Split feature set.
    #' @param Y List. ALE effect data.
    #' @param min.node.size Integer(1). Minimum node size.
    #' @param n.quantiles Integer(1) or NULL. Number of quantiles to use for candidate split points (for numeric features).
    #' @return List. List with best split feature, split point, etc.
    find_best_split = function(Z, Y, min.node.size, n.quantiles) {
      checkmate::assert_true(data.table::is.data.table(Z) || is.data.frame(Z), .var.name = "Z")
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")
      checkmate::assert_integerish(min.node.size, len = 1, lower = 1, any.missing = FALSE, .var.name = "min.node.size")
      checkmate::assert_integerish(n.quantiles, len = 1, lower = 1, null.ok = TRUE, .var.name = "n.quantiles")

      search_best_split_ale(
        Z = Z,
        effect = Y,
        min.node.size = min.node.size,
        n.quantiles = n.quantiles
      )
    },

    #' @description
    #' Visualize the ALE tree structure and ALE plots for each node.
    #' @param tree List. Tree structure as a list of Node objects.
    #' @param model Fitted model object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param depth Integer or NULL. Depth(s) to visualize (optional).
    #' @param node.id Integer or NULL. Node id(s) to visualize (optional).
    #' @param features Character or NULL. Features to visualize (optional).
    #' @param ... Additional plotting arguments.
    #' @return List. List of ggplot2 objects for different depths and nodes.
    plot = function(tree, model, data, target.feature.name, depth = NULL, node.id = NULL, features = NULL, ...) {
      checkmate::assert_list(tree)
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")

      # TODO: Implement ALE-specific plotting
      # For now, return a placeholder
      stop("ALE plotting not yet implemented. Use plot_tree_ale() function.")
    },

    #' @description
    #' Fit an ALE tree using the provided data and model.
    #' @param tree gadgetTree object. Tree object instance.
    #' @param model Fitted model object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param h Integer. Number of intervals for numeric features.
    #' @param feature.set Character or NULL. Feature subset (optional).
    #' @param split.feature Character or NULL. Split feature (optional).
    #' @param predict.fun Function or NULL. Prediction function (optional).
    #' @return gadgetTree object, invisibly. The fitted tree object.
    fit = function(tree, model, data, target.feature.name, n.intervals, feature.set = NULL, split.feature = NULL, predict.fun = NULL) {
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      checkmate::assert_integerish(n.intervals, len = 1, .var.name = "n.intervals")

      # Default prediction function for mlr3 models compatibility
      if (is.null(predict.fun)) {
        predict.fun = default_predict_fun
      }
      # Store parameters for ALE-specific splitting
      self$model = model
      self$data = data
      self$target.feature.name = target.feature.name
      self$n.intervals = n.intervals
      self$predict.fun = predict.fun
      # Keep reference to the tree
      self$tree_ref = tree
      # Run preprocess internally to obtain Z, Y, grid
      prepared.data = self$preprocess(model = model, data = data,
        target.feature.name = target.feature.name, n.intervals = n.intervals,
        feature.set = feature.set, split.feature = split.feature, predict.fun = predict.fun)
      Z = prepared.data$Z
      Y = prepared.data$Y
      # Force grid to be empty structure for aleStrategy to save memory
      # regardless of what preprocess returns
      grid = vector("list", length(names(Z)))
      names(grid) = names(Z)
      # Root objective
      objective.value.root.j = self$heterogeneity(Y)
      objective.value.root = sum(unlist(objective.value.root.j), na.rm = TRUE)
      # Create root node
      parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
        objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
        objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL,
        strategy = self)
      # Recursively build the tree
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
      invisible(tree)
    },
    #' @description
    #' Clean up large objects (data, model) after fitting to save memory.
    clean = function() {
      self$data = NULL
      self$model = NULL
    }
  )
)
