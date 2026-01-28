#' @keywords internal
default_predict_fun = function(model, data) {
  pred = model$predict_newdata(data)
  if (inherits(pred, "Prediction")) pred$response else pred
}

#' aleStrategy: ALE-Based Effect Strategy (R6)
#'
#' Implements the effectStrategy interface for ALE-based trees: preprocessing via
#' \code{prepare_split_data_ale}, node-wise ALE transform, heterogeneity from ALE
#' derivatives, and best-split search. Used by \code{gadgetTree} when \code{strategy = "ale"}.
#'
#' @field tree_ref Reference to the associated \code{gadgetTree} instance.
#' @field model Fitted model (persistent after \code{fit}).
#' @field data Data frame or data.table with features and target (persistent after \code{fit}).
#' @field target.feature.name Character(1). Name of the target variable.
#' @field n.intervals Integer. Number of intervals for numeric ALE.
#' @field predict.fun Function. \code{function(model, data)} returning predictions.
#' @field order.method Character. Categorical level ordering: \code{"mds"}, \code{"pca"}, or \code{"random"} (default \code{"mds"}).
#'
#' @details
#' Intended for use through \code{gadgetTree$new(strategy = aleStrategy$new())} and
#' \code{tree$fit(...)}. Can be instantiated directly for custom pipelines.
#'
#' @examples
#' \dontrun{
#' ale_strat = aleStrategy$new()
#' tree = gadgetTree$new(strategy = ale_strat, n.split = 2)
#' tree$fit(model = model, data = data, target.feature.name = "y")
#' }
#'
#' @export
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
    order.method = "mds",

    #' @description
    #' Initialize the strategy with name "ale".
    initialize = function() {
      self$name = "ale"
    },

    #' @description
    #' Preprocess data into split-feature matrix Z and ALE effect list Y by calling \code{prepare_split_data_ale}.
    #' @param model Fitted model object.
    #' @param data Data frame or data.table with features and target.
    #' @param target.feature.name Character(1). Name of the target variable.
    #' @param n.intervals Integer. Number of intervals for numeric ALE.
    #' @param feature.set Character or NULL. Features to compute ALE for; NULL = all.
    #' @param split.feature Character or NULL. Features to consider for splitting; NULL = all.
    #' @param predict.fun Function or NULL. Prediction function; NULL uses mlr3-style default.
    #' @param order.method Character. Categorical level order: \code{"mds"}, \code{"pca"}, or \code{"random"} (default \code{"mds"}).
    #' @return List with \code{Z} (data.table of split features) and \code{Y} (named list of ALE effect data.tables).
    preprocess = function(model, data, target.feature.name, n.intervals, feature.set = NULL, split.feature = NULL, predict.fun = NULL, order.method = "mds") {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_integerish(n.intervals, len = 1, lower = 1, .var.name = "n.intervals")
      checkmate::assert_character(feature.set, null.ok = TRUE, .var.name = "feature.set")
      checkmate::assert_character(split.feature, null.ok = TRUE, .var.name = "split.feature")
      checkmate::assert_function(predict.fun, null.ok = TRUE, .var.name = "predict.fun")
      checkmate::assert_character(order.method, len = 1, .var.name = "order.method")

      prepare_split_data_ale(model = model, data = data, target.feature.name = target.feature.name,
        n.intervals = n.intervals, feature.set = feature.set, split.feature = split.feature,
        predict.fun = predict.fun, order.method = order.method)
    },

    #' @description
    #' Transform ALE effects for a node: subset by \code{idx}, handle single-value and recalc categorical as needed.
    #' @param Y List. ALE effect list (each element a data.frame/data.table from \code{calculate_ale}).
    #' @param idx Integer. Row indices of samples in the node.
    #' @param split.feature Character(1) or NULL. Feature used for the split leading to this node.
    #' @return List of transformed ALE effect data.tables.
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
    #' Compute heterogeneity (variance of ALE derivatives) per feature from effect list \code{Y}.
    #' @param Y List. ALE effect list (each element a data.table from \code{calculate_ale}).
    #' @return Numeric vector of heterogeneity, one per feature.
    #' @seealso \code{\link{calculate_ale_heterogeneity_cpp}}
    heterogeneity = function(Y) {
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")

      unlist(calculate_ale_heterogeneity_cpp(Y))
    },

    #' @description
    #' Find the best split for a node by ALE heterogeneity reduction.
    #' @param Z Data frame or data.table. Split-feature matrix (columns = \code{split.feature}).
    #' @param Y List. ALE effect list from \code{calculate_ale}.
    #' @param min.node.size Integer(1). Minimum node size.
    #' @param n.quantiles Integer(1) or NULL. Quantile-based candidate cutpoints for numeric features.
    #' @return List with best split feature, cutpoint/level, and related fields.
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
    #' Placeholder for ALE tree visualization; currently throws. Use package plotting APIs when implemented.
    #' @param tree List. Tree structure (Node objects).
    #' @param model Fitted model.
    #' @param data Data frame or data.table.
    #' @param target.feature.name Character(1). Target variable name.
    #' @param depth Integer or NULL. Depth(s) to plot.
    #' @param node.id Integer or NULL. Node id(s) to plot.
    #' @param features Character or NULL. Features to plot.
    #' @param ... Passed to downstream plotting.
    #' @return Not used; throws an error.
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
    #' Fit an ALE tree: run \code{preprocess} to get Z and Y, then grow the tree by recursive splitting.
    #' @param tree \code{gadgetTree} instance.
    #' @param model Fitted model.
    #' @param data Data frame or data.table (features and target).
    #' @param target.feature.name Character(1). Target variable name.
    #' @param n.intervals Integer. Number of intervals for numeric ALE (default 10).
    #' @param feature.set Character or NULL. Features to compute ALE for; NULL = all.
    #' @param split.feature Character or NULL. Features to split on; NULL = all.
    #' @param predict.fun Function or NULL. Prediction function; NULL = default.
    #' @param order.method Character. Categorical level order: \code{"mds"}, \code{"pca"}, \code{"random"} (default \code{"mds"}).
    #' @param ... Ignored.
    #' @return The \code{tree} object, invisibly.
    fit = function(tree, model, data, target.feature.name, n.intervals = 10, feature.set = NULL, split.feature = NULL, predict.fun = NULL, order.method = "mds", ...) {
      if (missing(model)) stop("aleStrategy requires 'model' to be passed.", call. = FALSE)
      # if (missing(n.intervals)) stop("aleStrategy requires 'n.intervals' to be passed.", call. = FALSE)
      checkmate::assert_integerish(n.intervals, len = 1, lower = 1, .var.name = "n.intervals")
      checkmate::assert_function(predict.fun, null.ok = TRUE, .var.name = "predict.fun")

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
      self$order.method = order.method
      # Keep reference to the tree
      self$tree_ref = tree
      # Preprocess to obtain Z and Y
      prepared.data = self$preprocess(model = model, data = data,
        target.feature.name = target.feature.name, n.intervals = n.intervals,
        feature.set = feature.set, split.feature = split.feature, predict.fun = predict.fun, order.method = order.method)
      Z = prepared.data$Z
      Y = prepared.data$Y
      # Force grid to be empty structure for aleStrategy to save memory
      # regardless of what preprocess returns
      grid = vector("list", length(names(Z)))
      names(grid) = names(Z)
      # Root objective
      objective.value.root.j = self$heterogeneity(Y)
      objective.value.root = sum(objective.value.root.j, na.rm = TRUE)
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
