#' @keywords internal
default_predict_fun = function(model, data) {
  pred = model$predict_newdata(data)
  if (inherits(pred, "Prediction")) pred$response else pred
}

#' @title aleStrategy: Generalized additive decomposition based on ALE effects.
#'
#' @description
#' Implements the effectStrategy interface for ALE-based trees: preprocessing via
#' \code{prepare_split_data_ale}, node-wise ALE transform, heterogeneity from ALE
#' derivatives, and best-split search. Used by \code{gadgetTree} when \code{strategy = "ale"}.
#'
#' @field tree_ref Reference to the associated \code{gadgetTree} instance.
#' @field fit_timing Named numeric vector with global/regional fit times (seconds).
#' @field model Fitted model (persistent after \code{fit}).
#' @field data Data frame or data.table with features and target (persistent after \code{fit}).
#' @field target.feature.name Character(1). Name of the target variable.
#' @field n.intervals Integer. Number of intervals for numeric ALE.
#' @field predict.fun Function. \code{function(model, data)} returning predictions.
#' @field order.method Character. Categorical level ordering: \code{"mds"}, \code{"pca"},
#'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
#' @field with_stab Logical. Whether boundary-stabilized splits are enabled.
#' @field effect_root Cached ALE effect list at the root node (used by \code{$plot()}).
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
    fit_timing = NULL,
    # Persistent context fields (declared to allow assignment outside initialize)
    model = NULL,
    data = NULL,
    target.feature.name = NULL,
    n.intervals = NULL,
    predict.fun = NULL,
    order.method = "mds",
    with_stab = FALSE,
    effect_root = NULL,

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
    #' @param order.method Character. Categorical level order: \code{"mds"}, \code{"pca"},
    #'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
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
        n.quantiles = n.quantiles,
        with_stab = self$with_stab
      )
    },

    #' @description
    #' Visualize ALE curves for selected nodes by delegating to \code{plot_tree_ale()}.
    #' @param tree Depth-based list of Node objects (typically from \code{convert_tree_to_list}).
    #' @param effect Optional ALE effect list (from \code{calculate_ale}); if \code{NULL},
    #'   uses the cached \code{effect_root} from the last \code{$fit()} call.
    #' @param data Data frame or data.table used for plotting (same data passed to \code{$fit()}).
    #' @param target.feature.name Character(1). Name of the target variable.
    #' @param depth Optional integer vector of depths to plot.
    #' @param node.id Optional integer vector of node ids to plot.
    #' @param features Optional character vector of feature names to include.
    #' @param show.plot Logical. If \code{TRUE}, print plots as they are created.
    #' @param show.point Logical. If \code{TRUE}, overlay observation points.
    #' @param mean.center Logical. Whether to mean-center ALE curves (default \code{TRUE}).
    #' @param ... Passed on to \code{plot_tree_ale()}.
    plot = function(tree, effect = NULL, data, target.feature.name,
      depth = NULL, node.id = NULL, features = NULL,
      show.plot = TRUE, show.point = FALSE, mean.center = TRUE, ...) {
      checkmate::assert_list(tree)
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target.feature.name, len = 1, .var.name = "target.feature.name")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node.id, lower = 1, null.ok = TRUE, .var.name = "node.id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      checkmate::assert_flag(show.plot)
      checkmate::assert_flag(show.point)
      checkmate::assert_flag(mean.center)
      if (is.null(effect)) {
        effect = self$effect_root
        if (is.null(effect)) {
          stop("No cached ALE effect found. Please pass 'effect' or run fit() first.",
            call. = FALSE)
        }
      }
      plot_tree_ale(tree = tree, effect = effect, data = data,
        target.feature.name = target.feature.name, depth = depth,
        node.id = node.id, features = features, show.plot = show.plot,
        show.point = show.point, mean.center = mean.center, ...)
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
    #' @param order.method Character. Categorical level order: \code{"mds"}, \code{"pca"},
    #'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
    #' @param with_stab Logical. Whether to enable ALE boundary stabilizer in splitting.
    #' @param ... Ignored.
    #' @return The \code{tree} object, invisibly.
    fit = function(tree, model, data, target.feature.name,
      n.intervals = 10, feature.set = NULL, split.feature = NULL,
      predict.fun = NULL, order.method = "raw", with_stab = FALSE, ...) {
      if (missing(model)) stop("aleStrategy requires 'model' to be passed.", call. = FALSE)
      # if (missing(n.intervals)) stop("aleStrategy requires 'n.intervals' to be passed.", call. = FALSE)
      checkmate::assert_integerish(n.intervals, len = 1, lower = 1, .var.name = "n.intervals")
      checkmate::assert_function(predict.fun, null.ok = TRUE, .var.name = "predict.fun")

      # Default prediction function for mlr3 models compatibility
      if (is.null(predict.fun)) {
        predict.fun = default_predict_fun
      }
      # Store parameters for ALE-specific splitting
      # --- global part (timed) ---
      t_global = system.time({
        self$model = model
        self$data = data
        self$target.feature.name = target.feature.name
        self$n.intervals = n.intervals
        self$predict.fun = predict.fun
        self$order.method = order.method
        self$with_stab = with_stab
        self$tree_ref = tree
        prepared.data = self$preprocess(model = model, data = data,
          target.feature.name = target.feature.name, n.intervals = n.intervals,
          feature.set = feature.set, split.feature = split.feature, predict.fun = predict.fun, order.method = order.method)
        Z = prepared.data$Z
        Y = prepared.data$Y
        self$effect_root = Y
        grid = vector("list", length(names(Z)))
        names(grid) = names(Z)
        objective.value.root.j = self$heterogeneity(Y)
        objective.value.root = sum(objective.value.root.j, na.rm = TRUE)
      })[["elapsed"]]

      # --- regional part (timed) ---
      t_regional = system.time({
        parent = Node$new(id = 1, depth = 1, subset.idx = seq_len(nrow(Z)), grid = grid,
          objective.value.parent = NA, objective.value = objective.value.root, intImp.j = NULL,
          objective.value.j = objective.value.root.j, improvement.met = FALSE, intImp = NULL,
          strategy = self)
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

      self$fit_timing = list(global = t_global, regional = t_regional)
      message("aleStrategy fit timing: global ", round(t_global, 3), "s, regional ", round(t_regional, 3), "s")
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
