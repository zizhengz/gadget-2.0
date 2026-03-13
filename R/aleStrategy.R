#' Default prediction function for mlr3-style models.
#' Given model and data: calls model$predict_newdata(data); if result is
#' Prediction returns response else returns as-is.
#' @param model Fitted model.
#' @param data New data.
#' @keywords internal
default_predict_fun = function(model, data) {
  pred = model$predict_newdata(data)
  if (inherits(pred, "Prediction")) pred$response else pred
}

#' @title AleStrategy: Generalized additive decomposition based on ALE effects.
#'
#' @description
#' ALE-based strategy: given model and data, preprocesses to Z/Y via \code{prepare_split_data_ale};
#' transforms ALE effects per node; computes ALE-derivative heterogeneity;
#' finds best split via \code{search_best_split_ale}; fits tree and plots ALE curves.
#'
#' @field name Character. Strategy name (e.g., \code{"ale"}).
#' @field tree_ref Reference to the associated \code{GadgetTree} instance.
#' @field fit_timing Named numeric vector with global/regional fit times (seconds).
#' @field model Fitted model (persistent after \code{fit}).
#' @field data Data frame or data.table with features and target (persistent after \code{fit}).
#' @field target_feature_name Character(1). Name of the target variable.
#' @field n_intervals Integer. Number of intervals for numeric ALE.
#' @field predict_fun Function. \code{function(model, data)} returning predictions.
#' @field order_method Character. Categorical level ordering: \code{"mds"}, \code{"pca"},
#'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
#' @field with_stab Logical. Whether boundary-stabilized splits are enabled.
#' @field effect_root Cached ALE effect list at the root node (used by \code{$plot()}).
#'
#' @details
#' Intended for use through \code{GadgetTree$new(strategy = AleStrategy$new())} and
#' \code{tree$fit(...)}. Can be instantiated directly for custom pipelines.
#'
#' @examples
#' \dontrun{
#' ale_strat = AleStrategy$new()
#' tree = GadgetTree$new(strategy = ale_strat, n_split = 2)
#' tree$fit(model = model, data = data, target_feature_name = "y")
#' }
#'
#' @export
AleStrategy = R6::R6Class(
  "AleStrategy",
  public = list(
    name = NULL,
    tree_ref = NULL,
    fit_timing = NULL,
    # Persistent context fields (declared to allow assignment outside initialize)
    model = NULL,
    data = NULL,
    target_feature_name = NULL,
    n_intervals = NULL,
    predict_fun = NULL,
    order_method = "mds",
    with_stab = FALSE,
    effect_root = NULL,

    #' @description
    #' Sets \code{name = "ale"}. Returns the strategy instance.
    initialize = function() {
      self$name = "ale"
    },

    #' @description
    #' Given model, data, target_feature_name, n_intervals, and optional feature/split sets: computes ALE per feature;
    #' builds Z (split features) and Y (named list of ALE data.tables) via
    #' \code{prepare_split_data_ale}. Returns list \code{Z}, \code{Y}.
    #' @param model Fitted model object.
    #' @param data Data frame or data.table with features and target.
    #' @param target_feature_name Character(1). Name of the target variable.
    #' @param n_intervals Integer. Number of intervals for numeric ALE.
    #' @param feature_set Character or NULL. Features to compute ALE for; NULL = all.
    #' @param split_feature Character or NULL. Features to consider for splitting; NULL = all.
    #' @param predict_fun Function or NULL. Prediction function; NULL uses mlr3-style default.
    #' @param order_method Character. Categorical level order: \code{"mds"}, \code{"pca"},
    #'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
    #' @return List with \code{Z} (data.table of split features) and
    #'   \code{Y} (named list of ALE effect data.tables).
    preprocess = function(model, data, target_feature_name, n_intervals,
      feature_set = NULL, split_feature = NULL, predict_fun = NULL,
      order_method = "mds") {
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_integerish(n_intervals, len = 1, lower = 1, .var.name = "n_intervals")
      checkmate::assert_character(feature_set, null.ok = TRUE, .var.name = "feature_set")
      checkmate::assert_character(split_feature, null.ok = TRUE, .var.name = "split_feature")
      checkmate::assert_function(predict_fun, null.ok = TRUE, .var.name = "predict_fun")
      checkmate::assert_character(order_method, len = 1, .var.name = "order_method")

      prepare_split_data_ale(model = model, data = data, target_feature_name = target_feature_name,
        n_intervals = n_intervals, feature_set = feature_set, split_feature = split_feature,
        predict_fun = predict_fun, order_method = order_method)
    },

    #' @description
    #' Given Y (ALE effect list), idx, and split_feature: subsets ALE rows by idx;
    #' handles single-interval numeric and recalculates categorical if
    #' split_feature changes.
    #' Returns list of ALE data.tables.
    #' @param Y List. ALE effect list (each element a data.frame/data.table from \code{calculate_ale}).
    #' @param idx Integer. Row indices of samples in the node.
    #' @param split_feature Character(1) or NULL. Feature used for the split leading to this node.
    #' @return List of transformed ALE effect data.tables.
    node_transform = function(Y, idx, split_feature = NULL) {
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")
      checkmate::assert_integerish(idx, min.len = 1, .var.name = "idx")
      checkmate::assert_character(split_feature, len = 1, null.ok = TRUE, .var.name = "split_feature")

      node_transform_ale(
        Y = Y,
        idx = idx,
        split_feature = split_feature,
        model = self$model,
        data = self$data,
        target_feature_name = self$target_feature_name,
        n_intervals = self$n_intervals,
        predict_fun = self$predict_fun
      )
    },

    #' @description
    #' Given Y (list of ALE effect data.tables): computes sum of squared dL
    #' per feature via \code{calculate_ale_heterogeneity_cpp}.
    #' Returns numeric vector of length \code{length(Y)}.
    #' @param Y List. ALE effect list (each element a data.table from \code{calculate_ale}).
    #' @return Numeric vector of heterogeneity, one per feature.
    #' @seealso \code{\link{calculate_ale_heterogeneity_cpp}}
    heterogeneity = function(Y) {
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")

      unlist(calculate_ale_heterogeneity_cpp(Y))
    },

    #' @description
    #' Given Z, Y (ALE effect list), min_node_size, n_quantiles: calls
    #' \code{search_best_split_ale} to evaluate all features.
    #' Returns list with \code{split_feature}, \code{split_point},
    #' \code{best_split}, \code{left/right_objective_value_j}, etc.
    #' @param Z Data frame or data.table. Split-feature matrix (columns = \code{split_feature}).
    #' @param Y List. ALE effect list from \code{calculate_ale}.
    #' @param min_node_size Integer(1). Minimum node size.
    #' @param n_quantiles Integer(1) or NULL. Quantile-based candidate cutpoints for numeric features.
    #' @return List with best split feature, cutpoint/level, and related fields.
    find_best_split = function(Z, Y, min_node_size, n_quantiles) {
      checkmate::assert_true(data.table::is.data.table(Z) || is.data.frame(Z), .var.name = "Z")
      checkmate::assert_list(Y, min.len = 1, .var.name = "Y")
      checkmate::assert_true(all(sapply(Y, is.data.frame)), .var.name = "Y")
      checkmate::assert_integerish(min_node_size, len = 1, lower = 1, any.missing = FALSE, .var.name = "min_node_size")
      checkmate::assert_integerish(n_quantiles, len = 1, lower = 1, null.ok = TRUE, .var.name = "n_quantiles")

      search_best_split_ale(
        Z = Z,
        effect = Y,
        min_node_size = min_node_size,
        n_quantiles = n_quantiles,
        with_stab = self$with_stab
      )
    },

    #' @description
    #' Given tree, effect (or cached \code{effect_root}), data,
    #' target_feature_name, and optional depth/node_id/features: calls
    #' \code{plot_tree_ale}.
    #' Returns nested list of patchwork/ggplot objects (depth -> node).
    #' @param tree Depth-based list of Node objects (typically from \code{convert_tree_to_list}).
    #' @param effect Optional ALE effect list (from \code{calculate_ale}); if \code{NULL},
    #'   uses the cached \code{effect_root} from the last \code{$fit()} call.
    #' @param data Data frame or data.table used for plotting (same data passed to \code{$fit()}).
    #' @param target_feature_name Character(1). Name of the target variable.
    #' @param depth Optional integer vector of depths to plot.
    #' @param node_id Optional integer vector of node ids to plot.
    #' @param features Optional character vector of feature names to include.
    #' @param show_plot Logical. If \code{TRUE}, print plots as they are created.
    #' @param show_point Logical. If \code{TRUE}, overlay observation points.
    #' @param mean_center Logical. Whether to mean-center ALE curves (default \code{TRUE}).
    #' @param ... Passed on to \code{plot_tree_ale()}.
    plot = function(tree, effect = NULL, data, target_feature_name,
      depth = NULL, node_id = NULL, features = NULL,
      show_plot = TRUE, show_point = FALSE, mean_center = TRUE, ...) {
      checkmate::assert_list(tree)
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node_id, lower = 1, null.ok = TRUE, .var.name = "node_id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      checkmate::assert_flag(show_plot)
      checkmate::assert_flag(show_point)
      checkmate::assert_flag(mean_center)
      if (is.null(effect)) {
        effect = self$effect_root
        if (is.null(effect)) {
          stop("No cached ALE effect found. Please pass 'effect' or run fit() first.",
            call. = FALSE)
        }
      }
      plot_tree_ale(tree = tree, effect = effect, data = data,
        target_feature_name = target_feature_name, depth = depth,
        node_id = node_id, features = features, show_plot = show_plot,
        show_point = show_point, mean_center = mean_center, ...)
    },

    #' @description
    #' Given tree, model, data, target_feature_name: preprocesses Z/Y;
    #' creates root Node; recursively splits; stores effect_root and
    #' fit_timing.
    #' Returns tree invisibly.
    #' @param tree \code{GadgetTree} instance.
    #' @param model Fitted model.
    #' @param data Data frame or data.table (features and target).
    #' @param target_feature_name Character(1). Target variable name.
    #' @param n_intervals Integer. Number of intervals for numeric ALE (default 10).
    #' @param feature_set Character or NULL. Features to compute ALE for; NULL = all.
    #' @param split_feature Character or NULL. Features to split on; NULL = all.
    #' @param predict_fun Function or NULL. Prediction function; NULL = default.
    #' @param order_method Character. Categorical level order: \code{"mds"}, \code{"pca"},
    #'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
    #' @param with_stab Logical. Whether to enable ALE boundary stabilizer in splitting.
    #' @param ... Ignored.
    #' @return The \code{tree} object, invisibly.
    fit = function(tree, model, data, target_feature_name,
      n_intervals = 10, feature_set = NULL, split_feature = NULL,
      predict_fun = NULL, order_method = "raw", with_stab = FALSE, ...) {
      if (missing(model)) stop("AleStrategy requires 'model' to be passed.", call. = FALSE)
      # if (missing(n_intervals)) stop("AleStrategy requires 'n_intervals' to be passed.", call. = FALSE)
      checkmate::assert_integerish(n_intervals, len = 1, lower = 1, .var.name = "n_intervals")
      checkmate::assert_function(predict_fun, null.ok = TRUE, .var.name = "predict_fun")

      # Default prediction function for mlr3 models compatibility
      if (is.null(predict_fun)) {
        predict_fun = default_predict_fun
      }
      # Store parameters for ALE-specific splitting
      # --- global part (timed) ---
      t_global = system.time({
        self$model = model
        self$data = data
        self$target_feature_name = target_feature_name
        self$n_intervals = n_intervals
        self$predict_fun = predict_fun
        self$order_method = order_method
        self$with_stab = with_stab
        self$tree_ref = tree
        prepared_data = self$preprocess(model = model, data = data,
          target_feature_name = target_feature_name, n_intervals = n_intervals,
          feature_set = feature_set, split_feature = split_feature,
          predict_fun = predict_fun, order_method = order_method)
        Z = prepared_data$Z
        Y = prepared_data$Y
        self$effect_root = Y
        grid = vector("list", length(names(Z)))
        names(grid) = names(Z)
        objective_value_root_j = self$heterogeneity(Y)
        objective_value_root = sum(objective_value_root_j, na.rm = TRUE)
      })[["elapsed"]]

      # --- regional part (timed) ---
      t_regional = system.time({
        parent = Node$new(id = 1, depth = 1, subset_idx = seq_len(nrow(Z)), grid = grid,
          objective_value_parent = NA, objective_value = objective_value_root, intImp_j = NULL,
          objective_value_j = objective_value_root_j, improvement_met = FALSE, intImp = NULL,
          strategy = self)
        parent$split_node(
          Z = Z, Y = Y,
          objective_value_root_j = objective_value_root_j,
          objective_value_root = objective_value_root,
          min_node_size = tree$min_node_size,
          n_quantiles = tree$n_quantiles,
          impr_par = tree$impr_par,
          depth = 1,
          max_depth = tree$n_split + 1
        )
        tree$root = parent
      })[["elapsed"]]

      self$fit_timing = list(global = t_global, regional = t_regional)
      message("AleStrategy fit timing: global ", round(t_global, 3), "s, regional ", round(t_regional, 3), "s")
      invisible(tree)
    },
    #' @description
    #' Sets \code{data} and \code{model} to NULL to free memory. Returns NULL.
    clean = function() {
      self$data = NULL
      self$model = NULL
    }
  )
)
