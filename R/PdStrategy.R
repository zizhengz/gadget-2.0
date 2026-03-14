#' PdStrategy: Generalized Additive Decomposition Based on PD Effects
#'
#' @description
#' PD-based effect strategy (inherits from \link{EffectStrategy}). Given effect and data,
#' preprocesses to Z/Y/grid;
#' mean-centers effects per node; computes sum-of-variances heterogeneity;
#' finds best split via C++; fits tree and plots PD/ICE.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [EffectStrategy].
#'
#' @section Construction:
#' ```
#' s = PdStrategy$new()
#' ```
#'
#' @details
#' This class is used internally by the GadgetTree framework to implement partial dependence
#' tree growing, splitting, and visualization. It is not intended to be used directly by end users,
#' but can be instantiated for advanced customization.
#'
#' @examples
#' \dontrun{
#' # Example: Fit and plot a PD tree using PdStrategy and GadgetTree
#' # (Assuming effect and data are prepared)
#' pd_strat = PdStrategy$new()
#' tree = GadgetTree$new(strategy = pd_strat, n_split = 2)
#' tree$fit(effect, data, target_feature_name = "target")
#' tree$plot(effect, data, target_feature_name = "target")
#' }
#'
#' @include EffectStrategy.R
#' @export
PdStrategy = R6::R6Class(
  "PdStrategy",
  inherit = EffectStrategy,
  public = list(
    #' @description
    #' Create a PdStrategy instance (calls \code{super$initialize("pd")}).
    initialize = function() {
      super$initialize("pd")
    },

    #' @description
    #' Preprocess to Z, Y, grid via \code{prepare_split_data_pd}.
    #' @param effect (R6 or `list()`) \cr
    #'   Effect object (e.g. FeatureEffect).
    #' @param data (`data.frame()` or `data.table()`) \cr
    #'   Data.
    #' @param target_feature_name (`character(1)` or `NULL`) \cr
    #'   Target variable name.
    #' @param feature_set (`character()` or `NULL`) \cr
    #'   Features for effect; \code{NULL} = all.
    #' @param split_feature (`character()` or `NULL`) \cr
    #'   Features for splitting; \code{NULL} = all.
    #' @return (`list()`) \cr
    #'   \code{Z}, \code{Y}, \code{grid}.
    preprocess = function(effect, data, target_feature_name = NULL, feature_set = NULL,
      split_feature = NULL) {
      checkmate::assert_true(inherits(effect, "R6") || is.list(effect), .var.name = "effect")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target_feature_name, len = 1, null.ok = TRUE, .var.name = "target_feature_name")
      checkmate::assert_character(feature_set, null.ok = TRUE, .var.name = "feature_set")
      checkmate::assert_character(split_feature, null.ok = TRUE, .var.name = "split_feature")
      if (!is.null(target_feature_name)) {
        checkmate::assert_subset(target_feature_name, colnames(data), .var.name = "target_feature_name")
      }
      prepare_split_data_pd(effect = effect, data = data, target_feature_name = target_feature_name,
        feature_set = feature_set, split_feature = split_feature)
    },

    #' @description
    #' Subset and mean-center via \code{re_mean_center_ice_cpp}.
    #' @param Y (`list()`) \cr
    #'   Effect matrices per feature.
    #' @param idx (`integer()`) \cr
    #'   Sample indices in the node.
    #' @param grid (`list()`) \cr
    #'   Feature grids; required for PD.
    #' @param split_feature (`character(1)` or `NULL`) \cr
    #'   Ignored for PD; required by interface.
    #' @return (`list()`) \cr
    #'   Mean-centered effect matrices.
    node_transform = function(Y, idx, grid, split_feature = NULL) {
      checkmate::assert_list(Y, .var.name = "Y")
      checkmate::assert_list(grid, .var.name = "grid")
      checkmate::assert_integerish(idx, min.len = 1, .var.name = "idx")
      # Ensure Y elements are numeric matrices (avoid tibble -> C++ type mismatch)
      Y = lapply(Y, function(m) {
        m = as.matrix(m)
        storage.mode(m) = "double"
        m
      })
      re_mean_center_ice_cpp(Y = Y, grid = grid, idx = idx)
    },

    #' @description
    #' Compute heterogeneity via \code{node_heterogeneity}.
    #' @param Y (`list()`) \cr
    #'   Effect matrices.
    #' @return (`numeric()`) \cr
    #'   Heterogeneity per feature.
    heterogeneity = function(Y) {
      checkmate::assert_list(Y, .var.name = "Y")
      node_heterogeneity(Y)
    },

    #' @description
    #' Compute left/right child objective values via node_transform and heterogeneity.
    #' @param Z,Y,split_info,idx_left,idx_right,grid_left,grid_right \cr
    #'   Node/split context.
    #' @return (`list()`) \cr
    #'   \code{left_objective_value_j}, \code{right_objective_value_j},
    #'   \code{left_objective_value}, \code{right_objective_value}.
    get_child_objectives = function(Z, Y, split_info, idx_left, idx_right, grid_left, grid_right) {
      y_left = self$node_transform(Y = Y, idx = idx_left, grid = grid_left)
      y_right = self$node_transform(Y = Y, idx = idx_right, grid = grid_right)
      left_objective_value_j = self$heterogeneity(y_left)
      right_objective_value_j = self$heterogeneity(y_right)
      list(
        left_objective_value_j = left_objective_value_j,
        right_objective_value_j = right_objective_value_j,
        left_objective_value = sum(left_objective_value_j, na.rm = TRUE),
        right_objective_value = sum(right_objective_value_j, na.rm = TRUE)
      )
    },

    #' @description
    #' Find best split via \code{search_best_split_cpp}.
    #' @param Z (`data.frame()` or `data.table()`) \cr
    #'   Split features.
    #' @param Y (`list()`) \cr
    #'   Effect matrices.
    #' @param min_node_size (`integer(1)`) \cr
    #'   Minimum node size.
    #' @param n_quantiles (`integer(1)` or `NULL`) \cr
    #'   Quantile candidates.
    #' @return (`data.frame()` or `list()`) \cr
    #'   Best split info.
    find_best_split = function(Z, Y, min_node_size, n_quantiles) {
      checkmate::assert_data_frame(Z)
      checkmate::assert_list(Y)
      checkmate::assert_integerish(min_node_size, len = 1, lower = 1, any.missing = FALSE, .var.name = "min_node_size")
      checkmate::assert_integerish(n_quantiles, len = 1, lower = 1, null.ok = TRUE, .var.name = "n_quantiles")
      search_best_split_cpp(Z = Z, Y = Y, min_node_size = min_node_size, n_quantiles = n_quantiles)
    },

    #' @description
    #' Plot PD/ICE tree via \code{plot_tree_pd}.
    #' @param tree (`list()`) \cr
    #'   Depth-based list of Node objects.
    #' @param effect (R6 or `list()`) \cr
    #'   Effect object.
    #' @param data (`data.frame()`) \cr
    #'   Data.
    #' @param target_feature_name (`character(1)`) \cr
    #'   Target name.
    #' @param depth (`integer()` or `NULL`) \cr
    #'   Depths to plot.
    #' @param node_id (`integer()` or `NULL`) \cr
    #'   Node IDs to plot.
    #' @param features (`character()` or `NULL`) \cr
    #'   Features to plot.
    #' @param ... Plot arguments.
    #' @return (`list()`) \cr
    #'   Nested list (depth -> node -> patchwork).
    plot = function(tree, effect, data, target_feature_name, depth = NULL, node_id = NULL, features = NULL, ...) {
      checkmate::assert_list(tree, .var.name = "tree")
      checkmate::assert_true(is.list(effect) || inherits(effect, "R6"), .var.name = "effect")
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_integerish(depth, lower = 1, null.ok = TRUE, .var.name = "depth")
      checkmate::assert_integerish(node_id, lower = 1, null.ok = TRUE, .var.name = "node_id")
      checkmate::assert_character(features, null.ok = TRUE, .var.name = "features")
      plot_tree_pd(tree = tree, effect = effect, data = data,
        target_feature_name = target_feature_name,
        depth = depth, node_id = node_id, features = features, ...)
    },

    #' @description
    #' Fit tree: preprocess, create root, split recursively.
    #' @param tree (`GadgetTree`) \cr
    #'   Tree instance.
    #' @param effect (R6 or `list()`) \cr
    #'   Effect object.
    #' @param data (`data.frame()`) \cr
    #'   Data.
    #' @param target_feature_name (`character(1)`) \cr
    #'   Target name.
    #' @param feature_set,split_feature (`character()` or `NULL`) \cr
    #'   Feature subsets.
    #' @param ... Ignored.
    #' @return (`GadgetTree`) \cr
    #'   The tree, invisibly.
    fit = function(tree, effect, data, target_feature_name,
      feature_set = NULL, split_feature = NULL, ...) {
      if (missing(effect)) cli::cli_abort("PdStrategy requires 'effect' to be passed.")
      checkmate::assert_r6(tree, classes = "GadgetTree", .var.name = "tree")
      checkmate::assert_data_frame(data, .var.name = "data")
      checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
      checkmate::assert_character(feature_set, null.ok = TRUE, .var.name = "feature_set")
      checkmate::assert_character(split_feature, null.ok = TRUE, .var.name = "split_feature")
      checkmate::assert_true(is.list(effect) || inherits(effect, "R6"), .var.name = "effect")

      self$tree_ref = tree
      prepared_data = self$preprocess(effect = effect, data = data,
        target_feature_name = target_feature_name,
        feature_set = feature_set,
        split_feature = split_feature)
      Z = prepared_data$Z
      Y = prepared_data$Y
      grid = prepared_data$grid
      objective_value_root_j = self$heterogeneity(Y)
      objective_value_root = sum(objective_value_root_j, na.rm = TRUE)
      t_regional = private$fit_tree_internal(tree, Z, Y, grid, objective_value_root_j, objective_value_root)
      self$fit_timing = list(regional = t_regional)
      invisible(tree)
    },

    #' @description
    #' Optional cleanup hook (no-op for PD). Called after \code{$fit()} by GadgetTree.
    clean = function() {
      invisible(NULL)
    }
  )
)
