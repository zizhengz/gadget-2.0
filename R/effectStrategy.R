#' effectStrategy: Abstract Base Class for Effect Strategies (R6 class)
#'
#' Provides the interface for effect-based tree strategies (e.g., pdStrategy, aleStrategy).
#' This abstract class defines the required methods for any effect strategy implementation,
#' including data preprocessing, node transformation, heterogeneity calculation, split search,
#' tree fitting, and visualization.
#'
#' @field name Character. Name of the strategy.
#'
#' @details
#' This class is intended to be subclassed by concrete effect strategies (e.g., pdStrategy, aleStrategy).
#' All methods must be implemented in subclasses; calling them directly will result in an error.
#'
#' @examples
#' # Example: Defining a custom effect strategy (skeleton)
#' myStrategy = R6::R6Class(
#'   "myStrategy",
#'   inherit = effectStrategy,
#'   public = list(
#'     preprocess = function(effect, data, target.feature, feature.set = NULL, split.feature = NULL) {
#'       ...
#'     },
#'     node_transform = function(Y, grid, idx) {
#'       ...
#'     },
#'     heterogeneity = function(Y) {
#'       ...
#'     },
#'     find_best_split = function(Z, Y, min.node.size, n.quantiles) {
#'       ...
#'     },
#'     fit = function(tree_instance, effect, data, target.feature.name, ...) {
#'       ...
#'     },
#'     plot = function(tree, effect, data, target.feature.name, ...) {
#'       ...
#'     }
#'   )
#' )
#'
#' @keywords internal

effectStrategy = R6::R6Class(
  "effectStrategy",
  public = list(
    name = NULL,

    #' @description
    #' Initialize the strategy with name "abstract".
    initialize = function() {
      self$name = "abstract"
    },

    #' @description
    #' Preprocess input data and effect object. Must be implemented by subclasses.
    #' @param effect R6 object or list. Effect object, typically from FeatureEffect or FeatureEffects.
    #' @param data Data frame. Data frame containing all features and the target variable.
    #' @param target.feature Character(1). The name of the target feature in the data to explain.
    #' @param feature.set Character or NULL. Optional. Subset of features to use for effect calculation. If NULL, all features are used.
    #' @param split.feature Character or NULL. Optional. Features to consider for splitting at each node. If NULL, all features are considered.
    #' @return List. Preprocessed data for tree construction.
    preprocess = function(effect, data, target.feature, feature.set = NULL, split.feature = NULL) {
      stop("Not implemented.")
    },

    #' @description
    #' Node-level transformation of the effect list. Must be implemented by subclasses.
    #' @param Y List. Each element is an effect matrix for a feature.
    #' @param grid List. Feature grids.
    #' @param idx Integer. Sample indices for the current node.
    #' @return List. List of transformed effects.
    node_transform = function(Y, grid, idx) {
      stop("Not implemented.")
    },

    #' @description
    #' Calculate the heterogeneity (e.g., sum of variances) of effects within the node.
    #' Must be implemented by subclasses.
    #'
    #' @param Y List. List of effect matrices.
    #' @return Numeric. Numeric vector, heterogeneity for each feature.
    heterogeneity = function(Y) {
      stop("Not implemented.")
    },

    #' @description
    #' Find the best split point for a node. Must be implemented by subclasses.
    #' @param Z Data frame. Split feature set.
    #' @param Y List. Effect matrices.
    #' @param min.node.size Integer(1). Minimum node size.
    #' @param n.quantiles Integer(1) or NULL. \cr
    #'   Optional. Number of quantiles to use for candidate split points (for numeric features).
    #'   If NULL, use default.
    #' @return List. List with best split feature, split point, etc.
    find_best_split = function(Z, Y, min.node.size, n.quantiles) {
      stop("Not implemented.")
    },

    #' @description
    #' Fit the effect-based tree. Must be implemented by subclasses.
    #' @param tree_instance gadgetTree object. Tree object instance.
    #' @param effect R6 object or list. Effect object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param ... Additional arguments.
    #' @return gadgetTree object, invisibly. The fitted tree object.
    fit = function(tree_instance, effect, data, target.feature.name, ...) {
      stop("Not implemented.")
    },

    #' @description
    #' Visualize the effect-based tree. Must be implemented by subclasses.
    #' @param tree List. Tree structure as a list of Node objects.
    #' @param effect R6 object or list. Effect object.
    #' @param data Data frame. Data frame.
    #' @param target.feature.name Character(1). Target feature name.
    #' @param ... Additional plotting arguments.
    #' @return List. List of plot objects.
    plot = function(tree, effect, data, target.feature.name, ...) {
      stop("Not implemented.")
    }
  )
)
