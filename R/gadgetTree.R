#' gadgetTree: Main tree class for gadget
#'
#' This class implements the main tree growing logic, using a strategy object (e.g., pdStrategy) for effect-specific operations.
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
