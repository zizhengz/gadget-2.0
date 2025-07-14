#' effectStrategy (abstract base class for effect strategies)
#'
#' Defines the interface for effect strategies (PD, ALE, SD, etc.).
effectStrategy <- R6::R6Class(
  "effectStrategy",
  public = list(
    name = NULL,
    initialize = function() {
      self$name <- "abstract"
    },
    preprocess = function(effect, data, target.feature, feature.set = NULL, split.feature = NULL) {
      stop("Not implemented.")
    },
    node_transform = function(Y, grid, idx) {
      stop("Not implemented.")
    },
    heterogeneity = function(Y) {
      stop("Not implemented.")
    },
    find_best_split = function(Z, Y, min.node.size, n.quantiles) {
      stop("Not implemented.")
    }
  )
)
