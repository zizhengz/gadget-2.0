#' Mean-center and Extract Effect Matrices for Features
#'
#' Processes feature effect results to produce a list of effect matrices (Y) and their corresponding grid values for each feature. Optionally mean-centers the effects for each observation.
#'
#' @param effect (`R6` object or list)\cr
#'   An object containing feature effect results, typically with a `results` field. Usually produced by `FeatureEffect` or similar functions.
#' @param feature.set (`character` or `NULL`)\cr
#'   Optional. Subset of features to include. If `NULL`, all features in the effect object are used. Default is `NULL`.
#' @param mean.center (`logical(1)`)\cr
#'   Whether to mean-center the effect matrix for each observation. Default is `TRUE`.
#'
#' @return (`list`)\cr
#'   A list with the following elements (always lists, even for a single feature):
#'   \describe{
#'     \item{Y}{A named list of effect matrices (one per feature).}
#'     \item{grid}{A named list of grid values (column names) for each feature.}
#'   }
#'
#' @details
#' If the effect results are in `data.frame` format, the function pivots the data to wide format and mean-centers if requested. If the effect results are a list, each element is processed similarly. The grid contains the unique values (or bins) for each feature. This function is typically used internally for preparing data for effect tree construction and visualization.
#'
#' @examples
#' # Example usage (assuming 'effect' is an object with a 'results' field from FeatureEffect):
#' # result <- mean_center_ice(effect, feature.set = c("feature1", "feature2"), mean.center = TRUE)
#' # Y <- result$Y
#' # grid <- result$grid
#'
mean_center_ice = function(effect, feature.set = NULL,  mean.center = TRUE) {
  effect.results = effect$results
  if (inherits(effect.results, "data.frame")) {
    Y = effect.results
    feat = colnames(Y)[1]
    if (is.factor(Y$feat)) Y$feat = factor_to_numeric(Y$feat)
    Y = tidyr::pivot_wider(Y, names_from = feat, values_from = .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
    if (mean.center) Y = Y - rowMeans(Y, na.rm = TRUE)
    grid = colnames(Y)
    grid = list(grid)
    names(grid) = feat
    Y = list(Y)
    names(Y) = feat
  } else if (inherits(effect.results, "list")) {
    if (!is.null(feature.set)) {
      available_features = names(effect.results)
      missing_features = setdiff(feature.set, available_features)
      if (length(missing_features) > 0) {
        stop(sprintf("Features not found in effect results: %s. Available features: %s", 
                    paste(missing_features, collapse = ", "),
                    paste(available_features, collapse = ", ")))
      }
      features = names(effect.results) %in% feature.set
      effect.results = effect.results[features]
    }
    Y = lapply(effect.results, function(feat) {
      Y.i = feat
      if (is.factor(Y.i$.borders)) Y.i$.borders = factor_to_numeric(Y.i$.borders)
      Y.i = tidyr::pivot_wider(Y.i, names_from = .borders, values_from = .value)
      Y.i = Y.i[, setdiff(colnames(Y.i), c(".type", ".id", ".feature"))]
      if (mean.center) Y.i = Y.i - rowMeans(Y.i, na.rm = TRUE)
      Y.i
    })
    grid = lapply(Y, function(Y.i) {
      colnames(Y.i)
    })
  }
  return(list(Y = Y, grid = grid))
}
