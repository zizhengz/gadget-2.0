#' Extract and optionally mean-center effect matrices
#'
#' Given effect (with \code{results}), feature_set (or all), and mean_center:
#' pivots effect results to wide matrices per feature; optionally subtracts
#' row means.
#' Returns list \code{Y} (named list of matrices), \code{grid} (column names per feature).
#'
#' @param effect (`R6` object or list)\cr
#'   An object containing feature effect results, typically with
#'   a `results` field. Usually produced by `FeatureEffect` or
#'   similar functions.
#' @param feature_set (`character` or `NULL`)\cr
#'   Optional. Subset of features to include. If `NULL`, all
#'   features in the effect object are used. Default is `NULL`.
#' @param mean_center (`logical(1)`)\cr
#'   Whether to mean-center the effect matrix for each observation.
#'   Default is `TRUE`.
#'
#' @return (`list`)\cr
#'   A list with the following elements (always lists, even for
#'   a single feature):
#'   \describe{
#'     \item{Y}{A named list of effect matrices (one per feature).}
#'     \item{grid}{A named list of grid values (column names) for each feature.}
#'   }
#'
#' @details
#' If the effect results are in `data.frame` format, the function
#' pivots the data to wide format and mean-centers if requested.
#' If the effect results are a list, each element is processed similarly.
#' The grid contains the unique values (or bins) for each feature.
#' This function is typically used internally for preparing data for
#' effect tree construction and visualization.
#'
#' @examples
#' # Example usage (assuming 'effect' is an object with a 'results' field
#' # from FeatureEffect):
#' # result <- mean_center_ice(effect, feature_set = c("feature1", "feature2"), mean_center = TRUE)
#' # Y <- result$Y
#' # grid <- result$grid
#'
mean_center_ice = function(effect, feature_set = NULL, mean_center = TRUE) {
  effect_results = effect$results

  # data_table_pivot = function(data, names_from, values_from) {
  #   dt = data.table::as.data.table(data)
  #   formula = as.formula(paste("... ~", names_from))
  #   result = data.table::dcast(dt, formula, value.var = values_from, fill = NA_real_)
  #   return(as.data.frame(result))
  # }

  if (inherits(effect_results, "data.frame")) {
    Y = effect_results
    feat = colnames(Y)[1]
    if (is.factor(Y$feat)) Y$feat = factor_to_numeric(Y$feat)
    Y = tidyr::pivot_wider(Y, names_from = feat, values_from = .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
    if (mean_center) Y = Y - rowMeans(Y, na.rm = TRUE)
    grid = colnames(Y)
    grid = list(grid)
    names(grid) = feat
    Y = list(Y)
    names(Y) = feat
  } else if (inherits(effect_results, "list")) {
    if (!is.null(feature_set)) {
      available_features = names(effect_results)
      missing_features = setdiff(feature_set, available_features)
      if (length(missing_features) > 0) {
        stop(sprintf("Features not found in effect results: %s. Available features: %s",
            paste(missing_features, collapse = ", "),
            paste(available_features, collapse = ", ")))
      }
      features = names(effect_results) %in% feature_set
      effect_results = effect_results[features]
    }
    Y = lapply(effect_results, function(feat) {
      Y_i = feat
      if (is.factor(Y_i$.borders)) Y_i$.borders = factor_to_numeric(Y_i$.borders)
      Y_i = tidyr::pivot_wider(Y_i, names_from = .borders, values_from = .value)
      Y_i = Y_i[, setdiff(colnames(Y_i), c(".type", ".id", ".feature"))]
      if (mean_center) Y_i = Y_i - rowMeans(Y_i, na.rm = TRUE)
      Y_i
    })
    grid = lapply(Y, function(Y_i) {
      colnames(Y_i)
    })
  }
  list(Y = Y, grid = grid)
}
