#' Extract and optionally mean-center effect matrices
#'
#' Given effect (with \code{results}), feature_set (or all), and mean_center:
#' pivots effect results to wide matrices per feature; optionally subtracts
#' row means.
#' Returns list \code{Y} (named list of matrices), \code{grid} (column names per feature).
#'
#' @param effect (R6 or `list()`) \cr
#'   Effect object with \code{results} field (e.g. from FeatureEffect).
#' @param feature_set (`character()` or `NULL`) \cr
#'   Features to include; \code{NULL} = all.
#' @param mean_center (`logical(1)`) \cr
#'   Whether to mean-center each effect matrix.
#'
#' @return (`list()`) \cr
#'   \code{Y}: named list of effect matrices; \code{grid}: grid values per feature.
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
#' # result = mean_center_ice(effect, feature_set = c("feature1", "feature2"), mean_center = TRUE)
#' # Y = result$Y
#' # grid = result$grid
#'
pivot_effect_to_wide = function(data, grid_col, value_col = ".value", drop_cols = c(".type", ".id"),
  mean_center = FALSE) {
  if (is.factor(data[[grid_col]])) data[[grid_col]] = factor_to_numeric(data[[grid_col]])
  id_cols = setdiff(colnames(data), c(grid_col, value_col))
  formula = stats::as.formula(paste(paste(id_cols, collapse = " + "), "~", grid_col))
  wide = data.table::dcast(data.table::as.data.table(data), formula,
    value.var = value_col, fill = NA_real_)
  value_cols = setdiff(colnames(wide), drop_cols)
  out = as.data.frame(wide)[, value_cols, drop = FALSE]
  if (mean_center) out = out - rowMeans(out, na.rm = TRUE)
  out
}

mean_center_ice = function(effect, feature_set = NULL, mean_center = TRUE) {
  checkmate::assert_true(inherits(effect, "R6") || is.list(effect), .var.name = "effect")
  checkmate::assert_character(feature_set, null.ok = TRUE, .var.name = "feature_set")
  checkmate::assert_flag(mean_center, .var.name = "mean_center")
  effect_results = effect$results

  items = if (inherits(effect_results, "data.frame")) {
    feat = colnames(effect_results)[1]
    setNames(list(list(data = effect_results, grid_col = feat, drop = c(".type", ".id"))), feat)
  } else {
    all_features = names(effect_results)
    feature_set = resolve_split_features(feature_set, all_features, "Features")
    mlr3misc::map(setNames(nm = feature_set), function(f) {
      list(data = effect_results[[f]], grid_col = ".borders",
        drop = c(".type", ".id", ".feature"))
    })
  }

  res = mlr3misc::map(items, function(it) {
    Y = pivot_effect_to_wide(it$data, it$grid_col, drop_cols = it$drop, mean_center = mean_center)
    list(Y = Y, grid = colnames(Y))
  })
  list(
    Y = mlr3misc::map(res, "Y"),
    grid = mlr3misc::map(res, "grid")
  )
}
