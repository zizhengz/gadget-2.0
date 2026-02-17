#' Prepare PD Data for Tree Splitting
#'
#' Validates features, converts character columns to ordered factors (for categorical
#' split features), builds the split-feature matrix Z, and mean-centers effects.
#' Categorical level ordering uses \code{order_categorical_levels} so PD search
#' (e.g. C++) receives Z with a data-driven level order without changing search code.
#'
#' @param effect Effect object or list (e.g. from FeatureEffect).
#' @param data Data frame or data.table. Training data (features and target).
#' @param target.feature.name Character or NULL. Name of the target variable; required for level ordering.
#' @param feature.set Character or NULL. Features in effect; NULL = all non-target columns.
#' @param split.feature Character or NULL. Features to consider for splitting; NULL = all.
#' @param order.method Character. Categorical level order: \code{"mds"}, \code{"pca"},
#'   \code{"random"}, or \code{"raw"} (keep existing factor level order; default \code{"mds"}).
#' @return List with \code{Z} (split-feature matrix), \code{Y} (mean-centered effects), \code{grid}.
prepare_split_data_pd = function(effect, data, target.feature.name = NULL, feature.set = NULL,
  split.feature = NULL, order.method = "mds") {
  all.features = if (is.null(target.feature.name)) colnames(data) else setdiff(colnames(data), target.feature.name)
  take_cols = function(d, cols) {
    if (data.table::is.data.table(d)) d[, cols, with = FALSE] else d[, cols, drop = FALSE]
  }
  resolve_features = function(requested, err_label) {
    if (is.null(requested)) return(all.features)
    miss = setdiff(requested, all.features)
    if (length(miss) > 0L)
      stop(sprintf("%s not found in data: %s. Available: %s",
        err_label, paste(miss, collapse = ", "), paste(all.features, collapse = ", ")))
    requested
  }
  ensure_factors = function(d, cols) {
    for (c in cols) if (is.character(d[[c]])) d[[c]] = factor(d[[c]])
    d
  }

  feature.set = resolve_features(feature.set, "Features")
  split.feature = resolve_features(split.feature, "Split features")
  data = ensure_factors(data, union(feature.set, split.feature))
  if (!is.null(target.feature.name)) {
    for (col in union(feature.set, split.feature))
      if (is.factor(data[[col]]))
        data[[col]] = order_categorical_levels(droplevels(data[[col]]), data, col, target.feature.name, order.method)
  }
  Z = data.table::setDT(take_cols(data, split.feature))
  wide.mean.center = mean_center_ice(effect = effect, feature.set = feature.set)
  list(Z = Z, Y = wide.mean.center$Y, grid = wide.mean.center$grid)
}
