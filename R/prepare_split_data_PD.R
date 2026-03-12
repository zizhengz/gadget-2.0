#' Prepare PD Data for Tree Splitting
#'
#' Given effect, data, and optional feature/split sets: validates features;
#' converts character to factor; builds Z (data.table of split columns);
#' calls \code{mean_center_ice} for Y and grid. Returns list \code{Z}, \code{Y}, \code{grid}.
#'
#' @param effect Effect object or list (e.g. from FeatureEffect).
#' @param data Data frame or data.table. Training data (features and target).
#' @param target.feature.name Character or NULL. Name of the target variable (for \code{all.features} resolution).
#' @param feature.set Character or NULL. Features in effect; NULL = all non-target columns.
#' @param split.feature Character or NULL. Features to consider for splitting; NULL = all.
#' @return List with \code{Z} (split-feature matrix), \code{Y} (mean-centered effects), \code{grid}.
prepare_split_data_pd = function(effect, data, target.feature.name = NULL, feature.set = NULL,
  split.feature = NULL) {
  all.features = if (is.null(target.feature.name)) colnames(data) else setdiff(colnames(data), target.feature.name)
  take_cols = function(d, cols) {
    if (data.table::is.data.table(d)) d[, cols, with = FALSE] else d[, cols, drop = FALSE]
  }
  resolve_features = function(requested, err_label) {
    if (is.null(requested)) {
      return(all.features)
    }
    miss = setdiff(requested, all.features)
    if (length(miss) > 0L) {
      stop(sprintf("%s not found in data: %s. Available: %s",
        err_label, paste(miss, collapse = ", "), paste(all.features, collapse = ", ")))
    }
    requested
  }
  ensure_factors = function(d, cols) {
    for (c in cols) if (is.character(d[[c]])) d[[c]] = factor(d[[c]])
    d
  }

  feature.set = resolve_features(feature.set, "Features")
  split.feature = resolve_features(split.feature, "Split features")
  data = ensure_factors(data, union(feature.set, split.feature))
  Z = data.table::setDT(take_cols(data, split.feature))
  wide.mean.center = mean_center_ice(effect = effect, feature.set = feature.set)
  list(Z = Z, Y = wide.mean.center$Y, grid = wide.mean.center$grid)
}
