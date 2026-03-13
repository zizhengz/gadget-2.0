#' Prepare PD Data for Tree Splitting
#'
#' Given effect, data, and optional feature/split sets: validates features;
#' converts character to factor; builds Z (data.table of split columns);
#' calls \code{mean_center_ice} for Y and grid. Returns list \code{Z}, \code{Y}, \code{grid}.
#'
#' @param effect Effect object or list (e.g. from FeatureEffect).
#' @param data Data frame or data.table. Training data (features and target).
#' @param target_feature_name Character or NULL. Name of the target variable (for \code{all_features} resolution).
#' @param feature_set Character or NULL. Features in effect; NULL = all non-target columns.
#' @param split_feature Character or NULL. Features to consider for splitting; NULL = all.
#' @return List with \code{Z} (split-feature matrix), \code{Y} (mean-centered effects), \code{grid}.
prepare_split_data_pd = function(effect, data, target_feature_name = NULL, feature_set = NULL,
  split_feature = NULL) {
  all_features = if (is.null(target_feature_name)) colnames(data) else setdiff(colnames(data), target_feature_name)
  take_cols = function(d, cols) {
    if (data.table::is.data.table(d)) d[, cols, with = FALSE] else d[, cols, drop = FALSE]
  }
  resolve_features = function(requested, err_label) {
    if (is.null(requested)) {
      return(all_features)
    }
    miss = setdiff(requested, all_features)
    if (length(miss) > 0L) {
      stop(sprintf("%s not found in data: %s. Available: %s",
          err_label, paste(miss, collapse = ", "), paste(all_features, collapse = ", ")))
    }
    requested
  }
  ensure_factors = function(d, cols) {
    for (c in cols) if (is.character(d[[c]])) d[[c]] = factor(d[[c]])
    d
  }

  feature_set = resolve_features(feature_set, "Features")
  split_feature = resolve_features(split_feature, "Split features")
  data = ensure_factors(data, union(feature_set, split_feature))
  Z = data.table::setDT(take_cols(data, split_feature))
  wide_mean_center = mean_center_ice(effect = effect, feature_set = feature_set)
  list(Z = Z, Y = wide_mean_center$Y, grid = wide_mean_center$grid)
}
