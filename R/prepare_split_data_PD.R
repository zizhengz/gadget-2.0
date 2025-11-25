prepare_split_data_pd = function(effect, data, target.feature.name = NULL, feature.set = NULL, split.feature = NULL) {
  # X: full feature set
  feature_names = setdiff(colnames(data), target.feature.name)
  if (data.table::is.data.table(data)) {
    X = data[, feature_names, with = FALSE]
  } else {
    X = data[, feature_names, drop = FALSE]
  }
  # Z \subseteq X: split feature set
  Z = if (!is.null(split.feature)) {
    if (data.table::is.data.table(X)) {
      X[, split.feature, with = FALSE]
    } else {
      X[, split.feature, drop = FALSE]
    }
  } else {
    X
  }
  for (col in names(Z)) {
    if (is.character(Z[[col]])) {
      Z[[col]] = factor(Z[[col]])
    }
  }
  wide.mean.center = mean_center_ice(effect = effect, feature.set = feature.set)
  return(list(Z = Z, Y = wide.mean.center$Y, grid = wide.mean.center$grid))
}
