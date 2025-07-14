prepare_split_data_pd = function(effect, data, target.feature.name = NULL, feature.set = NULL, split.feature = NULL) {
  # X: full feature set
  X = data[, setdiff(colnames(data), target.feature.name)]
  # Z \subseteq X: split feature set
  Z = if (!is.null(split.feature)) {
    X[, split.feature, drop = FALSE]
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
