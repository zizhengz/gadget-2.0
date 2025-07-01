prepare_split_data_pd = function(effect, data, target.feature.name = NULL, feature.set = NULL, split.feature = NULL) {
  checkmate::assert_data_frame(data)
  if (is.null(target.feature.name) || target.feature.name == "") {
    stop("Please choose a feature name from your data as 'target.feature.name'.")
  }
  # X: feature set
  X = data[, setdiff(colnames(data), target.feature.name)]
  # Z: split feature set
  Z = if (!is.null(split.feature)) {
    checkmate::assert_character(split.feature)
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
