mean_center_ice = function(effect, feature.set,  mean.center = TRUE) {
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
