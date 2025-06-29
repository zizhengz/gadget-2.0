mean_center_ice = function(effect, mean.center = TRUE) {
  effect.results = effect$results
  # for effect.results using FeatureEffect$new()
  if (class(effect.results) == "data.frame") {
    Y = effect.results
    feat = colnames(Y)[1]
    # if the grid values are factor
    if (is.factor(Y$feat)) Y$feat = as.numeric(as.character(Y$feat))
    Y = tidyr::pivot_wider(Y, names_from = feat, values_from = .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
    if (mean.center) Y = Y - rowMeans(Y, na.rm = TRUE)
    grid = colnames(Y)
    grid = list(grid)
    names(grid) = feat
    Y = list(Y)
    names(Y) = feat
    # for effect.results using FeatureEffects$new()
  } else if (class(effect.results) == "list") {
    Y = lapply(effect.results, function(feat) {
      Y.i = feat
      if (is.factor(Y.i$.borders)) Y.i$.borders = as.numeric(as.character(Y.i$.borders))
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
