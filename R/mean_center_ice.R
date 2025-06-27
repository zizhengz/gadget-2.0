mean_center_ice = function(effect) {
  effect.results = effect$results
  # for REPID, i.e. use FeatureEffect$new()
  if (class(effect.results) == "data.frame") {
    Y = effect.results
    feat = colnames(Y)[1]
    if (is.factor(Y$feat)) Y$feat = as.numeric(as.character(Y$feat))
    Y = tidyr::pivot_wider(Y, names_from = feat, values_from = .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
    Y = Y - rowMeans(Y)
    grid = colnames(Y)
    grid = list(grid)
    names(grid) = feat
    Y = list(Y)
    names(Y) = feat
    # for GADGET, i.e. use FeatureEffects$new()
  } else if (class(effect.results) == "list") {
    Y = lapply(effect.results, function(feat) {
      Y_i = feat
      if (is.factor(Y_i$.borders)) Y_i$.borders = as.numeric(as.character(Y_i$.borders))
      Y_i = tidyr::pivot_wider(Y_i, names_from = .borders, values_from = .value)
      Y_i = Y_i[, setdiff(colnames(Y_i), c(".type", ".id", ".feature"))]
      Y_i = Y_i - rowMeans(Y_i)
    })
    grid = lapply(Y, function(Y_i) {
      colnames(Y_i)
    })
  }
  return(list(Y = Y, grid = grid))
}
