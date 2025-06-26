search_best_split = function(Z, Y, min.node.size, n.quantiles) {
  checkmate::assert_data_frame(Z)
  t1 = proc.time()
  if (class(Y) == "list") {
    res = data.table::rbindlist(
      lapply(Z, function(z) {
        search_split_listY(z = z, Y_list = Y, n.quantiles = n.quantiles,
          is.categorical = is.factor(z),
          min.node.size = min.node.size)
      })
    )
  } else if (class(Y) == "data.frame") {
    res = data.table::rbindlist(
      lapply(Z, function(z) {
        search_split(z = z, Y = Y, n.quantiles = n.quantiles,
          is.categorical = is.factor(z),
          min.node.size = min.node.size)
      })
    )
  }
  t2 = proc.time()
  res$is.categorical = unname(sapply(Z, is.factor))
  res$split.feature = colnames(Z)
  res$best.split = res$objective.value == min(res$objective.value, na.rm = TRUE)
  res$runtime = (t2 - t1)[[3]]
  return(res[, c("split.feature", "is.categorical", "split.point", "objective.value", "runtime", "best.split", "current.objective")])
}
