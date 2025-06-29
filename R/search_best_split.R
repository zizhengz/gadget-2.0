search_best_split = function(Z, Y, min.node.size, n.quantiles) {
  checkmate::assert_data_frame(Z)
  checkmate::assert_list(Y)
  t1 = proc.time()
  res = do.call(rbind,
    lapply(Z, function(z) {
      search_best_split_point(z = z, Y = Y, n.quantiles = n.quantiles,
        is.categorical = is.factor(z),
        min.node.size = min.node.size)
    })
  )
  t2 = proc.time()
  res$split.feature = colnames(Z)
  res$is.categorical = unname(sapply(Z, is.factor))
  res$best.split = res$split.objective == min(res$split.objective, na.rm = TRUE)
  res$split.runtime = (t2 - t1)[[3]]
  return(res[, c("split.feature", "is.categorical", "split.point", "split.objective",
                 "split.runtime", "best.split")])
}
