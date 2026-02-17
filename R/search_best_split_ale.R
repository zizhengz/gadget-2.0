search_best_split_ale = function(
  Z, effect,
  min.node.size = 1L,
  n.quantiles = NULL,
  with_stab = FALSE
) {
  #### Helper: Build per-feature interval statistics ####
  build_stats = function(effect, features) {
    p = length(features)
    stats.list = vector("list", p)
    # row.pos.list: p x n, interval index per sample
    row.pos.list = vector("list", p)
    # dL.list: p x n, dL per sample
    dL.list = vector("list", p)
    # K: p x 1, number of intervals per feature
    K = integer(p)
    risk_from_stats = function(n, s1, s2) ifelse(n <= 1L, 0.0, s2 - (s1 * s1) / n)
    for (j in seq_len(p)) {
      DT = effect[[features[j]]]
      # Ensure DT is ordered by row.id to maintain alignment with original data rows
      # row.id column ensures correct mapping even if DT was reordered
      data.table::setorder(DT, row.id)
      # sort by interval, one row per interval
      S = unique(DT[, list(interval.index, n = int_n, s1 = int_s1, s2 = int_s2)])
      data.table::setorder(S, interval.index)
      # sufficient statistics of feature j
      stats.list[[j]] = S
      # map the sample-level interval.index to row numbers in S
      row.pos.list[[j]] = match(DT$interval.index, S$interval.index)
      # dL for n samples of feature j (order matches original data rows via row.id)
      dL.list[[j]] = DT$dL
      # number of intervals of feature j
      K[j] = nrow(S)
    }

    # |feature1 interval1 | feature1 interval2 | feature2 interval1 | feature2 interval2 | feature2 interval3 | feature3 interval1 | feature3 interval2 | feature3 interval3 |
    # |----- offsets[1]=0 -----|---- offsets[2]=2 -----|---- offsets[3]=5 ----|
    # offsets[j]: number of intervals for the first (j−1) features
    offsets = c(0L, cumsum(K))
    # M: total number of intervals across all features of interest
    M = offsets[length(offsets)]
    # initial offset of each feature, then the position of the k-th interval for feature j is: offsets[j] + k
    offsets = offsets[-length(offsets)]

    tot.n = numeric(M)
    tot.s1 = numeric(M)
    tot.s2 = numeric(M)
    r.n = numeric(M)
    r.s1 = numeric(M)
    r.s2 = numeric(M)
    r.risks = numeric(p)
    pos = 1L
    for (j in seq_len(p)) {
      m = K[j]
      S = stats.list[[j]]
      rng = pos:(pos + m - 1L)
      tot.n[rng] = S$n
      tot.s1[rng] = S$s1
      tot.s2[rng] = S$s2
      r.n[rng] = S$n
      r.s1[rng] = S$s1
      r.s2[rng] = S$s2
      r.risks[j] = sum(risk_from_stats(S$n, S$s1, S$s2))
      pos = pos + m
    }
    N = length(effect[[1]]$dL)
    # dL of the j-th feature on the i-th sample
    dL.mat = matrix(0.0, nrow = p, ncol = N)
    # interval that the i-th sample of the j-th feature belongs to
    interval.idx.mat = matrix(0L, nrow = p, ncol = N)
    for (j in seq_len(p)) {
      dL.mat[j, ] = dL.list[[j]]
      interval.idx.mat[j, ] = row.pos.list[[j]]
    }
    list(K = K, offsets = offsets,
      tot.n = tot.n, tot.s1 = tot.s1, tot.s2 = tot.s2,
      r.n = r.n, r.s1 = r.s1, r.s2 = r.s2, r.risks = r.risks,
      dL.mat = dL.mat, interval.idx.mat = interval.idx.mat)
  }
  #####
  split.feature.names = colnames(Z)
  if (is.null(split.feature.names)) stop("Z (split features) must have column names.")
  t.start = proc.time()

  st.table = build_stats(effect, names(effect))
  # Per split.feature, compute best split once and capture per-feature vectors
  per.feature.res = lapply(split.feature.names, function(split.feat) {
    if (with_stab) {
      res = search_best_split_point_ale_with_cpp(
        z = Z[[split.feat]],
        effect = effect,
        st.table = st.table,
        split.feat = split.feat,
        is.categorical = is.factor(Z[[split.feat]]),
        n.quantiles = n.quantiles,
        min.node.size = min.node.size
      )
    } else {
      res = search_best_split_point_ale(
        z = Z[[split.feat]],
        effect = effect,
        st.table = st.table,
        split.feat = split.feat,
        is.categorical = is.factor(Z[[split.feat]]),
        n.quantiles = n.quantiles,
        min.node.size = min.node.size
      )
    }
    res$split.feature = split.feat
    res$is.categorical = is.factor(Z[[split.feat]])
    res
  })
  t.end = proc.time()

  # Long format rows
  res = data.table::rbindlist(lapply(per.feature.res, function(res) {
    feature.names = names(effect)
    ovj = res$objective.value.j
    ovjl = res$left.objective.value.j
    ovjr = res$right.objective.value.j
    if (length(ovj) == 1 && is.na(ovj)) ovj = rep(NA_real_, length(feature.names))
    if (length(ovjl) == 1 && is.na(ovjl)) ovjl = rep(NA_real_, length(feature.names))
    if (length(ovjr) == 1 && is.na(ovjr)) ovjr = rep(NA_real_, length(feature.names))
    data.frame(
      split.feature = res$split.feature,
      is.categorical = res$is.categorical,
      split.point = res$split.point,
      split.objective = res$split.objective,
      feature = feature.names,
      objective.value.j = as.numeric(ovj),
      left.objective.value.j = as.numeric(ovjl),
      right.objective.value.j = as.numeric(ovjr),
      stringsAsFactors = FALSE
    )
  }), fill = TRUE)

  min.obj = min(res$split.objective, na.rm = TRUE)
  res$best.split = is.finite(min.obj) & (res$split.objective == min.obj)
  res$split.runtime = (t.end - t.start)[[3]]
  res[, c("split.feature", "is.categorical", "split.point",
    "split.objective", "feature", "objective.value.j",
    "left.objective.value.j", "right.objective.value.j",
    "split.runtime", "best.split")]
}
