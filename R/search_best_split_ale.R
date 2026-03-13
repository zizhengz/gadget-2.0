#' Find best ALE split (internal).
#' @param Z,effect,min_node_size,n_quantiles,with_stab Split/search arguments.
#' @keywords internal
search_best_split_ale = function(
  Z, effect,
  min_node_size = 1L,
  n_quantiles = NULL,
  with_stab = FALSE
) {
  #### Helper: Build per-feature interval statistics ####
  build_stats = function(effect, features) {
    p = length(features)
    stats_list = vector("list", p)
    # row_pos_list: p x n, interval index per sample
    row_pos_list = vector("list", p)
    # dL_list: p x n, dL per sample
    dL_list = vector("list", p)
    # K: p x 1, number of intervals per feature
    K = integer(p)
    risk_from_stats = function(n, s1, s2) ifelse(n <= 1L, 0.0, s2 - (s1 * s1) / n)
    for (j in seq_len(p)) {
      DT = effect[[features[j]]]
      # Ensure DT is ordered by row_id to maintain alignment with original data rows
      # row_id column ensures correct mapping even if DT was reordered
      data.table::setorder(DT, row_id)
      # sort by interval, one row per interval
      S = unique(DT[, list(interval_index, n = int_n, s1 = int_s1, s2 = int_s2)])
      data.table::setorder(S, interval_index)
      # sufficient statistics of feature j
      stats_list[[j]] = S
      # map the sample-level interval_index to row numbers in S
      row_pos_list[[j]] = match(DT$interval_index, S$interval_index)
      # dL for n samples of feature j (order matches original data rows via row_id)
      dL_list[[j]] = DT$dL
      # number of intervals of feature j
      K[j] = nrow(S)
    }

    # |feat1 int1 | feat1 int2 | feat2 int1 | feat2 int2 | feat2 int3 |
    # |feat3 int1 | feat3 int2 | feat3 int3 |
    # |----- offsets[1]=0 -----|---- offsets[2]=2 -----|---- offsets[3]=5 ----|
    # offsets[j]: number of intervals for the first (j−1) features
    offsets = c(0L, cumsum(K))
    # M: total number of intervals across all features of interest
    M = offsets[length(offsets)]
    # initial offset of each feature, then the position of the k-th interval for feature j is: offsets[j] + k
    offsets = offsets[-length(offsets)]

    tot_n = numeric(M)
    tot_s1 = numeric(M)
    tot_s2 = numeric(M)
    r_n = numeric(M)
    r_s1 = numeric(M)
    r_s2 = numeric(M)
    r_risks = numeric(p)
    pos = 1L
    for (j in seq_len(p)) {
      m = K[j]
      S = stats_list[[j]]
      rng = pos:(pos + m - 1L)
      tot_n[rng] = S$n
      tot_s1[rng] = S$s1
      tot_s2[rng] = S$s2
      r_n[rng] = S$n
      r_s1[rng] = S$s1
      r_s2[rng] = S$s2
      r_risks[j] = sum(risk_from_stats(S$n, S$s1, S$s2))
      pos = pos + m
    }
    N = length(effect[[1]]$dL)
    # dL of the j-th feature on the i-th sample
    dL_mat = matrix(0.0, nrow = p, ncol = N)
    # interval that the i-th sample of the j-th feature belongs to
    interval_idx_mat = matrix(0L, nrow = p, ncol = N)
    for (j in seq_len(p)) {
      dL_mat[j, ] = dL_list[[j]]
      interval_idx_mat[j, ] = row_pos_list[[j]]
    }
    list(K = K, offsets = offsets,
      tot_n = tot_n, tot_s1 = tot_s1, tot_s2 = tot_s2,
      r_n = r_n, r_s1 = r_s1, r_s2 = r_s2, r_risks = r_risks,
      dL_mat = dL_mat, interval_idx_mat = interval_idx_mat)
  }
  #####
  split_feature_names = colnames(Z)
  if (is.null(split_feature_names)) stop("Z (split features) must have column names.")
  t_start = proc.time()

  st_table = build_stats(effect, names(effect))
  # Per split_feature, compute best split once and capture per-feature vectors
  per_feature_res = lapply(split_feature_names, function(split_feat) {
    if (with_stab) {
      res = search_best_split_point_ale_with_cpp(
        z = Z[[split_feat]],
        effect = effect,
        st_table = st_table,
        split_feat = split_feat,
        is_categorical = is.factor(Z[[split_feat]]),
        n_quantiles = n_quantiles,
        min_node_size = min_node_size
      )
    } else {
      res = search_best_split_point_ale(
        z = Z[[split_feat]],
        effect = effect,
        st_table = st_table,
        split_feat = split_feat,
        is_categorical = is.factor(Z[[split_feat]]),
        n_quantiles = n_quantiles,
        min_node_size = min_node_size
      )
    }
    res$split_feature = split_feat
    res$is_categorical = is.factor(Z[[split_feat]])
    res
  })
  t_end = proc.time()

  # Long format rows
  res = data.table::rbindlist(lapply(per_feature_res, function(res) {
    feature_names = names(effect)
    ovj = res$objective_value_j
    ovjl = res$left_objective_value_j
    ovjr = res$right_objective_value_j
    if (length(ovj) == 1 && is.na(ovj)) ovj = rep(NA_real_, length(feature_names))
    if (length(ovjl) == 1 && is.na(ovjl)) ovjl = rep(NA_real_, length(feature_names))
    if (length(ovjr) == 1 && is.na(ovjr)) ovjr = rep(NA_real_, length(feature_names))
    data.frame(
      split_feature = res$split_feature,
      is_categorical = res$is_categorical,
      split_point = res$split_point,
      split_objective = res$split_objective,
      feature = feature_names,
      objective_value_j = as.numeric(ovj),
      left_objective_value_j = as.numeric(ovjl),
      right_objective_value_j = as.numeric(ovjr),
      stringsAsFactors = FALSE
    )
  }), fill = TRUE)

  min_obj = min(res$split_objective, na.rm = TRUE)
  res$best_split = is.finite(min_obj) & (res$split_objective == min_obj)
  res$split_runtime = (t_end - t_start)[[3]]
  res[, c("split_feature", "is_categorical", "split_point",
      "split_objective", "feature", "objective_value_j",
      "left_objective_value_j", "right_objective_value_j",
      "split_runtime", "best_split")]
}
