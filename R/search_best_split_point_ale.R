search_best_split_point_ale = function(
  z, effect, st.table, split.feat,
  is.categorical,
  n.quantiles = NULL,
  min.node.size = 1L
) {
  feature.names = names(effect)
  p = length(feature.names)
  split.feat.j = match(split.feat, feature.names)
  has.self.ale = !is.na(split.feat.j)

  # Helper: Calculate interval-wise SSE
  risk_from_stats = function(n, s1, s2) ifelse(n <= 1L, 0.0, s2 - (s1 * s1) / n)

  # Helper: Find split candidates
  build_order_and_candidates = function() {
    if (!is.categorical) {
      ord.idx = order(z, na.last = NA)
      z.sorted = z[ord.idx]
      n.obs = length(z.sorted)
      if (n.obs <= 1L) {
        return(NULL)
      }
      if (!is.null(n.quantiles) && length(unique(z.sorted)) >= n.quantiles) {
        probs = seq(0, 1, length.out = n.quantiles + 2L)[-c(1L, n.quantiles + 2L)]
        splits = unique(as.numeric(quantile(z.sorted, probs, type = 7)))
      } else {
        splits = unique(z.sorted)
      }
      t.idx = findInterval(splits, z.sorted)
      if (length(t.idx) == 0L) {
        return(NULL)
      }
      is.cand = rep(FALSE, n.obs - 1L)
      valid.t = t.idx[t.idx >= 1L & t.idx <= (n.obs - 1L)]
      is.cand[unique(valid.t)] = TRUE
      list(ord.idx = ord.idx, z.sorted = z.sorted, n.obs = n.obs, is.cand = is.cand)
    } else {
      z.fac = droplevels(z)
      z.nonNA = which(!is.na(z.fac))
      if (length(z.nonNA) <= 1L) return(NULL)
      level.id = as.integer(z.fac[z.nonNA])
      ord.idx = z.nonNA[order(level.id)]
      n.obs = length(ord.idx)
      counts = tabulate(level.id)
      t.idx = head(cumsum(counts), -1L)
      if (length(t.idx) == 0L) return(NULL)
      is.cand = rep(FALSE, n.obs - 1L)
      is.cand[t.idx] = TRUE
      list(
        ord.idx = ord.idx,
        z.sorted = z.fac[ord.idx],
        n.obs = n.obs,
        is.cand = is.cand,
        boundary.pos = t.idx,
        levels.vec = levels(z.fac)
      )
    }
  }

  plan = build_order_and_candidates()
  if (is.null(plan)) {
    return(list(
      split.point = NA_real_,
      split.objective = Inf,
      objective.value.j = rep(NA_real_, p),
      left.objective.value.j = rep(NA_real_, p),
      right.objective.value.j = rep(NA_real_, p)
    ))
  }
  ord.idx = plan$ord.idx
  z.sorted = plan$z.sorted
  n.obs = plan$n.obs
  is.cand.t = plan$is.cand

  split_feat_j_arg = if (has.self.ale) split.feat.j else 0L
  z_sorted_num = if (is.numeric(z.sorted)) as.numeric(z.sorted) else rep(0.0, n.obs)

  cpp_res = ale_sweep_cpp(
    ord_idx = ord.idx,
    dL_mat = st.table$dL.mat,
    interval_idx_mat = st.table$interval.idx.mat,
    offsets = st.table$offsets,
    tot_n = st.table$tot.n,
    tot_s1 = st.table$tot.s1,
    tot_s2 = st.table$tot.s2,
    r_risks = st.table$r.risks,
    is_cand = is.cand.t,
    min_node_size = min.node.size,
    split_feat_j = split_feat_j_arg,
    use_stabilizer = FALSE,
    z_sorted = z_sorted_num,
    n_obs = n.obs
  )
  best.t = cpp_res$best_t
  best.risks.sum = cpp_res$best_risks_sum
  best.left.risks = cpp_res$best_left_risks
  best.right.risks = cpp_res$best_right_risks

  if (is.na(best.t) || best.t < 0L) { # in cpp: best_t = -1 means no valid split point found
    return(list(
      split.point = NA_real_,
      split.objective = Inf,
      objective.value.j = rep(NA_real_, p),
      left.objective.value.j = rep(NA_real_, p),
      right.objective.value.j = rep(NA_real_, p)
    ))
  }
  if (!is.categorical) {
    left.value = max(z.sorted[1:best.t])
    right.value = min(z.sorted[-(1:best.t)])
    best.split.point = (left.value + right.value) / 2
  } else {
    k = which(plan$boundary.pos == best.t)[1]
    best.split.point = if (!is.na(k)) plan$levels.vec[k] else NA
  }
  list(
    split.point = best.split.point,
    split.objective = best.risks.sum,
    objective.value.j = best.left.risks + best.right.risks,
    left.objective.value.j = best.left.risks,
    right.objective.value.j = best.right.risks
  )
}
