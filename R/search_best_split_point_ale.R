search_best_split_point_ale = function(
  z, effect, st.table, split.feat,
  is.categorical,
  n.quantiles = NULL,
  min.node.size = 1L
) {
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
      list(ord.idx = ord.idx,
        z.sorted = z.sorted,
        n.obs = n.obs,
        is.cand = is.cand)
    } else {
      z.fac = droplevels(z)
      z.nonNA = which(!is.na(z.fac))
      n.L = nlevels(z.fac)
      if (length(z.nonNA) <= 1L || n.L <= 1L) {
        return(NULL)
      }
      level.id = as.integer(z.fac[z.nonNA])
      dL.split.feat = effect[[split.feat]]$dL
      dL.nonNA = dL.split.feat[z.nonNA]
      avg.dL.by.level = tapply(dL.nonNA, level.id, mean, na.rm = TRUE)
      if (anyNA(avg.dL.by.level)) {
        global.mean = mean(dL.nonNA, na.rm = TRUE)
        avg.dL.by.level[is.na(avg.dL.by.level)] = global.mean
      }
      levels.orded = order(avg.dL.by.level)
      rows.by.level = split(z.nonNA, level.id)
      ord.idx = unlist(rows.by.level[as.character(levels.orded)], use.names = FALSE)
      n.obs = length(ord.idx)
      counts = tabulate(level.id, nbins = n.L)[levels.orded]
      cum.counts = cumsum(counts)
      t.idx = cum.counts[cum.counts < n.obs]
      if (length(t.idx) == 0L) {
        return(NULL)
      }
      is.cand = rep(FALSE, n.obs - 1L)
      is.cand[t.idx] = TRUE
      list(
        ord.idx = ord.idx,
        z.sorted = z.fac[ord.idx],
        n.obs = n.obs,
        is.cand = is.cand,
        level.order = levels.orded,
        cum.counts = cum.counts,
        levels.vec = levels(z.fac)
      )
    }
  }

  # Helper: Move one row across all features (updates flattened vectors and per-feature risks)
  move_row_all = function(row.id) {
    d = st.table$dL.mat[, row.id] # length p, dL of each feature
    interval.idx = st.table$interval.idx.mat[, row.id] # length p, interval number of each feature
    m = offsets + interval.idx # length p, subscript of each feature in flattened vector

    r.n.old = st.table$r.n[m]
    r.s1.old = st.table$r.s1[m]
    r.s2.old = st.table$r.s2[m]
    r.risk.old = risk_from_stats(r.n.old, r.s1.old, r.s2.old)

    r.n.new = r.n.old - 1.0
    r.s1.new = r.s1.old - d
    r.s2.new = r.s2.old - d * d
    r.risk.new = risk_from_stats(r.n.new, r.s1.new, r.s2.new)

    st.table$r.n[m] <<- r.n.new
    st.table$r.s1[m] <<- r.s1.new
    st.table$r.s2[m] <<- r.s2.new
    r.risks <<- r.risks - r.risk.old + r.risk.new

    l.n.old = st.table$tot.n[m] - r.n.old
    l.s1.old = st.table$tot.s1[m] - r.s1.old
    l.s2.old = st.table$tot.s2[m] - r.s2.old
    l.risk.old = risk_from_stats(l.n.old, l.s1.old, l.s2.old)

    l.n.new = st.table$tot.n[m] - r.n.new
    l.s1.new = st.table$tot.s1[m] - r.s1.new
    l.s2.new = st.table$tot.s2[m] - r.s2.new
    l.risk.new = risk_from_stats(l.n.new, l.s1.new, l.s2.new)

    l.risks <<- l.risks - l.risk.old + l.risk.new
    sum(-l.risk.old - r.risk.old + l.risk.new + r.risk.new)
  }

  # Helper: Boundary stabilizer using cumulative sums (numeric feature only)
  # adjust_side_for_feature = function(side, t, w) {
  #   # w = window size (# of points near boundary)
  #   if (w <= 0L) {
  #     return(if (side == "L") l.risks[split.feat.j] else r.risks[split.feat.j])
  #   }
  #   if (side == "L") {
  #     # Left node: ord[1..t]
  #     tot.n = t
  #     if (tot.n <= 1L) {
  #       return(l.risks[split.feat.j])
  #     }
  #     w = min(w, tot.n)
  #     # near: last w samples on the left
  #     s1.near = S1[t] - if (t - w >= 1L) S1[t - w] else 0.0
  #     s2.near = S2[t] - if (t - w >= 1L) S2[t - w] else 0.0
  #
  #     risk.j = l.risks[split.feat.j]
  #     s1.tot = S1[t]
  #     s2.tot = S2[t]
  #   } else {
  #     # Right node: ord[t+1..n.obs]
  #     tot.n = n.obs - t
  #     if (tot.n <= 1L) {
  #       return(r.risks[split.feat.j])
  #     }
  #     w = min(w, tot.n)
  #     # near: first w samples on the right
  #     s1.near = S1[t + w] - S1[t]
  #     s2.near = S2[t + w] - S2[t]
  #
  #     risk.j = r.risks[split.feat.j]
  #     # totals over the right node
  #     s1.tot = S1.tot - S1[t]
  #     s2.tot = S2.tot - S2[t]
  #   }
  #
  #   # far region = node minus near window
  #   n.far = tot.n - w
  #   if (n.far <= 1L) {
  #     return(risk.j)
  #   }
  #   s1.far = s1.tot - s1.near
  #   s2.far = s2.tot - s2.near
  #   var.near = (s2.near - s1.near * s1.near / w) / max(w - 1L, 1L)
  #   var.far = (s2.far - s1.far * s1.far / n.far) / max(n.far - 1L, 1L)
  #   if (is.na(var.far) || is.na(var.near) || var.far <= 0 || var.near < 4.0 * var.far) {
  #     return(risk.j)
  #   }
  #   risk.j - risk_from_stats(w, s1.near, s2.near) + w * var.far
  # }

  adjust_side_for_feature = function(side, t) {
    # 1. Determine range of current side
    is.left = (side == "L")
    if (is.left) {
      idx.start = 1L
      idx.end = t
      original.risk = l.risks[split.feat.j]
    } else {
      idx.start = t + 1L
      idx.end = n.obs
      original.risk = r.risks[split.feat.j]
    }

    n.side = idx.end - idx.start + 1L
    # 2. if sample size <= 20, do not smooth
    if (n.side <= 20L) {
      return(original.risk)
    }

    # 3. window size w: max(10% of side samples, 10)
    w = max(round(0.1 * n.side), 10L)

    # 4. Get interval indices and dL for current side
    node.int = interval.idx.sorted[idx.start:idx.end]
    node.dL = dL.j.sorted[idx.start:idx.end]

    # 5. Get unique set of interval indices within the window
    if (is.left) {
      # Left window: last w elements
      window.indices = (n.side - w + 1L):n.side
    } else {
      # Right window: first w elements
      window.indices = 1L:w
    }
    target.intervals = unique(node.int[window.indices])

    # 6. Define Near / Far samples
    # As long as it belongs to target_intervals, treat as Near (regardless of whether inside window w)
    is.near = node.int %in% target.intervals

    # 7. Sample size check
    n.near = sum(is.near)
    n.far = n.side - n.near
    if (n.near < 2 || n.far < 2) {
      return(original.risk)
    }

    # 8. Get dL and calculate SD
    vals.near = node.dL[is.near]
    vals.far = node.dL[!is.near]
    sd.near = sd(vals.near)
    sd.far = sd(vals.far)

    # 9. Condition and replacement
    if (!is.na(sd.near) && !is.na(sd.far) && sd.near > 2.0 * sd.far) {
      node.dL[is.near] = rnorm(n = n.near, mean = mean(vals.far), sd = sd.far)
      s1.vec = rowsum(node.dL, group = node.int, reorder = FALSE)
      s2.vec = rowsum(node.dL^2, group = node.int, reorder = FALSE)
      n.vec  = rowsum(rep(1L, length(node.dL)), group = node.int, reorder = FALSE)
      interval.sse = s2.vec - (s1.vec^2) / n.vec
      return(sum(interval.sse))
    }
    return(original.risk)
  }

  feature.names = names(effect)
  p = length(feature.names)
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

  offsets = st.table$offsets
  r.risks = st.table$r.risks
  l.risks = numeric(p)

  split.feat.j = match(split.feat, feature.names)
  # cumulative dL sums of current (only if numeric) split.feat by ord.idx for O(1) neighborhood stats calculation in boundary stabilizer
  if (!is.categorical) {
    dL.j.sorted = st.table$dL.mat[split.feat.j, ord.idx] # N x 1
    # S1 = cumsum(dL.j.sorted)
    # S2 = cumsum(dL.j.sorted * dL.j.sorted)
    # S1.tot = S1[n.obs]
    # S2.tot = S2[n.obs]

    #### new ####
    interval.idx.sorted = st.table$interval.idx.mat[split.feat.j, ord.idx]
    #### new ####
  }

  # Main sweep
  risks.sum = sum(r.risks)
  best.risks.sum = Inf
  best.t = NA_integer_
  # w.base = if (!is.categorical) max(round(0.1 * n.obs, 0), 10L) else 0L
  best.l.risks = NULL
  best.r.risks = NULL
  for (t in 1:(n.obs - 1L)) {
    risks.sum = risks.sum + move_row_all(ord.idx[t])
    if (!is.cand.t[t]) next
    if (t < min.node.size || (n.obs - t) < min.node.size) next
    if (!is.categorical) {
      # risk.t.j = l.risks[split.feat.j] + r.risks[split.feat.j]
      # # w.l = min(w.base, t)
      # # w.r = min(w.base, n.obs - t)
      # # adj.l.risk.t.j = adjust_side_for_feature("L", t, w.l)
      # # adj.r.risk.t.j = adjust_side_for_feature("R", t, w.r)
      # adj.l.risk.t.j = adjust_side_for_feature("L", t)
      # adj.r.risk.t.j = adjust_side_for_feature("R", t)
      # adj.risk.t.j = adj.l.risk.t.j + adj.r.risk.t.j
      # total = risks.sum - risk.t.j + adj.risk.t.j
      total = risks.sum
    } else {
      total = risks.sum
    }
    if (!is.na(total) && !is.infinite(total) && total < best.risks.sum) {
      best.risks.sum = total
      best.t = t
      best.left.risks = l.risks
      best.right.risks = r.risks
      # if (!is.categorical) {
      #   best.left.risks[split.feat.j] = adj.l.risk.t.j
      #   best.right.risks[split.feat.j] = adj.r.risk.t.j
      # }
    }
  }
  if (is.na(best.t)) {
    return(list(
      split.point = NA_real_,
      split.objective = Inf,
      objective.value.j = rep(NA_real_, p),
      left.objective.value.j = rep(NA_real_, p),
      right.objective.value.j = rep(NA_real_, p)
    ))
  }
  best.split.point = NULL
  if (!is.categorical) {
    left.value = max(z.sorted[1:best.t])
    right.value = min(z.sorted[-(1:best.t)])
    best.split.point = (left.value + right.value) / 2
  } else {
    k = which(plan$cum.counts == best.t)[1]
    best.split.point = if (!is.na(k)) plan$levels.vec[plan$level.order[k]] else NA
  }
  objective.value.j = best.left.risks + best.right.risks
  list(
    split.point = best.split.point,
    split.objective = best.risks.sum,
    objective.value.j = objective.value.j,
    left.objective.value.j = best.left.risks,
    right.objective.value.j = best.right.risks
  )
}
