utils::globalVariables(c("interval.index", "x.left", "x.right", "dL", "x.grid", "level"))
interval.index = x.left = x.right = dL = x.grid = level = NULL

#' Prepare ALE Plot Data for One or More Nodes
#'
#' Extends the original helper to support arbitrary node subsets via row indices.
#' When \code{idx} is a list, a nested list mirroring the input structure is returned.
#'
#' @param effect List returned by \code{calculate_ale()}.
#' @param idx Integer vector of row indices (node subset), list of such vectors,
#'   or NULL for the root node.
#' @param features Character vector of features to include (default: all features in effect).
#' @param mean.center Logical. Whether to mean-center ALE curves (default \code{TRUE}).
#'
#' @return Named list where each element contains a single \code{mean_effect}
#'   data.table for the corresponding feature (or a nested list if \code{idx}
#'   is a list).
prepare_plot_data_ale = function(effect, idx = NULL, features = names(effect),
  mean.center = TRUE) {
  if (is.null(effect) || !length(effect)) {
    return(list())
  }
  if (is.list(idx) && length(idx) && !is.atomic(idx)) {
    idx_names = names(idx)
    res = lapply(
      idx,
      function(one_idx) {
        prepare_plot_data_ale(
          effect,
          idx = one_idx,
          features = features,
          mean.center = mean.center
        )
      }
    )
    if (!is.null(idx_names)) names(res) = idx_names
    return(res)
  }
  feats = intersect(features, names(effect))
  if (!length(feats)) {
    return(list())
  }
  out = lapply(feats, function(feat) {
    dt = data.table::as.data.table(effect[[feat]])
    if (!is.null(idx)) {
      dt = dt[dt[["row.id"]] %in% idx]
    }
    mean_dt = mean_center_ale(dt, mean.center = mean.center)
    mean_dt$feature = feat
    list(mean_effect = mean_dt)
  })
  names(out) = feats
  out
}

#' Internal ALE Curve Computation
#'
#' Performs the per-feature accumulation and centering logic used by
#' \code{prepare_plot_data_ale()} and returns a mean ALE curve.
mean_center_ale = function(feat, mean.center = TRUE) {
  feat$dL[feat$dL == 0] = NA
  data.table::setkeyv(feat, c("interval.index"))
  delta.aggr = feat[, list(
    dL = mean(.SD[[1]], na.rm = TRUE),
    interval.n = .N
  ), by = c("interval.index", "x.left", "x.right"), .SDcols = "dL"]

  if (is.numeric(feat$feat.val)) {
    vals = delta.aggr$dL
    csum = cumsum_na_as_zero(c(0, vals))
    weights = delta.aggr$interval.n
    mid_vals = (csum[-length(csum)] + csum[-1]) / 2
    denom = sum(weights)
    fJ0 = if (denom > 0) sum(mid_vals * weights) / denom else 0
    if (!isTRUE(mean.center)) {
      fJ0 = 0
    }
    x_grid = c(delta.aggr$x.left, tail(delta.aggr$x.right, 1))
    mean_dt = data.table::data.table(
      feature = NA_character_,
      x.grid = x_grid,
      dL = csum - fJ0
    )
  } else if (is.factor(feat$feat.val)) {
    vals = delta.aggr$dL
    csum = cumsum_na_as_zero(c(0, vals))
    weights = delta.aggr$interval.n
    mid_vals = (csum[-length(csum)] + csum[-1]) / 2
    denom = sum(weights)
    fJ0 = if (denom > 0) sum(mid_vals * weights) / denom else 0
    if (!isTRUE(mean.center)) {
      fJ0 = 0
    }
    levs = levels(feat$feat.val)
    if (is.null(levs)) levs = unique(as.character(feat$feat.val))
    level_vals = as.character(delta.aggr$x.left)
    mean_dt = data.table::data.table(
      feature = NA_character_,
      x.grid = level_vals,
      dL = csum[-1] - fJ0
    )
    mean_dt$x.grid = factor(mean_dt$x.grid, levels = levs)
  } else {
    mean_dt = data.table::data.table(feature = NA_character_, x.grid = numeric(0), dL = numeric(0))
  }

  mean_dt
}

#' Cumulative Sum with NA Handling
#'
#' @param values Numeric vector. Values to be cumulatively summed;
#'   any \code{NA} values are treated as \code{0}.
cumsum_na_as_zero = function(values) {
  values[is.na(values)] = 0
  cumsum(values)
}
