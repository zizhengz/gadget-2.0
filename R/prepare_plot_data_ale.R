utils::globalVariables(c("interval_index", "x_left", "x_right", "dL", "x_grid", "level"))
interval_index = x_left = x_right = dL = x_grid = level = NULL

#' Prepare ALE Plot Data for One or More Nodes
#'
#' Given effect (from \code{calculate_ale}), idx (row indices or list of such), features, 
#' mean_center: subsets ALE rows by idx; calls \code{mean_center_ale} per feature for cumulative and optional centering. 
#' Returns named list of \code{mean_effect} data.tables (or nested list if idx is list).
#'
#' @param effect List returned by \code{calculate_ale()}.
#' @param idx Integer vector of row indices (node subset), list of such vectors,
#'   or NULL for the root node.
#' @param features Character vector of features to include (default: all features in effect).
#' @param mean_center Logical. Whether to mean-center ALE curves (default \code{TRUE}).
#'
#' @return Named list where each element contains a single \code{mean_effect}
#'   data.table for the corresponding feature (or a nested list if \code{idx}
#'   is a list).
prepare_plot_data_ale = function(effect, idx = NULL, features = names(effect),
  mean_center = TRUE) {
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
          mean_center = mean_center
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
      dt = dt[dt[["row_id"]] %in% idx]
    }
    mean_dt = mean_center_ale(dt, mean_center = mean_center)
    mean_dt$feature = feat
    list(mean_effect = mean_dt)
  })
  names(out) = feats
  out
}

#' Internal ALE curve computation
#'
#' Given ALE data.table for one feature and mean_center: cumsums dL by interval; 
#' optionally subtracts global mean. Returns data.table with x_grid and .value (cumulative ALE).
#'
#' @param feat data.table with per-interval ALE derivatives and metadata.
#' @param mean_center Logical. Whether to mean-center the resulting ALE
#'   curve (default \code{TRUE}).
mean_center_ale = function(feat, mean_center = TRUE) {
  feat$dL[feat$dL == 0] = NA
  data.table::setkeyv(feat, c("interval_index"))
  delta_aggr = feat[, list(
    dL = mean(.SD[[1]], na.rm = TRUE),
    interval_n = .N
  ), by = c("interval_index", "x_left", "x_right"), .SDcols = "dL"]

  if (is.numeric(feat$feat_val)) {
    vals = delta_aggr$dL
    csum = cumsum_na_as_zero(c(0, vals))
    weights = delta_aggr$interval_n
    mid_vals = (csum[-length(csum)] + csum[-1]) / 2
    denom = sum(weights)
    fJ0 = if (denom > 0) sum(mid_vals * weights) / denom else 0
    if (!isTRUE(mean_center)) {
      fJ0 = 0
    }
    x_grid = c(delta_aggr$x_left, tail(delta_aggr$x_right, 1))
    mean_dt = data.table::data.table(
      feature = NA_character_,
      x_grid = x_grid,
      dL = csum - fJ0
    )
  } else if (is.factor(feat$feat_val)) {
    vals = delta_aggr$dL
    csum = cumsum_na_as_zero(c(0, vals))
    weights = delta_aggr$interval_n
    mid_vals = (csum[-length(csum)] + csum[-1]) / 2
    denom = sum(weights)
    fJ0 = if (denom > 0) sum(mid_vals * weights) / denom else 0
    if (!isTRUE(mean_center)) {
      fJ0 = 0
    }
    levs = levels(feat$feat_val)
    if (is.null(levs)) levs = unique(as.character(feat$feat_val))
    level_vals = as.character(delta_aggr$x_left)
    mean_dt = data.table::data.table(
      feature = NA_character_,
      x_grid = level_vals,
      dL = csum[-1] - fJ0
    )
    mean_dt$x_grid = factor(mean_dt$x_grid, levels = levs)
  } else {
    mean_dt = data.table::data.table(feature = NA_character_, x_grid = numeric(0), dL = numeric(0))
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
