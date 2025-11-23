prepare_plot_data_ale = function(effect) {

  effect_cum = lapply(effect, function(feat) {
    res = mean_center_ale(feat)
    list("mean_effect" = res$mean_effect, "sd_effect" = res$sd_effect)
  })
  names(effect_cum) = names(effect)

  return(effect_cum)
}

#' Internal ALE Curve Computation
#'
#' Internal function that performs the actual ALE curve computation including
#' aggregation, accumulation, and centering steps.
#'
#' @param feat Data.table. Single feature's ALE data from calculate_ale.
#'
#' @return List with mean_effect and sd_effect components.
#' @keywords internal
mean_center_ale = function(feat) {
  feat$dL[feat$dL == 0] = NA

  # Average over instances within each interval
  data.table::setkeyv(feat, c("interval.index"))
  delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
    interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.sd = feat[, list(sd = sd(dL, na.rm = TRUE),
    interval.n = .N), by = c("interval.index", "x.left", "x.right")]

  if (is.numeric(feat$feat.val)) {
    # Accumulate over the intervals
    delta.acc = delta.aggr[, list(dL.cumsum = cumsum_na_as_zero(c(0, dL)),
      index0 = c(0, interval.index),
      index1 = c(interval.index, max(interval.index) + 1))]

    # The mean effect is the weighted mean of the interval mid point effects
    # weighted by the number of points in the interval
    fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
      dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]

    # Centering the ALEs
    fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0,
      x.grid = c(delta.aggr$x.left, delta.aggr$x.right[length(delta.aggr$x.right)]))]

  } else if (is.factor(feat$feat.val)) {
    delta.acc = delta.aggr[, list(dL.cumsum = dL, index = interval.index)]
    fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
      dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]
    fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0,
      x.grid = levels(feat$feat.val)[index])]
  }

  list("mean_effect" = fJ, "sd_effect" = delta.sd)
}

#' Cumulative Sum with NA Handling
#'
#' Internal helper function for cumulative sum that treats NA values as zero.
#'
#' @param values Numeric vector that may contain NA values.
#' @return Numeric vector with cumulative sums, NA values treated as zero.
#' @keywords internal
cumsum_na_as_zero = function(values) {
  values[is.na(values)] = 0
  cumsum(values)
}
