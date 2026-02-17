utils::globalVariables(c("x.grid", "dL", "level", "x", "y"))
x.grid = dL = level = x = y = NULL

#' Build per-feature ALE panels (mean curve only)
#'
#' Helper used by \code{plot_tree_ale()} to generate per-feature ALE mean
#' panels (optionally with overlaid observation points).
#'
#' @param curves Output of \code{prepare_plot_data_ale()} for a single node.
#' @param color.ale Character. Color for ALE mean curves.
#' @param ymin Numeric. Lower y-axis limit (auto-computed by \code{plot_tree_ale}).
#' @param ymax Numeric. Upper y-axis limit (auto-computed by \code{plot_tree_ale}).
#' @param show.point Logical. Whether to add observation points to ALE plots.
#' @param point.values Named list where each element is a data.frame with columns
#'   \code{x} (feature values) and \code{y} (target/response), only needed when
#'   \code{show.point = TRUE}.
#' @param x.limits Named list providing per-feature x-axis info. For numeric
#'   features: numeric length-2 vector \code{c(xmin, xmax)}. For categorical
#'   features: character vector with the full level order.
#'
#' @return List named by feature, each element a single ggplot object for the
#'   ALE mean curve (with optional observation points).
#' @keywords internal
plot_regional_ale = function(curves, color.ale = "lightcoral",
  ymin = NULL, ymax = NULL, show.point = FALSE,
  point.values = NULL, x.limits = NULL) {
  if (!length(curves)) return(list())

  feats = names(curves)
  out = lapply(feats, function(feat) {
    mean_dt = curves[[feat]]$mean_effect
    is_numeric = "x.grid" %in% names(mean_dt) && is.numeric(mean_dt$x.grid)
    xlim_feat = if (!is.null(x.limits)) x.limits[[feat]] else NULL

    has_points = show.point && !is.null(point.values) && !is.null(point.values[[feat]])
    obs_df = if (has_points) point.values[[feat]] else NULL

    if (is_numeric) {
      valid = !is.na(mean_dt$dL) & !is.na(mean_dt$x.grid)
      n_x = length(unique(mean_dt$x.grid[valid]))
      has_line = n_x >= 2L

      p_ale = ggplot2::ggplot(mean_dt, ggplot2::aes(x = x.grid, y = dL)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(feat) +
        ggplot2::ylab(expression(hat(f)[j]))

      if (has_line) {
        p_ale = p_ale + ggplot2::geom_line(color = color.ale, linewidth = 1.2)
      } else {
        p_ale = p_ale + ggplot2::geom_point(color = color.ale, size = 1.5)
      }

      if (!is.null(obs_df)) {
        p_ale = p_ale +
          ggplot2::geom_point(
            data = obs_df,
            ggplot2::aes(x = x, y = y),
            inherit.aes = FALSE,
            alpha = 0.3, size = 0.8, color = "black"
          ) +
          ggplot2::expand_limits(y = range(obs_df$y, na.rm = TRUE))
      }
    } else {
      if (!is.null(xlim_feat)) {
        level_order = xlim_feat
      } else {
        level_order = as.character(mean_dt$x.grid)
      }
      mean_dt$level = factor(mean_dt$x.grid, levels = level_order)

      valid = !is.na(mean_dt$dL) & !is.na(mean_dt$level)
      n_x = length(unique(mean_dt$level[valid]))
      has_line = n_x >= 2L

      p_ale = ggplot2::ggplot(mean_dt, ggplot2::aes(x = level, y = dL, group = 1)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(feat) +
        ggplot2::ylab(expression(hat(f)[j]))

      if (has_line) {
        p_ale = p_ale + ggplot2::geom_line(color = color.ale, linewidth = 1.2)
      }
      p_ale = p_ale + ggplot2::geom_point(color = color.ale, size = 2)

      if (!is.null(obs_df)) {
        obs_df$level = factor(obs_df$x, levels = level_order)
        p_ale = p_ale +
          ggplot2::geom_point(
            data = obs_df,
            ggplot2::aes(x = level, y = y),
            inherit.aes = FALSE,
            alpha = 0.3, size = 1.0, color = "black",
            position = ggplot2::position_jitter(width = 0.12, height = 0)
          ) +
          ggplot2::expand_limits(y = range(obs_df$y, na.rm = TRUE))
      }
    }

    coord_args = list()
    if (is_numeric && !is.null(xlim_feat) && length(xlim_feat) == 2L) {
      coord_args$xlim = xlim_feat
    }
    if (!is.null(ymin) && !is.null(ymax)) {
      coord_args$ylim = c(ymin, ymax)
    }
    if (length(coord_args)) {
      p_ale = p_ale + do.call(ggplot2::coord_cartesian, coord_args)
    }

    p_ale
  })

  names(out) = feats
  out
}
