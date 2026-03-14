#' Build per-feature ALE panels (mean curve only)
#'
#' Helper used by \code{plot_tree_ale()} to generate per-feature ALE mean
#' panels (optionally with overlaid observation points).
#'
#' @param curves (`list()`) \cr
#'   Output of \code{prepare_plot_data_ale} for a node.
#' @param color_ale (`character(1)`) \cr
#'   Color for ALE curves.
#' @param ymin,ymax (`numeric(1)` or `NULL`) \cr
#'   Y-axis limits.
#' @param show_point (`logical(1)`) \cr
#'   Whether to add observation points.
#' @param point_values (`list()` or `NULL`) \cr
#'   Per-feature data.frames with \code{x}, \code{y}; used when \code{show_point = TRUE}.
#' @param x_limits (`list()` or `NULL`) \cr
#'   Per-feature x-axis: numeric \code{c(xmin, xmax)} or character (level order).
#'
#' @return (`list()`) \cr
#'   Named list of ggplot objects per feature.
#' @keywords internal
plot_regional_ale = function(curves, color_ale = "lightcoral",
  ymin = NULL, ymax = NULL, show_point = FALSE,
  point_values = NULL, x_limits = NULL) {
  if (!length(curves)) {
    return(list())
  }

  feats = names(curves)
  out = lapply(feats, function(feat) {
    mean_dt = curves[[feat]]$mean_effect
    is_numeric = "x_grid" %in% names(mean_dt) && is.numeric(mean_dt$x_grid)
    xlim_feat = if (!is.null(x_limits)) x_limits[[feat]] else NULL

    has_points = show_point && !is.null(point_values) && !is.null(point_values[[feat]])
    obs_df = if (has_points) point_values[[feat]] else NULL

    if (is_numeric) {
      valid = !is.na(mean_dt$d_l) & !is.na(mean_dt$x_grid)
      n_x = length(unique(mean_dt$x_grid[valid]))
      has_line = n_x >= 2L

      p_ale = ggplot2::ggplot(mean_dt, ggplot2::aes(x = x_grid, y = d_l)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(feat) +
        ggplot2::ylab(expression(hat(f)[j]))

      if (has_line) {
        p_ale = p_ale + ggplot2::geom_line(color = color_ale, linewidth = 1.2)
      } else {
        p_ale = p_ale + ggplot2::geom_point(color = color_ale, size = 1.5)
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
        level_order = as.character(mean_dt$x_grid)
      }
      mean_dt$level = factor(mean_dt$x_grid, levels = level_order)

      valid = !is.na(mean_dt$d_l) & !is.na(mean_dt$level)
      n_x = length(unique(mean_dt$level[valid]))
      has_line = n_x >= 2L

      p_ale = ggplot2::ggplot(mean_dt, ggplot2::aes(x = level, y = d_l, group = 1)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(feat) +
        ggplot2::ylab(expression(hat(f)[j]))

      if (has_line) {
        p_ale = p_ale + ggplot2::geom_line(color = color_ale, linewidth = 1.2)
      }
      p_ale = p_ale + ggplot2::geom_point(color = color_ale, size = 2)

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
