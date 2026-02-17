#' Plot ALE curves for selected tree nodes
#'
#' External helper mirroring \code{plot_tree_pd}, used by \code{aleStrategy$plot}.
#' Produces patchwork objects per node with ALE mean curves and optional observation points.
#' Y-axis limits are auto-computed from effect and target data (like PD strategy).
#'
#' @param tree Depth-based list of Node objects (from \code{convert_tree_to_list()}).
#' @param effect List returned by \code{calculate_ale()}.
#' @param data Data frame or data.table (features and target).
#' @param target.feature.name Character(1). Target variable name.
#' @param depth Integer vector or NULL. Depth levels to render.
#' @param node.id Integer vector or NULL. Specific node IDs to render.
#' @param features Character vector or NULL. Feature subset to plot.
#' @param color.ale Character. Color for ALE mean curves.
#' @param show.plot Logical. Whether to print plots while generating.
#' @param show.point Logical. Whether to show per-node observation points on ALE plots.
#' @param mean.center Logical. Whether to mean-center ALE curves (default \code{TRUE}).
#' @return Nested list (depth -> node -> patchwork object).
#' @export
plot_tree_ale = function(tree, effect, data, target.feature.name,
  depth = NULL, node.id = NULL, features = NULL,
  color.ale = "lightgreen", show.plot = TRUE,
  show.point = FALSE, mean.center = TRUE, ...) {

  if (is.null(features)) features = names(effect)
  features = intersect(features, names(effect))
  if (!length(features)) stop("No overlapping features between 'features' and effect list.")

  select_depths = function(tree, depth, node.id) {
    if (is.null(node.id)) {
      if (is.null(depth)) return(seq_along(tree))
      depths = suppressWarnings(as.integer(depth))
      depths[!is.na(depths) & depths > 0 & depths <= length(tree)]
    } else {
      ids = suppressWarnings(as.integer(node.id))
      depths_hit = integer(0)
      for (depth_idx in seq_along(tree)) {
        nodes = tree[[depth_idx]]
        if (any(sapply(nodes, function(n) n$id %in% ids))) {
          depths_hit = c(depths_hit, depth_idx)
        }
      }
      if (!is.null(depth)) {
        depth_filter = suppressWarnings(as.integer(depth))
        depth_filter = depth_filter[!is.na(depth_filter) & depth_filter > 0 & depth_filter <= length(tree)]
        depths_hit = intersect(depths_hit, depth_filter)
      }
      unique(depths_hit)
    }
  }

  depths_to_render = select_depths(tree, depth, node.id)
  if (!length(depths_to_render)) {
    warning("No valid depths to render for ALE plots.")
    return(invisible(list()))
  }

  build_node_title = function(node, depth_idx, tree) {
    n.samples = length(node$subset.idx)
    if (depth_idx == 1) return(paste0("Root node (N = ", n.samples, ")"))
    conds = track_split_condition(node, tree)
    cond_txt = if (length(conds)) paste(conds, collapse = " & ") else "Split"
    paste0("Depth ", depth_idx, " - Node ", node$id, " (", cond_txt, ", N = ", n.samples, ")")
  }

  global_curves = prepare_plot_data_ale(effect,
    features = features, mean.center = mean.center)
  y_range = calculate_y_range_ale(global_curves, data, target.feature.name, show.point)
  x_limits = lapply(features, function(feat) {
    if (is.null(global_curves[[feat]])) return(NULL)
    mean_dt = global_curves[[feat]]$mean_effect
    if (!("x.grid" %in% names(mean_dt))) return(NULL)
    if (is.numeric(mean_dt$x.grid)) {
      range(mean_dt$x.grid, na.rm = TRUE)
    } else if (is.factor(mean_dt$x.grid)) {
      levels(mean_dt$x.grid)
    } else {
      NULL
    }
  })
  names(x_limits) = features

  plot.list = list()
  for (depth_idx in depths_to_render) {
    nodes = tree[[depth_idx]]
    if (is.null(node.id)) {
      nodes_to_render = seq_along(nodes)
    } else {
      ids = suppressWarnings(as.integer(node.id))
      nodes_to_render = which(sapply(nodes, function(n) n$id %in% ids))
    }
    depth_plots = list()
    for (node_idx in nodes_to_render) {
      node = nodes[[node_idx]]
      curves = prepare_plot_data_ale(effect,
        idx = node$subset.idx, features = features, mean.center = mean.center) 
      point_values = NULL
      if (show.point) {
        point_values = lapply(names(curves), function(feat) {
          data.frame(
            x = data[[feat]][node$subset.idx],
            y = data[[target.feature.name]][node$subset.idx]
          )
        })
        names(point_values) = names(curves)
      }
      feat_panels = plot_regional_ale(curves,
        color.ale = color.ale,
        ymin = y_range$ymin, ymax = y_range$ymax,
        show.point = show.point,
        point.values = point_values,
        x.limits = x_limits)
      if (!length(feat_panels)) next
      stacked = lapply(names(feat_panels), function(feat_name) {
        panel = feat_panels[[feat_name]]
        panel +
          patchwork::plot_annotation(title = feat_name) &
          ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5))
      })
      if (!length(stacked)) next
      ncol_used = max(1L, min(length(stacked), 3L))
      nrow_used = ceiling(length(stacked) / ncol_used)
      combined = patchwork::wrap_plots(stacked, ncol = ncol_used, nrow = nrow_used) +
        patchwork::plot_annotation(title = build_node_title(node, depth_idx, tree)) &
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      combined$layout$ncol = ncol_used
      combined$layout$nrow = nrow_used
      if (show.plot) print(combined)
      depth_plots[[paste0("Node_", node$id)]] = combined
    }
    plot.list[[paste0("Depth_", depth_idx)]] = depth_plots
  }
  invisible(plot.list)
}

#' Compute y-axis range for ALE plots (effect + optional target data, with padding)
#' @keywords internal
calculate_y_range_ale = function(curves, data, target.feature.name, show.point) {
  effect.values = unlist(lapply(curves, function(x) x$mean_effect$dL), use.names = FALSE)
  effect.values = effect.values[!is.na(effect.values)]
  if (!length(effect.values)) {
    return(list(ymin = 0, ymax = 1))
  }
  ymin.effect = min(effect.values)
  ymax.effect = max(effect.values)
  if (!is.null(data) && target.feature.name %in% colnames(data)) {
    ymin.data = min(data[[target.feature.name]], na.rm = TRUE)
    ymax.data = max(data[[target.feature.name]], na.rm = TRUE)
    ymax = max(ymax.data, ymax.effect)
    ymin = min(ymin.data, ymin.effect)
  } else {
    ymax = ymax.effect
    ymin = ymin.effect
  }
  y.range = ymax - ymin
  ymax = ymax + 0.2 * y.range
  list(ymin = ymin, ymax = ymax)
}
