plot_tree_pd = function(tree, effect, data, target.feature.name,
  color.ice = "lightblue", color.pd = "lightcoral",
  show.plot = TRUE, show.point = FALSE, mean.center = TRUE,
  depth = NULL, node.id = NULL, features = NULL) {

  # Pre-process data once for all depths
  wide.mean.center = mean_center_ice(effect, feature.set = features, mean.center = mean.center)
  Y = wide.mean.center$Y
  grid.total = wide.mean.center$grid

  # Pre-calculate all node data
  all_node_data = preprocess_node_data(tree, Y, grid.total, mean.center)

  # Determine depths and nodes to render
  if (is.null(node.id)) {
    # If no specific nodes requested, render all depths
    depths_to_render = if (is.null(depth)) {
      seq_along(tree)
    } else {
      depths = suppressWarnings(as.integer(depth))
      depths[!is.na(depths) & depths > 0 & depths <= length(tree)]
    }
  } else {
    # If specific nodes requested, find which depths contain these nodes
    node_ids = suppressWarnings(as.integer(node.id))
    depths_to_render = c()
    for (depth_idx in seq_along(tree)) {
      nodes_at_depth = sapply(tree[[depth_idx]], function(n) !is.null(n) && n$id %in% node_ids)
      if (any(nodes_at_depth)) {
        depths_to_render = c(depths_to_render, depth_idx)
      }
    }
    # If depth is specified, filter to only those depths
    if (!is.null(depth)) {
      depths = suppressWarnings(as.integer(depth))
      depths = depths[!is.na(depths) & depths > 0 & depths <= length(tree)]
      depths_to_render = intersect(depths_to_render, depths)
    }
  }

  if (length(depths_to_render) == 0) {
    warning("No valid depths to render")
    return(invisible(list()))
  }

  plot.list = list()

  for (depth_idx in depths_to_render) {
    prepared.data = all_node_data[[depth_idx]]
    if (is.null(node.id)) {
      nodes_to_render = seq_along(tree[[depth_idx]])
    } else {
      node_ids = suppressWarnings(as.integer(node.id))
      nodes_to_render = which(sapply(tree[[depth_idx]], function(n) !is.null(n) && n$id %in% node_ids))
    }
    if (length(nodes_to_render) == 0) {
      stop(paste("No valid nodes to render at depth", depth_idx))
    }
    plots.at.depth = create_plots_for_depth(tree, prepared.data, data, target.feature.name,
                                           depth_idx, nodes_to_render, color.ice, color.pd,
                                           show.plot, show.point, mean.center)
    plot.list[[paste0("Depth_", depth_idx)]] = plots.at.depth
  }

  invisible(plot.list)
}

# Helper function to preprocess node data
preprocess_node_data = function(tree, Y, grid.total, mean.center) {
  all_node_data = list()
  for (depth_idx in seq_along(tree)) {
    nodes = tree[[depth_idx]]
    Y_processed = lapply(names(Y), function(i) {
      Y.i = Y[[i]]
      Y.i$node = NA_integer_
      grid.i.total = grid.total[[i]]
      for (node.idx in seq_along(nodes)) {
        node = nodes[[node.idx]]
        if (!is.null(node)) {
          subset.idx = node$subset.idx
          grid.i.curr = node$grid[[i]]
          Y.i$node[subset.idx] = node.idx

          if (length(grid.i.curr) < length(grid.i.total)) {
            Y.i[subset.idx, which(!(grid.i.total %in% grid.i.curr))] = NA
            if (mean.center && length(grid.i.curr) > 1) {
              Y.i[subset.idx, grid.i.total] = Y.i[subset.idx, grid.i.total] -
                rowMeans(Y.i[subset.idx, grid.i.total], na.rm = TRUE)
            }
          }
        }
      }
      Y.i
    })
    names(Y_processed) = names(Y)
    all_node_data[[depth_idx]] = Y_processed
  }
  all_node_data
}

# Helper function to create plots for a depth
create_plots_for_depth = function(tree, prepared.data, data, target.feature.name,
  depth_idx, nodes_to_render, color.ice, color.pd,
  show.plot, show.point, mean.center) {
  plots.at.depth = list()
  for (node.idx in nodes_to_render) {
    node = tree[[depth_idx]][[node.idx]]
    if (!is.null(node)) {
      title = create_node_title(node, depth_idx, tree)
      y_range = calculate_y_range(prepared.data, data, target.feature.name, show.point)

      plots = plot_regional_pd(prepared.data = prepared.data,
        origin.data = data,
        target.feature.name = target.feature.name,
        node.idx = node.idx,
        color.ice = color.ice,
        color.pd = color.pd,
        ymin = y_range$ymin,
        ymax = y_range$ymax,
        split.condition = node$split.condition,
        show.point = show.point,
        mean.center = mean.center)

      p = patchwork::wrap_plots(plots, ncol = min(length(prepared.data), 3)) +
        patchwork::plot_annotation(title = title) &
        theme(plot.title = element_text(hjust = 0.5))
      if (show.plot) print(p)
      plots.at.depth[[paste0("Node_", node.idx)]] = p
    }
  }
  plots.at.depth
}

# Helper function to create node title
create_node_title = function(node, depth_idx, tree) {
  n.samples = length(node$subset.idx)
  if (depth_idx == 1) {
    return(paste0("Root node", " (N = ", n.samples, ")"))
  }
  #parent.node = find_parent_by_id(tree[[depth_idx - 1]], node$id.parent)
  path.conditions = track_split_condition(node, tree)
  split.condition = if (length(path.conditions) > 0) paste(path.conditions, collapse = " & ") else NULL
  paste0(depth_idx - 1, ".Split results: ", split.condition, " (N = ", n.samples, ")")
}

# Helper function to calculate y range
calculate_y_range = function(prepared.data, data, target.feature.name, show.point) {
  # Collect ICE values excluding the 'node' column by name from each data.frame
  values.list = lapply(prepared.data, function(df) {
    if (is.data.frame(df) && ncol(df) >= 1) {
      cols = setdiff(colnames(df), "node")
      if (length(cols) == 0) return(numeric(0))
      as.numeric(unlist(df[, cols, drop = FALSE]))
    } else {
      numeric(0)
    }
  })
  effect.values = unlist(values.list, use.names = FALSE)

  ymin.effect = min(effect.values, na.rm = TRUE)
  ymax.effect = max(effect.values, na.rm = TRUE)
  if (show.point) {
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
