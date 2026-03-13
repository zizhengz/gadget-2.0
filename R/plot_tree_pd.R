#' Plot PD tree by depth and node (internal).
#' @param tree,effect,data,target_feature_name,color_ice,color_pd,show_plot,
#'   show_point,mean_center,depth,node_id,features Plot arguments.
#' @keywords internal
plot_tree_pd = function(tree, effect, data, target_feature_name,
  color_ice = "lightblue", color_pd = "lightcoral",
  show_plot = TRUE, show_point = FALSE, mean_center = TRUE,
  depth = NULL, node_id = NULL, features = NULL) {

  # Pre-process data once for all depths
  wide_mean_center = mean_center_ice(effect, feature_set = features, mean_center = mean_center)
  Y = wide_mean_center$Y
  grid_total = wide_mean_center$grid

  # Pre-calculate all node data
  all_node_data = preprocess_node_data(tree, Y, grid_total, mean_center)

  # Determine depths and nodes to render
  if (is.null(node_id)) {
    # If no specific nodes requested, render all depths
    depths_to_render = if (is.null(depth)) {
      seq_along(tree)
    } else {
      depths = suppressWarnings(as.integer(depth))
      depths[!is.na(depths) & depths > 0 & depths <= length(tree)]
    }
  } else {
    # If specific nodes requested, find which depths contain these nodes
    node_ids = suppressWarnings(as.integer(node_id))
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

  plot_list = list()

  for (depth_idx in depths_to_render) {
    prepared_data = all_node_data[[depth_idx]]
    if (is.null(node_id)) {
      nodes_to_render = seq_along(tree[[depth_idx]])
    } else {
      node_ids = suppressWarnings(as.integer(node_id))
      nodes_to_render = which(sapply(tree[[depth_idx]], function(n) !is.null(n) && n$id %in% node_ids))
    }
    if (length(nodes_to_render) == 0) {
      stop(paste("No valid nodes to render at depth", depth_idx))
    }
    plots_at_depth = create_plots_for_depth(tree, prepared_data, data, target_feature_name,
      depth_idx, nodes_to_render, color_ice, color_pd,
      show_plot, show_point, mean_center)
    plot_list[[paste0("Depth_", depth_idx)]] = plots_at_depth
  }

  invisible(plot_list)
}

#' Preprocess PD node data by depth (internal).
#' @keywords internal
preprocess_node_data = function(tree, Y, grid_total, mean_center) {
  all_node_data = list()
  for (depth_idx in seq_along(tree)) {
    nodes = tree[[depth_idx]]
    Y_processed = lapply(names(Y), function(i) {
      Y_i = Y[[i]]
      Y_i$node = NA_integer_
      grid_i_total = grid_total[[i]]
      for (node_idx in seq_along(nodes)) {
        node = nodes[[node_idx]]
        if (!is.null(node)) {
          subset_idx = node$subset_idx
          grid_i_curr = node$grid[[i]]
          Y_i$node[subset_idx] = node_idx

          if (length(grid_i_curr) < length(grid_i_total)) {
            Y_i[subset_idx, which(!(grid_i_total %in% grid_i_curr))] = NA
            if (mean_center && length(grid_i_curr) > 1) {
              Y_i[subset_idx, grid_i_total] = Y_i[subset_idx, grid_i_total] -
                rowMeans(Y_i[subset_idx, grid_i_total], na.rm = TRUE)
            }
          }
        }
      }
      Y_i
    })
    names(Y_processed) = names(Y)
    all_node_data[[depth_idx]] = Y_processed
  }
  all_node_data
}

#' Create PD plots for one depth (internal).
#' @keywords internal
create_plots_for_depth = function(tree, prepared_data, data, target_feature_name,
  depth_idx, nodes_to_render, color_ice, color_pd,
  show_plot, show_point, mean_center) {
  plots_at_depth = list()
  for (node_idx in nodes_to_render) {
    node = tree[[depth_idx]][[node_idx]]
    if (!is.null(node)) {
      title = create_node_title(node, depth_idx, tree)
      y_range = calculate_y_range(prepared_data, data, target_feature_name, show_point)

      plots = plot_regional_pd(prepared_data = prepared_data,
        origin_data = data,
        target_feature_name = target_feature_name,
        node_idx = node_idx,
        color_ice = color_ice,
        color_pd = color_pd,
        ymin = y_range$ymin,
        ymax = y_range$ymax,
        split_condition = node$split_condition,
        show_point = show_point,
        mean_center = mean_center)

      p = patchwork::wrap_plots(plots, ncol = min(length(prepared_data), 3)) +
        patchwork::plot_annotation(title = title) &
        theme(plot.title = element_text(hjust = 0.5))
      if (show_plot) print(p)
      plots_at_depth[[paste0("Node_", node_idx)]] = p
    }
  }
  plots_at_depth
}

#' Create node title for PD plot (internal).
#' @keywords internal
create_node_title = function(node, depth_idx, tree) {
  n_samples = length(node$subset_idx)
  if (depth_idx == 1) {
    return(paste0("Root node", " (N = ", n_samples, ")"))
  }
  # parent_node = find_parent_by_id(tree[[depth_idx - 1]], node$id_parent)
  path_conditions = track_split_condition(node, tree)
  split_condition = if (length(path_conditions) > 0) paste(path_conditions, collapse = " & ") else NULL
  paste0(depth_idx - 1, ".Split results: ", split_condition, " (N = ", n_samples, ")")
}

#' Compute y-axis range for PD plots (internal).
#' @keywords internal
calculate_y_range = function(prepared_data, data, target_feature_name, show_point) {
  # Collect ICE values excluding the 'node' column by name from each data.frame
  values_list = lapply(prepared_data, function(df) {
    if (is.data.frame(df) && ncol(df) >= 1) {
      cols = setdiff(colnames(df), "node")
      if (length(cols) == 0) {
        return(numeric(0))
      }
      as.numeric(unlist(df[, cols, drop = FALSE]))
    } else {
      numeric(0)
    }
  })
  effect_values = unlist(values_list, use.names = FALSE)

  ymin_effect = min(effect_values, na.rm = TRUE)
  ymax_effect = max(effect_values, na.rm = TRUE)
  if (show_point) {
    ymin_data = min(data[[target_feature_name]], na.rm = TRUE)
    ymax_data = max(data[[target_feature_name]], na.rm = TRUE)
    ymax = max(ymax_data, ymax_effect)
    ymin = min(ymin_data, ymin_effect)
  } else {
    ymax = ymax_effect
    ymin = ymin_effect
  }
  y_range = ymax - ymin
  ymax = ymax + 0.2 * y_range
  list(ymin = ymin, ymax = ymax)
}
