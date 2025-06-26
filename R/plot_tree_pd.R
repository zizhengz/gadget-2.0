plot_tree_pd = function(tree, effect, target.feature.name, color_ice = "lightblue", color_pd = "lightcoral", show.plot = TRUE) {
  plot_list = list()
  for (depth in 1:length(tree)) {
    prepared_regional_pd_data = prepare_regional_pd_data(effect = effect, tree = tree, depth = depth)
    plots_at_depth = list()
    for (node_idx in seq_along(tree[[depth]])) {
      node = tree[[depth]][[node_idx]]
      if (!is.null(node)) {
        # make title
        n_samples = length(node$subset.idx)
        split_condition = NULL
        if (depth == 1) {
          title = paste0("Root node", " (N = ", n_samples, ")")
        } else {
          parent_node = find_parent_by_id(tree[[depth - 1]], node$id.parent)
          op = choose_operator(parent_node, node)
          path_conditions = track_split_condition(node, tree)
          if (length(path_conditions) > 0) {
            split_condition = paste(path_conditions, collapse = " & ")
          } else {
            split_condition = NULL
          }
          title = paste0(depth - 1, ".Split results: ", split_condition, " (N = ", n_samples, ")")
        }

        # make plots
        ymin = min(unlist(prepared_regional_pd_data), na.rm = TRUE)
        ymax = max(unlist(prepared_regional_pd_data), na.rm = TRUE)
        y_range = ymax - ymin
        ymax = ymax + 0.2 * y_range

        plots = plot_regional_pd(prepared_regional_pd_data = prepared_regional_pd_data,
          target.feature.name = target.feature.name,
          node_idx = node_idx,
          color_ice = color_ice,
          color_pd = color_pd,
          ymin = ymin, ymax = ymax,
          split_condition = split_condition)

        p = patchwork::wrap_plots(plots, ncol = if (length(prepared_regional_pd_data) <= 3) length(prepared_regional_pd_data) else 2) +
          patchwork::plot_annotation(title = title) & theme(plot.title = element_text(hjust = 0.5))

        if (show.plot) print(p)

        plots_at_depth[[paste0("Node_", node_idx)]] = p
      }
    }
    plot_list[[paste0("Depth_", depth)]] = plots_at_depth
  }
  return(plot_list)
}
