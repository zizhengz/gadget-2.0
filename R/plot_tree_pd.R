plot_tree_pd = function(tree, effect, data, target.feature.name,
                        color.ice = "lightblue", color.pd = "lightcoral",
                        show.plot = FALSE, show.point = FALSE, mean.center = TRUE) {
  plot.list = list()
  for (depth in 1:length(tree)) {
    prepared.data = prepare_plot_data_pd(effect = effect, tree = tree, depth = depth,
                                         mean.center = mean.center)
    plots.at.depth = list()
    for (node.idx in seq_along(tree[[depth]])) {
      node = tree[[depth]][[node.idx]]
      if (!is.null(node)) {
        #### make title ####
        n.samples = length(node$subset.idx)
        split.condition = NULL
        if (depth == 1) {
          title = paste0("Root node", " (N = ", n.samples, ")")
        } else {
          parent.node = find_parent_by_id(tree[[depth - 1]], node$id.parent)
          op = choose_operator(parent.node, node)
          path.conditions = track_split_condition(node, tree)
          if (length(path.conditions) > 0) {
            split.condition = paste(path.conditions, collapse = " & ")
          } else {
            split.condition = NULL
          }
          title = paste0(depth - 1, ".Split results: ", split.condition, " (N = ", n.samples, ")")
        }

        ##### make plots ####
        ymin.data = min(data[[target.feature.name]], na.rm = TRUE)
        ymax.data = max(data[[target.feature.name]], na.rm = TRUE)
        ymin.effect = min(unlist(prepared.data), na.rm = TRUE)
        ymax.effect = max(unlist(prepared.data), na.rm = TRUE)
        if (show.point) {
          ymax = max(ymax.data, ymax.effect)
          ymin = min(ymin.data, ymin.effect)
          y.range = ymax - ymin
          ymax = ymax + 0.2 * y.range
        } else {
          ymax = ymax.effect
          ymin = ymin.effect
          y.range = ymax - ymin
          ymax = ymax + 0.2 * y.range
        }

        plots = plot_regional_pd(prepared.data = prepared.data,
          origin.data = data,
          target.feature.name = target.feature.name,
          node.idx = node.idx,
          color.ice = color.ice,
          color.pd = color.pd,
          ymin = ymin, ymax = ymax,
          split.condition = split.condition,
          show.point = show.point,
          mean.center = mean.center)

        p = patchwork::wrap_plots(plots, ncol = if (length(prepared.data) <= 3) length(prepared.data) else 2) +
          patchwork::plot_annotation(title = title) & theme(plot.title = element_text(hjust = 0.5))

        if (show.plot) print(p)

        plots.at.depth[[paste0("Node_", node.idx)]] = p
      }
    }
    plot.list[[paste0("Depth_", depth)]] = plots.at.depth
  }
  return(plot.list)
}
