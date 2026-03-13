#' Visualize the Tree Structure
#'
#' Given tree (depth-list of Node objects): calls
#' \code{prepare_layout_data} to build layout data; creates parent map;
#' builds edge list; creates ggraph plot with nodes labeled by split info
#' and edges representing tree hierarchy. Returns ggplot object.
#'
#' @param tree List. A list containing the tree structure, typically converted from the internal Node object format.
#'
#' @return A `ggplot` object representing the tree structure.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_elbow geom_node_label circle
#' @importFrom ggplot2 aes coord_flip scale_fill_manual theme_void scale_y_reverse theme expansion arrow unit margin
#' @importFrom stats setNames na.omit
#' @importFrom grDevices hcl.colors
#'
#' @keywords internal
plot_tree_structure = function(tree) {
  data = prepare_layout_data(tree)
  parent_map = setNames(data$id, data$node_id)
  data$parent_id = parent_map[as.character(data$id_parent)]
  edge_list = na.omit(data[, c("parent_id", "id")])
  colnames(edge_list) = c("from", "to")

  g = igraph::graph_from_data_frame(edge_list, vertices = data, directed = TRUE)

  gg = ggraph::ggraph(g, layout = "tree") +
    coord_flip(clip = "off")

  # Only add edges if there are any
  if (nrow(edge_list) > 0) {
    gg = gg + ggraph::geom_edge_elbow(
      arrow = arrow(length = unit(0.05, "cm")),
      end_cap = ggraph::circle(1.5, "mm"),
      edge_colour = "grey40",
      edge_width = 0.4
    )
  }

  gg +
    ggraph::geom_node_label(
      aes(label = label, fill = factor(depth)),
      size = 3.5,
      label.padding = unit(0.25, "lines"),
      # label.size = 0.3,
      label.r = unit(0.1, "lines")
    ) +
    scale_fill_manual(values = hcl.colors(n = length(tree), palette = "Set2")) +
    theme_void() +
    scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "mm")
    )
}
