#' Visualize the Tree Structure
#'
#' This function visualizes the decision tree structure using the `ggraph` and `igraph` packages.
#' It creates a plot where nodes are labeled with split information and edges represent the tree hierarchy.
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
  parent.map = setNames(data$id, data$node.id)
  data$parent.id = parent.map[as.character(data$id.parent)]
  edge.list = na.omit(data[, c("parent.id", "id")])
  colnames(edge.list) = c("from", "to")

  g = igraph::graph_from_data_frame(edge.list, vertices = data, directed = TRUE)

  gg = ggraph::ggraph(g, layout = "tree") +
    coord_flip(clip = "off")

  # Only add edges if there are any
  if (nrow(edge.list) > 0) {
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
