plot_node_pd = function(plot.list, depth, node.idx) {
  checkmate::assert_list(plot.list)
  if (!is.null(plot.list[[depth]][[node.idx]]))
    print(plot.list[[depth]][[node.idx]])
}
