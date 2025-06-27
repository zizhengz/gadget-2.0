track_split_condition = function(node, tree) {
  path_conditions = c()
  current_node = node
  while (!is.null(current_node$id.parent)) {
    parent_node = find_parent_by_id(tree[[current_node$depth - 1]], current_node$id.parent)
    if (is.null(parent_node)) break
    op = choose_operator(parent_node, current_node)
    cond = paste0(parent_node$split.feature, " ", op, " ", round(as.numeric(parent_node$split.value), 3))
    path_conditions = c(cond, path_conditions)
    current_node = parent_node
  }
  return (path_conditions)
}

