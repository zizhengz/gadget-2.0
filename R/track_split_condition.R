#' Build path of split conditions from root to node
#'
#' Given node and tree (depth-list): walks parent chain via \code{find_parent_by_id};
#' at each step builds condition string (e.g. "x <= 0.5") via \code{choose_operator}.
#' Returns character vector of conditions (root to node).
#'
#' @param node Node object (with \code{id_parent}, \code{depth}).
#' @param tree Depth-based list of nodes (from \code{convert_tree_to_list}).
#' @return Character vector of conditions (e.g. \code{"x <= 0.5"}).
#' @keywords internal
track_split_condition = function(node, tree) {
  path_conditions = c()
  current_node = node
  while (!is.null(current_node$id_parent)) {
    parent_node = find_parent_by_id(tree[[current_node$depth - 1]], current_node$id_parent)
    if (is.null(parent_node)) break
    op = choose_operator(parent_node, current_node)
    cond = paste0(parent_node$split_feature, " ", op, " ", round(as.numeric(parent_node$split_value), 3))
    path_conditions = c(cond, path_conditions)
    current_node = parent_node
  }
  return(path_conditions)
}
