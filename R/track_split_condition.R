#' Build path of split conditions from root to node
#'
#' Given node and tree (depth-list): walks parent chain via \code{find_parent_by_id}; 
#' at each step builds condition string (e.g. "x <= 0.5") via \code{choose_operator}. 
#' Returns character vector of conditions (root to node).
#'
#' @param node Node object (with \code{id.parent}, \code{depth}).
#' @param tree Depth-based list of nodes (from \code{convert_tree_to_list}).
#' @return Character vector of conditions (e.g. \code{"x <= 0.5"}).
#' @keywords internal
track_split_condition = function(node, tree) {
  path.conditions = c()
  current.node = node
  while (!is.null(current.node$id.parent)) {
    parent.node = find_parent_by_id(tree[[current.node$depth - 1]], current.node$id.parent)
    if (is.null(parent.node)) break
    op = choose_operator(parent.node, current.node)
    cond = paste0(parent.node$split.feature, " ", op, " ", round(as.numeric(parent.node$split.value), 3))
    path.conditions = c(cond, path.conditions)
    current.node = parent.node
  }
  return(path.conditions)
}
