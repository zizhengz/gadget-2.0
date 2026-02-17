#' Find a node by id in a flat list of nodes
#'
#' @param node.list List of node objects (each with an \code{id} field).
#' @param id Scalar; node id to look up.
#' @return The node with \code{id} or \code{NULL}.
#' @keywords internal
find_parent_by_id = function(node.list, id) {
  for (n in node.list) {
    if (!is.null(n) && isTRUE(n$id == id))
      return(n)
  }
  return(NULL)
}
