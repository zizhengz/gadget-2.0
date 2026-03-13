#' Find node by id in a flat list. Given node_list and id: loops and returns first node with \code{node$id == id}, or NULL.
#'
#' @param node_list List of node objects (each with an \code{id} field).
#' @param id Scalar; node id to look up.
#' @return The node with \code{id} or \code{NULL}.
#' @keywords internal
find_parent_by_id = function(node_list, id) {
  for (n in node_list) {
    if (!is.null(n) && isTRUE(n$id == id)) {
      return(n)
    }
  }
  return(NULL)
}
