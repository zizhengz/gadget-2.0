#' Find node by id in a flat list.
#'
#' Given node_list and id: loops and returns first node with \code{node$id == id}, or NULL.
#'
#' @param node_list (`list()`) \cr
#'   List of Node objects (each with an \code{id} field).
#' @param id (`integer(1)`) \cr
#'   Node id to look up.
#' @return (`Node` or `NULL`) \cr
#'   The node with matching \code{id}, or \code{NULL} if not found.
#' @keywords internal
find_node_by_id = function(node_list, id) {
  for (n in node_list) {
    if (!is.null(n) && isTRUE(n$id == id)) {
      return(n)
    }
  }
  NULL
}
