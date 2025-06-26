find_parent_by_id = function(node_list, id) {
  for (n in node_list) {
    if (!is.null(n) && isTRUE(n$id == id)) {
      return(n)
    }
  }
  return(NULL)
}
