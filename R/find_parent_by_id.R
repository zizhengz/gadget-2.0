find_parent_by_id = function(node.list, id) {
  for (n in node.list) {
    if (!is.null(n) && isTRUE(n$id == id)) {
      return(n)
    }
  }
  return(NULL)
}
