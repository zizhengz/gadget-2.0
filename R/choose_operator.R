choose_operator = function(parent.node, current.node) {
  if (!is.null(parent.node)) {
    if (!is.null(parent.node$children[[1]]) &&
      identical(parent.node$children[[1]], current.node)) {
      op = if (is.numeric(parent.node$split.value)) "<=" else "="
    } else if (!is.null(parent.node$children[[2]]) &&
      identical(parent.node$children[[2]], current.node)) {
      op = if (is.numeric(parent.node$split.value)) ">" else "!="
    }
  }
  return (op)
}
