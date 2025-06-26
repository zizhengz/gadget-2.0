choose_operator = function(parent_node, current_node) {
  if (!is.null(parent_node)) {
    if (!is.null(parent_node$children[[1]]) &&
      identical(parent_node$children[[1]], current_node)) {
      op = if (is.numeric(parent_node$split.value)) "≤" else "="
    } else if (!is.null(parent_node$children[[2]]) &&
      identical(parent_node$children[[2]], current_node)) {
      op = if (is.numeric(parent_node$split.value)) ">" else "≠"
    }
  }
  return (op)
}
