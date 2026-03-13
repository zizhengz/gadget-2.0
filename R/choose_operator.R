#' Return comparison operator for child relative to parent. 
#' Given parent_node and current_node: if current is left child returns \code{<=} (numeric) or \code{=} (categorical); 
#' if right returns \code{>} or \code{!=}.
#'
#' @param parent_node Node with \code{children} and \code{split_value}.
#' @param current_node One of the children.
#' @return Character \code{"<="}, \code{">"}, \code{"="}, or \code{"!="}.
#' @keywords internal
choose_operator = function(parent_node, current_node) {
  if (!is.null(parent_node)) {
    if (!is.null(parent_node$children[[1]]) &&
      identical(parent_node$children[[1]], current_node)) {
      op = if (is.numeric(parent_node$split_value)) "<=" else "="
    } else if (!is.null(parent_node$children[[2]]) &&
      identical(parent_node$children[[2]], current_node)) {
      op = if (is.numeric(parent_node$split_value)) ">" else "!="
    }
  }
  return(op)
}
