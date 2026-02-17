#' Return comparison operator for a child relative to parent split
#'
#' Left child: \code{<=} (numeric) or \code{=} (categorical); right: \code{>} or \code{!=}.
#'
#' @param parent.node Node with \code{children} and \code{split.value}.
#' @param current.node One of the children.
#' @return Character \code{"<="}, \code{">"}, \code{"="}, or \code{"!="}.
#' @keywords internal
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
  return(op)
}
