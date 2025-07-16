#' Convert Recursive Tree to Depth-based List Structure
#'
#' Converts a recursively built tree (with Node objects) to a depth-based list structure
#' where each depth contains a list of nodes at that depth.
#'
#' @param root.node A Node object representing the root of the tree
#' @param max.depth Maximum depth to traverse (optional)
#'
#' @return A list where each element represents a depth level and contains a list of Node objects
#' @keywords internal
convert_tree_to_list = function(root.node, max.depth = NULL) {
  if (is.null(root.node)) {
    return(list())
  }
  # Traverse the tree and collect nodes by level (depth)
  traverse_and_collect = function(node, current.depth, depth.list) {
    if (is.null(node)) {
      return(depth.list)
    }
    if (!is.null(max.depth) && current.depth > max.depth) {
      return(depth.list)
    }
    # Ensure the depth exists
    while (length(depth.list) < current.depth) {
      depth.list[[length(depth.list) + 1]] = list()
    }
    # Add node to current depth
    depth.list[[current.depth]][[length(depth.list[[current.depth]]) + 1]] = node
    # Recursively process children
    if (!is.null(node$children)) {
      if (!is.null(node$children$left.child)) {
        depth.list = traverse_and_collect(node$children$left.child,
          current.depth + 1, depth.list)
      }
      if (!is.null(node$children$right.child)) {
        depth.list = traverse_and_collect(node$children$right.child,
          current.depth + 1, depth.list)
      }
    }
    return(depth.list)
  }
  # Start traversal from root
  tree.depths = traverse_and_collect(root.node, 1, list())
  # Clean up empty levels and ensure proper structure
  tree.depths = Filter(function(depth) length(depth) > 0, tree.depths)
  return(tree.depths)
}
