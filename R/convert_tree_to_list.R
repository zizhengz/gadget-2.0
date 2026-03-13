#' Convert Recursive Tree to Depth-based List
#'
#' Given root_node and optional max_depth: traverses children recursively, collects nodes by depth.
#' Returns list of lists (depth 1, 2, ...), each element a list of Node objects at that depth.
#'
#' @param root_node A Node object representing the root of the tree
#' @param max_depth Maximum depth to traverse (optional)
#'
#' @return A list where each element represents a depth level and contains a list of Node objects
#' @keywords internal
convert_tree_to_list = function(root_node, max_depth = NULL) {
  if (is.null(root_node)) {
    return(list())
  }
  # Traverse the tree and collect nodes by level (depth)
  traverse_and_collect = function(node, current_depth, depth_list) {
    if (is.null(node)) {
      return(depth_list)
    }
    if (!is.null(max_depth) && current_depth > max_depth) {
      return(depth_list)
    }
    # Ensure the depth exists
    while (length(depth_list) < current_depth) {
      depth_list[[length(depth_list) + 1]] = list()
    }
    # Add node to current depth
    depth_list[[current_depth]][[length(depth_list[[current_depth]]) + 1]] = node
    # Recursively process children
    if (!is.null(node$children)) {
      if (!is.null(node$children$left_child)) {
        depth_list = traverse_and_collect(node$children$left_child,
          current_depth + 1, depth_list)
      }
      if (!is.null(node$children$right_child)) {
        depth_list = traverse_and_collect(node$children$right_child,
          current_depth + 1, depth_list)
      }
    }
    depth_list
  }
  # Start traversal from root
  tree_depths = traverse_and_collect(root_node, 1, list())
  # Clean up empty levels and ensure proper structure
  tree_depths = Filter(function(depth) length(depth) > 0, tree_depths)
  tree_depths
}
