#' Build layout data frame for tree structure plot
#'
#' Given tree (depth-list of Node objects): flattens to one row per node; extracts id, id_parent,
#' split_feature, split_value, N, depth; builds label. Returns data frame for \code{plot_tree_structure} (ggraph).
#'
#' @param tree List of depth levels, each a list of node objects.
#' @return Data frame with columns \code{id}, \code{node_id}, \code{id_parent},
#'   \code{split_feature}, \code{split_value}, \code{label}, \code{depth}, etc.
#' @keywords internal
prepare_layout_data = function(tree) {
  rows = vector("list", 0)
  k = 1
  for (depth in seq_along(tree)) {
    nodes = tree[[depth]]
    for (i in seq_along(nodes)) {
      node = nodes[[i]]
      if (!is.null(node)) {
        rows[[k]] = list(
          id            = paste0(depth, "_", i),
          node_id       = if (!is.null(node$id)) node$id else NA,
          id_parent     = if (!is.null(node$id_parent)) node$id_parent else NA,
          child_type    = if (!is.null(node$child_type)) node$child_type else NA,
          split_feature = if (!is.null(node$split_feature)) node$split_feature else NA,
          split_value   = if (!is.null(node$split_value)) node$split_value else NA,
          intImp        = if (!is.null(node$intImp)) node$intImp else NA,
          N             = if (!is.null(node$subset_idx)) length(node$subset_idx) else NA,
          depth         = depth
        )
        k = k + 1
      }
    }
  }
  layout = do.call(rbind.data.frame, rows)
  rownames(layout) = NULL
  layout$label = NA

  # Helper to format split values
  format_val = function(x) {
    num_x = suppressWarnings(as.numeric(x))
    if (!is.na(num_x)) round(num_x, 3) else x
  }

  for (i in layout$node_id) {
    path_conditions = c()
    current = layout[layout$node_id == i, ]
    while (!is.na(current$id_parent)) {
      parent = layout[layout$node_id == current$id_parent, ]
      if (nrow(parent) != 1) break
      op = current$child_type
      cond = paste0(parent$split_feature, " ", op, " ", format_val(parent$split_value))
      path_conditions = c(cond, path_conditions)
      current = parent
    }
    this = layout[layout$node_id == i, ]
    child_row = which(layout$node_id == 2 * i)
    child_type = if (length(child_row) > 0) layout$child_type[child_row] else NA_character_
    if (!is.na(this$split_feature)) {
      cond_self = paste0(this$split_feature, " ", ifelse(child_type == "<=", "<=", "="), " ", format_val(this$split_value))
      path_conditions = paste0(cond_self, "\nheter_reduction: ", round(this$intImp, 3))
    } else {
      path_conditions = path_conditions
    }
    layout[layout$node_id == i, ]$label = paste0("depth_", this$depth, "_id_", i, "  #obs: ", this$N, "\n",
      paste(path_conditions, collapse = " \n& "))
  }
  return(layout)
}
