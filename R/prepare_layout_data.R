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
          node.id       = if (!is.null(node$id)) node$id else NA,
          id.parent     = if (!is.null(node$id.parent)) node$id.parent else NA,
          child.type    = if (!is.null(node$child.type)) node$child.type else NA,
          split.feature = if (!is.null(node$split.feature)) node$split.feature else NA,
          split.value   = if (!is.null(node$split.value)) node$split.value else NA,
          intImp        = if (!is.null(node$intImp)) node$intImp else NA,
          N             = if (!is.null(node$subset.idx)) length(node$subset.idx) else NA,
          depth         = depth
        )
        k = k + 1
      }
    }
  }
  layout = do.call(rbind.data.frame, rows)
  rownames(layout) = NULL
  layout$label = NA
  for (i in layout$node.id) {
    path_conditions = c()
    current = layout[layout$node.id == i, ]
    while (!is.na(current$id.parent)) {
      parent = layout[layout$node.id == current$id.parent, ]
      if (nrow(parent) != 1) break
      op = current$child.type
      cond = paste0(parent$split.feature, " ", op, " ", round(as.numeric(parent$split.value), 3))
      path_conditions = c(cond, path_conditions)
      current = parent
    }
    this = layout[layout$node.id == i, ]
    child_row = which(layout$node.id == 2 * i)
    child_type = if (length(child_row) > 0) layout$child.type[child_row] else NA_character_
    if (!is.na(this$split.feature)) {
      cond_self = paste0(this$split.feature, " ", ifelse(child_type == "<=", "<=", "="), " ", round(as.numeric(this$split.value), 3))
      path_conditions = paste0(cond_self, "\nheter.reduction: ", round(this$intImp, 3))
    } else {
      path_conditions = path_conditions
    }
    layout[layout$node.id == i, ]$label = paste0("depth_", this$depth, "_id_", i, "  #obs: ", this$N, "\n",
      paste(path_conditions, collapse = " \n& "))
  }
  return(layout)
}
