prepare_tree_structure_data = function(tree) {
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
          depth         = depth
        )
        k = k + 1
      }
    }
  }
  layout = do.call(rbind.data.frame, rows)
  rownames(layout) = NULL
  layout$label = NA
  for (i in seq_len(nrow(layout))) {
    path_conditions = c()
    current = layout[i, ]
    while (!is.na(current$id.parent)) {
      parent = layout[layout$node.id == current$id.parent, ]
      if (nrow(parent) != 1) break
      op = switch(current$child.type, "<=" = {
        "≤"
      }, ">" = {
        ">"
      }, "==" = {
        "="
      }, "!=" = {
        "≠"
      })
      cond = paste0(parent$split.feature, " ", op, " ", round(as.numeric(parent$split.value), 3))
      path_conditions = c(cond, path_conditions)
      current = parent
    }
    if (!is.na(layout$split.feature[i])) {
      cond_self = paste0(layout$split.feature[i], " ", ifelse(layout$child.type[i + 1] == "==", "=", "≤"), " ", round(as.numeric(layout$split.value[i]), 3))
      path_conditions = paste0(cond_self, "\nintImp: ", round(layout$intImp[i], 3))
    } else {
      path_conditions = path_conditions
    }
    layout$label[i] = paste(path_conditions, collapse = " \n& ")
  }
  return(layout)
}
