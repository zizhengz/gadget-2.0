extract_split_info = function(tree) {
  list.split = lapply(tree, function(depth) {
    lapply(depth, function(node) {
      if (is.null(node)) {
        tree.structure = NULL
        heterogeneity.table = NULL
      } else if (node$improvement.met | node$stop.criterion.met | node$depth == length(tree)) {
        tree.structure = data.frame("depth" = node$depth, "id" = node$id,
          "n.obs" = length(node$subset.idx),
          "child.type" = if (node$id %% 2 == 0) "left" else "right",
          "split.feature" = "none",
          "split.value" = NA,
          "objective.value" = node$objective.value,
          "intImp" = NA,
          "split.feature.parent" = node$split.feature.parent,
          "split.value.parent" = node$split.value.parent,
          "objective.value.parent" = node$objective.value.parent,
          "intImp_parent" = node$intImp.parent,
          "is.final" = TRUE)
        heterogeneity.table = as.data.frame(matrix(NA, nrow = 1, ncol = length(node$intImp.j)))
        names(heterogeneity.table) = paste0("intImp.", names(node$intImp.j))
      } else {
        tree.structure = data.frame("depth" = node$depth, "id" = node$id,
          "n.obs" = length(node$subset.idx),
          "child.type" = ifelse(node$id == 1, "root", if (node$id %% 2 == 0) "left" else "right"),
          "split.feature" = node$split.feature,
          "split.value" = node$split.value,
          "objective.value" = node$objective.value,
          "intImp" = node$intImp,
          "split.feature.parent" = node$split.feature.parent,
          "split.value.parent" = node$split.value.parent,
          "objective.value.parent" = node$objective.value.parent,
          "intImp_parent" = node$intImp.parent,
          "is.final" = FALSE)
        heterogeneity.table = as.data.frame(matrix(NA, nrow = 1, ncol = length(node$intImp.j)))
        heterogeneity.table[1, ] = node$intImp.j
        names(heterogeneity.table) = paste0("intImp.", names(node$intImp.j))
      }
      res = cbind(tree.structure, heterogeneity.table)
      res
    })
  })
  list.split = Filter(function(x) length(x) > 0L, list.split)
  df.split = unlist(list.split, recursive = FALSE)
  df.split = as.data.frame(do.call(rbind, df.split))
  all_cols = names(df.split)
  intimp_pos = which(all_cols == "intImp")
  intimp_dot_cols = grep("^intImp\\.", all_cols, value = TRUE)
  intimp_dot_pos = match(intimp_dot_cols, all_cols)
  cols_wo_dot = all_cols[-intimp_dot_pos]
  new_order = append(cols_wo_dot, intimp_dot_cols, after = intimp_pos)
  df.split = df.split[, new_order]
  return(df.split)
}

# extract_split_info = function(tree, feat_name = NULL) {
#   cat_line = function(...) cat(paste0(..., "\n"))
#
#   if (is.null(feat_name)) {
#     cat_line("🌳 Full Tree Structure:")
#   } else {
#     cat_line(sprintf("Feature %s - 🌳 Full partition tree:", feat_name))
#   }
#   cat_line(strrep("─", 40))
#
#   all_nodes = unlist(tree, recursive = FALSE)
#   all_nodes = Filter(Negate(is.null), all_nodes)
#   node_map = setNames(all_nodes, vapply(all_nodes, function(n) as.character(n$id), character(1)))
#
#   print_node = function(node, prefix = "") {
#     if (is.null(node)) {
#       return()
#     }
#
#     heter_vec = unlist(node$objective.value, recursive = TRUE, use.names = TRUE)
#     heter_strs = if (!is.null(heter_vec)) {
#       if (!is.null(feat_name) && feat_name %in% names(heter_vec)) {
#         paste0(feat_name, ".heter: ", formatC(heter_vec[feat_name], format = "f", digits = 2))
#       } else {
#         paste0(names(heter_vec), ".heter: ", formatC(heter_vec, format = "f", digits = 2))
#       }
#     } else {
#       "heter: NA"
#     }
#     inst = if (!is.null(node$subset.idx)) length(node$subset.idx) else NA_integer_
#     intImp = if (!is.null(node$intImp) && is.numeric(node$intImp)) round(node$intImp, 3) else NULL
#
#     conds = c()
#     current = node
#     while (!is.null(current$id.parent)) {
#       parent = node_map[[as.character(current$id.parent)]]
#       if (is.null(parent)) break
#       is_left = !is.null(parent$children[[1]]) && identical(parent$children[[1]], current)
#       op = if (is_left) {
#         if (is.numeric(parent$split.value)) "≤" else "="
#       } else {
#         if (is.numeric(parent$split.value)) ">" else "≠"
#       }
#       cond = paste0(parent$split.feature, " ", op, " ", round(as.numeric(parent$split.value), 3))
#       conds = c(cond, conds)
#       current = parent
#     }
#
#     split_expr = if (length(conds) > 0) paste(paste(conds, collapse = " & "), " ") else ""
#     show_intImp = !(isTRUE(node$improvement.met) | isTRUE(node$stop.criterion.met) | node$depth == length(tree))
#
#     fields = c(
#       paste0("depth: ", node$depth),
#       paste0("id: ", node$id),
#       if (!is.null(intImp) && show_intImp) paste0("intImp: ", formatC(intImp, format = "f", digits = 3)) else NULL,
#       heter_strs,
#       if (!is.na(inst)) paste0("# inst: ", inst) else NULL
#     )
#
#     if (nzchar(split_expr)) {
#       cat_line(prefix, paste0("✂️ ", split_expr))
#     }
#     cat_line(prefix, "[", paste(fields, collapse = " | "), "]")
#
#
#     # if (is.null(node$children) || (is.null(node$children[[1]]) && is.null(node$children[[2]]))) {
#     #   cat_line(prefix, "    🌿 Leaf Node")
#     # } else {
#     #   print_node(node$children[[1]], paste0(prefix, "    "))
#     #   print_node(node$children[[2]], paste0(prefix, "    "))
#     # }
#     if (!is.list(node$children) || length(node$children) < 2 ||
#       (is.null(node$children[[1]]) && is.null(node$children[[2]]))) {
#       cat_line(prefix, "    🌿 Leaf Node")
#     } else {
#       if (!is.null(node$children[[1]])) print_node(node$children[[1]], paste0(prefix, "    "))
#       if (!is.null(node$children[[2]])) print_node(node$children[[2]], paste0(prefix, "    "))
#     }
#
#   }
#
#   print_node(tree[[1]][[1]])
#   invisible(NULL)
# }
