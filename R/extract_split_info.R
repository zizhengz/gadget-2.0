#' Extract Split Information from Tree Structure
#'
#' Given tree (depth-list of Node objects) and optional split_benchmark: flattens nodes; builds one row per node (depth, id, n.obs, split.feature, split.value, intImp, intImp.j, etc.); merges timing if split_benchmark has node.id/depth. 
#' Returns data frame.
#'
#' @param tree (`list`)
#'   A depth-based list of Node objects, typically produced by `convert_tree_to_list`.
#' @param split_benchmark (`data.frame` or `list`, optional)
#'   Optional. A data frame or list containing split timing information, with columns `node.id` and `depth` (or similar). If provided, timing info will be merged into the output.
#'
#' @return (`data.frame`)
#'   A data frame where each row summarizes a node, including split feature, split value, statistics, and (if available) split timing.
#'
#' @details
#' This function is used internally by the gadgetTree framework to extract and summarize the structure and statistics of effect-based decision trees. It is useful for interpretation, reporting, and benchmarking.
#'
#' @keywords internal
extract_split_info = function(tree, split_benchmark = NULL) {
  # Collect all intImp.j field names
  all_intimp_names = unique(unlist(
    lapply(tree, function(depth) {
      lapply(depth, function(node) if (!is.null(node$intImp.j)) names(node$intImp.j))
    })
  ))
  all_intimp_names = all_intimp_names[!is.na(all_intimp_names)]

  rows = lapply(unlist(tree, recursive = FALSE), function(node) {
    if (is.null(node)) {
      return(NULL)
    }
    n_obs = if (is.null(node$subset.idx) || length(node$subset.idx) == 0) 0 else length(node$subset.idx)
    is_final = isTRUE(node$improvement.met) | isTRUE(node$stop.criterion.met) |
      is.null(node$children) || (is.list(node$children) && all(sapply(node$children, is.null)))
    row = data.frame(
      depth = as.integer(node$depth),
      id = as.integer(node$id),
      n.obs = as.integer(n_obs),
      node.type = if (is.null(node$id)) NA_character_ else if (node$id == 1) "root" else if (node$id %% 2 == 0) "left" else "right",
      split.feature = if (is.null(node$split.feature)) NA_character_ else as.character(node$split.feature),
      split.value = if (is.null(node$split.value)) NA else node$split.value,
      node.objective = if (is.null(node$objective.value)) NA else node$objective.value,
      intImp = if (is.null(node$intImp)) NA else round(node$intImp, 2),
      split.feature.parent = if (is.null(node$split.feature.parent)) NA_character_ else as.character(node$split.feature.parent),
      split.value.parent = if (is.null(node$split.value.parent)) NA else node$split.value.parent,
      objective.value.parent = if (is.null(node$objective.value.parent)) NA else node$objective.value.parent,
      intImp.parent = if (is.null(node$intImp.parent)) NA else round(node$intImp.parent, 2),
      is.final = is_final,
      stringsAsFactors = FALSE
    )
    # Ensure all intImp.* fields exist
    for (nm in all_intimp_names) {
      row[[paste0("intImp.", nm)]] = if (!is.null(node$intImp.j) && nm %in% names(node$intImp.j)) round(node$intImp.j[[nm]], 2) else NA
    }
    row
  })
  df.split = do.call(rbind, rows)
  rownames(df.split) = NULL

  # Field order: place intImp.* fields after intImp
  all_cols = names(df.split)
  intimp_pos = which(all_cols == "intImp")
  intimp_dot_cols = grep("^intImp\\.", all_cols, value = TRUE)
  cols_wo_dot = setdiff(all_cols, intimp_dot_cols)
  new_order = append(cols_wo_dot, intimp_dot_cols, after = intimp_pos)
  df.split = df.split[, new_order]

  # Merge split_benchmark info if provided
  if (!is.null(split_benchmark) && length(split_benchmark) > 0) {
    # Ensure split_benchmark is a data.frame
    if (is.list(split_benchmark) && !is.data.frame(split_benchmark)) {
      split_benchmark = do.call(rbind, lapply(split_benchmark, as.data.frame))
    }
    # Check if split_benchmark is valid data frame with rows
    if (is.data.frame(split_benchmark) && nrow(split_benchmark) > 0) {
      df.split = merge(df.split, split_benchmark, by.x = c("id", "depth"), by.y = c("node.id", "depth"), all.x = TRUE, sort = TRUE)
    }
  }
  df.split
}
