#' Extract Split Information from Tree Structure
#'
#' Given tree (depth-list of Node objects) and optional split_benchmark:
#' flattens nodes; builds one row per node (depth, id, n_obs, split_feature,
#' split_value, intImp, intImp_j, etc.); merges timing if split_benchmark has
#' node_id/depth.
#' Returns data frame.
#'
#' @param tree (`list`)
#'   A depth-based list of Node objects, typically produced by `convert_tree_to_list`.
#' @param split_benchmark (`data.frame` or `list`, optional)
#'   Optional. A data frame or list containing split timing information,
#'   with columns `node_id` and `depth` (or similar). If provided, timing
#'   info will be merged into the output.
#'
#' @return (`data.frame`)
#'   A data frame where each row summarizes a node, including split feature,
#'   split value, statistics, and (if available) split timing.
#'
#' @details
#' This function is used internally by the GadgetTree framework to extract
#' and summarize the structure and statistics of effect-based decision trees.
#' It is useful for interpretation, reporting, and benchmarking.
#'
#' @keywords internal
extract_split_info = function(tree, split_benchmark = NULL) {
  # Collect all intImp_j field names
  all_intimp_names = unique(unlist(
    lapply(tree, function(depth) {
      lapply(depth, function(node) if (!is.null(node$intImp_j)) names(node$intImp_j))
    })
  ))
  all_intimp_names = all_intimp_names[!is.na(all_intimp_names)]

  rows = lapply(unlist(tree, recursive = FALSE), function(node) {
    if (is.null(node)) {
      return(NULL)
    }
    n_obs = if (is.null(node$subset_idx) || length(node$subset_idx) == 0) 0 else length(node$subset_idx)
    is_final = isTRUE(node$improvement_met) | isTRUE(node$stop_criterion_met) |
      is.null(node$children) || (is.list(node$children) && all(sapply(node$children, is.null)))
    row = data.frame(
      depth = as.integer(node$depth),
      id = as.integer(node$id),
      n_obs = as.integer(n_obs),
      node_type = if (is.null(node$id)) NA_character_ else
        if (node$id == 1) "root" else if (node$id %% 2 == 0) "left" else "right",
      split_feature = if (is.null(node$split_feature)) NA_character_ else as.character(node$split_feature),
      split_value = if (is.null(node$split_value)) NA else node$split_value,
      node_objective = if (is.null(node$objective_value)) NA else node$objective_value,
      intImp = if (is.null(node$intImp)) NA else round(node$intImp, 2),
      split_feature_parent = if (is.null(node$split_feature_parent))
        NA_character_ else as.character(node$split_feature_parent),
      split_value_parent = if (is.null(node$split_value_parent)) NA else node$split_value_parent,
      objective_value_parent = if (is.null(node$objective_value_parent)) NA else node$objective_value_parent,
      intImp_parent = if (is.null(node$intImp_parent)) NA else round(node$intImp_parent, 2),
      is_final = is_final,
      stringsAsFactors = FALSE
    )
    # Ensure all intImp.* fields exist
    for (nm in all_intimp_names) {
      row[[paste0("intImp_", nm)]] =
        if (!is.null(node$intImp_j) && nm %in% names(node$intImp_j))
          round(node$intImp_j[[nm]], 2) else NA
    }
    row
  })
  df_split = do.call(rbind, rows)
  rownames(df_split) = NULL

  # Field order: place intImp.* fields after intImp
  all_cols = names(df_split)
  intimp_pos = which(all_cols == "intImp")
  intimp_dot_cols = grep("^intImp_", all_cols, value = TRUE)
  cols_wo_dot = setdiff(all_cols, intimp_dot_cols)
  new_order = append(cols_wo_dot, intimp_dot_cols, after = intimp_pos)
  df_split = df_split[, new_order]

  # Merge split_benchmark info if provided
  if (!is.null(split_benchmark) && length(split_benchmark) > 0) {
    # Ensure split_benchmark is a data.frame
    if (is.list(split_benchmark) && !is.data.frame(split_benchmark)) {
      split_benchmark = do.call(rbind, lapply(split_benchmark, as.data.frame))
    }
    # Check if split_benchmark is valid data frame with rows
    if (is.data.frame(split_benchmark) && nrow(split_benchmark) > 0) {
      df_split = merge(df_split, split_benchmark,
        by.x = c("id", "depth"), by.y = c("node_id", "depth"),
        all.x = TRUE, sort = TRUE)
    }
  }
  df_split
}
