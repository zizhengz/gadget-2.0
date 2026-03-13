#' Node Transform ALE
#'
#' Applies node-specific transformations to ALE effect data, including subsetting,
#' single value handling, and categorical feature recalculation when needed.
#'
#' @param Y List. List of ALE effect data for each feature.
#' @param idx Integer vector. Indices of samples in the current node.
#' @param split_feature Character(1) or NULL. Name of the feature used for splitting.
#'   If NULL (first split), no preprocessing is applied.
#' @param model Fitted model object. Used for categorical recalculation.
#' @param data Data frame. Original complete data.
#' @param target_feature_name Character(1). Name of the target variable.
#' @param n_intervals Integer. Number of intervals for numeric features.
#' @param predict_fun Function or NULL. Prediction function.
#'
#' @return List. List of transformed ALE effects.
#'
node_transform_ale = function(Y, idx, split_feature = NULL, model = NULL, data = NULL,
  target_feature_name = NULL, n_intervals = 10, predict_fun = NULL) {
  # Step 1: Subset Y to current node indices using row_id for alignment
  y_subset = lapply(names(Y), function(feat) {
    y_j = Y[[feat]]
    y_j = y_j[y_j$row_id %in% idx, ]
    y_j[, `:=`(
      int_n    = .N,
      int_s1   = sum(d_l),
      int_s2   = sum(d_l^2)
    ), by = interval_index]
    y_j
  })
  names(y_subset) = names(Y)
  # Step 2: Apply postprocessing
  if (!is.null(split_feature)) {
    y_processed = lapply(names(y_subset), function(feat) {
      y_j = y_subset[[feat]]
      # Handle single unique value case
      if (length(unique(y_j$feat_val)) == 1) {
        y_j$d_l = 0
        y_j[, `:=`(
          int_n    = .N,
          int_s1   = sum(d_l),
          int_s2   = sum(d_l^2)
        ), by = interval_index]
      } else if (feat == split_feature && is.factor(y_j$feat_val)) {
        # # Recalculate ALE for categorical split feature
        # # Use row_id to find corresponding rows in original data
        # subset_idx = y_j$row_id
        # # Recalculate ALE for the split feature using subset data
        # res = calculate_ale(
        #   model = model,
        #   data = data[subset_idx, , drop = FALSE],
        #   feature_set = split_feature,
        #   target_feature_name = target_feature_name,
        #   n_intervals = n_intervals,
        #   predict_fun = predict_fun
        # )
        # y_j = res[[split_feature]]
        # # Update row_id to match original data indices
        # y_j$row_id = subset_idx
      }
      # Ensure it's a data.table
      if (!data.table::is.data.table(y_j)) {
        y_j = data.table::as.data.table(y_j)
      }
      y_j
    })
    names(y_processed) = names(y_subset)
    y_processed
  } else {
    y_subset
  }
}
