#' Node Transform ALE
#'
#' Applies node-specific transformations to ALE effect data, including subsetting,
#' single value handling, and categorical feature recalculation when needed.
#'
#' @param Y List. List of ALE effect data for each feature.
#' @param idx Integer vector. Indices of samples in the current node.
#' @param split.feature Character(1) or NULL. Name of the feature used for splitting.
#'   If NULL (first split), no preprocessing is applied.
#' @param model Fitted model object. Used for categorical recalculation.
#' @param data Data frame. Original complete data.
#' @param target.feature.name Character(1). Name of the target variable.
#' @param n.intervals Integer. Number of intervals for numeric features.
#' @param predict.fun Function or NULL. Prediction function.
#'
#' @return List. List of transformed ALE effects.
#'
node_transform_ale = function(Y, idx, split.feature = NULL, model = NULL, data = NULL,
  target.feature.name = NULL, n.intervals = 10, predict.fun = NULL) {
  # Step 1: Subset Y to current node indices using row.id for alignment
  Y.subset = lapply(names(Y), function(feat) {
    Y.j = Y[[feat]]
    Y.j = Y.j[Y.j$row.id %in% idx, ]
    Y.j[, `:=`(
      int_n    = .N,
      int_s1   = sum(dL),
      int_s2   = sum(dL^2)
    ), by = interval.index]
    return(Y.j)
  })
  names(Y.subset) = names(Y)
  # Step 2: Apply postprocessing
  if (!is.null(split.feature)) {
    Y.processed = lapply(names(Y.subset), function(feat) {
      Y.j = Y.subset[[feat]]
      # Handle single unique value case
      if (length(unique(Y.j$feat.val)) == 1) {
        Y.j$dL = 0
        Y.j[, `:=`(
          int_n    = .N,
          int_s1   = sum(dL),
          int_s2   = sum(dL^2)
        ), by = interval.index]
      } else if (feat == split.feature && is.factor(Y.j$feat.val)) {
        # # Recalculate ALE for categorical split feature
        # # Use row.id to find corresponding rows in original data
        # subset.idx = Y.j$row.id
        # # Recalculate ALE for the split feature using subset data
        # res = calculate_ale(
        #   model = model,
        #   data = data[subset.idx, , drop = FALSE],
        #   feature.set = split.feature,
        #   target.feature.name = target.feature.name,
        #   n.intervals = n.intervals,
        #   predict.fun = predict.fun
        # )
        # Y.j = res[[split.feature]]
        # # Update row.id to match original data indices
        # Y.j$row.id = subset.idx
      }
      # Ensure it's a data.table
      if (!data.table::is.data.table(Y.j)) {
        Y.j = data.table::as.data.table(Y.j)
      }
      return(Y.j)
    })
    names(Y.processed) = names(Y.subset)
    return(Y.processed)
  } else {
    return(Y.subset)
  }
}
