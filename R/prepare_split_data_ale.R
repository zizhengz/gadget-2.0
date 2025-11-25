#' Prepare ALE Data for Tree Splitting
#'
#' This function computes ALE effects and prepares the data structure needed for
#' ALE-based tree splitting. It includes robust feature validation and handles
#' both feature.set and split.feature parameters with proper error checking.
#'
#' @param model Fitted model object. The model should have a predict method.
#' @param data Data frame. Training data containing features and target variable.
#' @param target.feature.name Character. Name of the target variable in the data.
#' @param h Integer. Number of intervals for numeric features (default: 10).
#' @param feature.set Character vector or NULL. Features to compute ALE effects for.
#'   If NULL, computes for all features. Will validate that all specified features exist.
#' @param split.feature Character vector or NULL. Features to consider for splitting.
#'   If NULL, all features are considered for splitting. Will validate that all specified features exist.
#' @param predict.fun Function or NULL. Prediction function that takes (model, data) and returns predictions.
#'   If NULL, uses default mlr3-compatible prediction function.
#'
#' @return List with the following components:
#'   \item{Z}{Data frame of split features (numeric)}
#'   \item{Y}{List of ALE effect data for each feature}
#'   \item{grid}{List of feature value grids}
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that all specified features exist in the data
#' 2. Computes ALE effects using calculate_ale()
#' 3. Prepares split feature set Z with proper type conversion
#' 4. Extracts feature grids from ALE effect data
#'
#' The function includes robust error handling that will stop execution with
#' informative messages if any specified features are not found in the data.
#'
#' @examples
#' \dontrun{
#' # Basic usage with all features
#' result = prepare_split_data_ale(model, data, "target")
#'
#' # With specific feature sets
#' result = prepare_split_data_ale(model, data, "target",
#'                                feature.set = c("x1", "x2"),
#'                                split.feature = c("x1", "x2", "x3"))
#' }
#'
prepare_split_data_ale = function(model, data, target.feature.name, n.intervals,
                                  feature.set = NULL, split.feature = NULL, predict.fun = NULL) {
  # X: full feature set
  feature_names = setdiff(colnames(data), target.feature.name)
  if (data.table::is.data.table(data)) {
    X = data[, feature_names, with = FALSE]
  } else {
    X = data[, feature_names, drop = FALSE]
  }

  # feature.set: feature set to compute ALE for (default to all features if NULL)
  if (!is.null(feature.set)) {
    available.features = colnames(X)
    missing.features = setdiff(feature.set, available.features)
    if (length(missing.features) > 0) {
      stop(sprintf("Features not found in data: %s. Available features: %s",
        paste(missing.features, collapse = ", "),
        paste(available.features, collapse = ", ")))
    }
    feature.set = feature.set
  } else {
    feature.set = colnames(X)
  }

  # Convert character columns to factors in the selected feature set
  for (col in feature.set) {
    if (is.character(data[[col]])) {
      data[[col]] = factor(data[[col]])
    }
  }
  effect = calculate_ale(model = model, data = data, target.feature.name = target.feature.name,
                             feature.set = feature.set, n.intervals = n.intervals, predict.fun = predict.fun)

  # Z \subseteq X: split feature set
  if (!is.null(split.feature)) {
    available.features = colnames(X)
    missing.features = setdiff(split.feature, available.features)
    if (length(missing.features) > 0) {
      stop(sprintf("Split features not found in data: %s. Available features: %s",
        paste(missing.features, collapse = ", "),
        paste(available.features, collapse = ", ")))
    }
    if (data.table::is.data.table(X)) {
      Z = X[, split.feature, with = FALSE]
    } else {
      Z = X[, split.feature, drop = FALSE]
    }
  } else {
    Z = X
  }
  for (col in names(Z)) {
    if (is.character(Z[[col]])) {
      Z[[col]] = factor(Z[[col]])
    }
  }
  Z = data.table::setDT(Z)

  # No feature grids need
  grid = lapply(effect, function(feat) {
    feat$feat.val
  })

  return(list(Z = Z, Y = effect, grid = grid))
}
