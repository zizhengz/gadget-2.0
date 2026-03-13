#' Prepare ALE Data for Tree Splitting
#'
#' Given model, data, target_feature_name, n_intervals, and optional feature/split sets:
#' validates features; converts character to factor and orders levels via
#' \code{order_categorical_levels}; builds Z (data.table of split columns);
#' calls \code{calculate_ale} for Y. Returns list \code{Z}, \code{Y}.
#'
#' @param model Fitted model object with a predict interface.
#' @param data Data frame or data.table. Training data (features and target).
#' @param target_feature_name Character. Name of the target variable.
#' @param n_intervals Integer. Number of intervals for numeric ALE (default 10).
#' @param feature_set Character or NULL. Features to compute ALE for; NULL = all.
#' @param split_feature Character or NULL. Features to consider for splitting; NULL = all.
#' @param predict_fun Function or NULL. \code{function(model, data)} returning predictions;
#'   NULL uses mlr3-style default.
#' @param order_method Character. How to order categorical levels: \code{"mds"},
#'   \code{"pca"}, \code{"random"}, or \code{"raw"} (keep existing factor level order;
#'   default \code{"mds"}).
#'
#' @return List with:
#'   \item{Z}{data.table of split features (columns in \code{split_feature}).}
#'   \item{Y}{List of ALE effect data per feature (from \code{calculate_ale}).}
#'
#' @details
#' Steps performed:
#' \enumerate{
#'   \item Resolve and validate \code{feature_set} and \code{split_feature} against \code{colnames(data)}.
#'   \item For \code{union(feature_set, split_feature)}, convert character
#'     columns to factor and order levels via
#'     \code{order_categorical_levels} (using \code{droplevels} internally).
#'   \item Build \code{Z} as \code{data[split_feature]} (data.table).
#'   \item Call \code{calculate_ale(model, data, feature_set, ...)} to get \code{Y}.
#' }
#' Stops with an error if any requested feature is missing from \code{data}.
#'
#' @examples
#' \dontrun{
#' result = prepare_split_data_ale(model, data, "y", n_intervals = 10)
#' result = prepare_split_data_ale(model, data, "y", n_intervals = 10,
#'   feature_set = c("x1", "x2"), split_feature = c("x1", "x2", "x3"))
#' }
#'
prepare_split_data_ale = function(
  model,
  data,
  target_feature_name,
  n_intervals,
  feature_set = NULL,
  split_feature = NULL,
  predict_fun = NULL,
  order_method = "mds"
) {
  # Local helpers
  all_features = setdiff(colnames(data), target_feature_name)
  take_cols = function(d, cols) {
    if (data.table::is.data.table(d)) d[, cols, with = FALSE] else d[, cols, drop = FALSE]
  }
  resolve_features = function(requested, err_label) {
    if (is.null(requested)) {
      return(all_features)
    }
    miss = setdiff(requested, all_features)
    if (length(miss) > 0L) {
      stop(sprintf(
        "%s not found in data: %s. Available features: %s",
        err_label,
        paste(miss, collapse = ", "),
        paste(all_features, collapse = ", ")
      ))
    }
    requested
  }
  ensure_factors = function(d, cols) {
    for (c in cols) {
      if (is.character(d[[c]])) d[[c]] = factor(d[[c]])
    }
    d
  }

  feature_set = resolve_features(feature_set, "Features")
  split_feature = resolve_features(split_feature, "Split features")
  data = ensure_factors(data, union(feature_set, split_feature))
  for (col in union(feature_set, split_feature)) {
    if (is.factor(data[[col]])) {
      data[[col]] = order_categorical_levels(droplevels(data[[col]]), data, col, target_feature_name, order_method)
    }
  }
  Z = data.table::setDT(take_cols(data, split_feature))
  effect = calculate_ale(
    model = model,
    data = data,
    target_feature_name = target_feature_name,
    feature_set = feature_set,
    n_intervals = n_intervals,
    predict_fun = predict_fun
  )

  list(Z = Z, Y = effect)
}
