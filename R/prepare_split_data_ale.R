#' Prepare ALE Data for Tree Splitting
#'
#' Validates features, converts character columns to ordered factors, builds the
#' split-feature matrix Z, and computes ALE effects for tree splitting.
#'
#' @param model Fitted model object with a predict interface.
#' @param data Data frame or data.table. Training data (features and target).
#' @param target.feature.name Character. Name of the target variable.
#' @param n.intervals Integer. Number of intervals for numeric ALE (default 10).
#' @param feature.set Character or NULL. Features to compute ALE for; NULL = all.
#' @param split.feature Character or NULL. Features to consider for splitting; NULL = all.
#' @param predict.fun Function or NULL. \code{function(model, data)} returning predictions; NULL uses mlr3-style default.
#' @param order.method Character. How to order categorical levels: \code{"mds"},
#'   \code{"pca"}, \code{"random"}, or \code{"raw"} (keep existing factor level order;
#'   default \code{"mds"}).
#'
#' @return List with:
#'   \item{Z}{data.table of split features (columns in \code{split.feature}).}
#'   \item{Y}{List of ALE effect data per feature (from \code{calculate_ale}).}
#'
#' @details
#' Steps performed:
#' \enumerate{
#'   \item Resolve and validate \code{feature.set} and \code{split.feature} against \code{colnames(data)}.
#'   \item For \code{union(feature.set, split.feature)}, convert character columns to factor and order levels via \code{order_categorical_levels} (using \code{droplevels} internally).
#'   \item Build \code{Z} as \code{data[split.feature]} (data.table).
#'   \item Call \code{calculate_ale(model, data, feature.set, ...)} to get \code{Y}.
#' }
#' Stops with an error if any requested feature is missing from \code{data}.
#'
#' @examples
#' \dontrun{
#' result = prepare_split_data_ale(model, data, "y", n.intervals = 10)
#' result = prepare_split_data_ale(model, data, "y", n.intervals = 10,
#'   feature.set = c("x1", "x2"), split.feature = c("x1", "x2", "x3"))
#' }
#'
prepare_split_data_ale = function(model, data, target.feature.name, n.intervals,
  feature.set = NULL, split.feature = NULL, predict.fun = NULL, order.method = "mds") {
  # Local helpers
  all.features = setdiff(colnames(data), target.feature.name)
  take_cols = function(d, cols) {
    if (data.table::is.data.table(d)) d[, cols, with = FALSE] else d[, cols, drop = FALSE]
  }
  resolve_features = function(requested, err_label) {
    if (is.null(requested)) {
      return(all.features)
    }
    miss = setdiff(requested, all.features)
    if (length(miss) > 0L) {
      stop(sprintf("%s not found in data: %s. Available features: %s",
        err_label, paste(miss, collapse = ", "), paste(all.features, collapse = ", ")))
    }
    requested
  }
  ensure_factors = function(d, cols) {
    for (c in cols) if (is.character(d[[c]])) d[[c]] = factor(d[[c]])
    d
  }

  feature.set = resolve_features(feature.set, "Features")
  split.feature = resolve_features(split.feature, "Split features")
  data = ensure_factors(data, union(feature.set, split.feature))
  for (col in union(feature.set, split.feature)) {
    if (is.factor(data[[col]])) {
      data[[col]] = order_categorical_levels(droplevels(data[[col]]), data, col, target.feature.name, order.method)
    }
  }
  Z = data.table::setDT(take_cols(data, split.feature))
  effect = calculate_ale(model = model, data = data, target.feature.name = target.feature.name,
    feature.set = feature.set, n.intervals = n.intervals, predict.fun = predict.fun)

  list(Z = Z, Y = effect)
}
