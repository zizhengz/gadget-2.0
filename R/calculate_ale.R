#' Calculate Accumulated Local Effects (ALE)
#'
#' Given model, data, feature_set, target_feature_name, n_intervals, predict_fun: for each feature,
#' computes finite differences (d_l) and per-interval stats (int_n, int_s1, int_s2).
#' Numeric: quantile intervals; categorical: level-by-level prediction differences.
#' Returns named list of data.tables (row_id, feat_val, d_l, interval_index, int_n, int_s1, int_s2, etc.).
#'
#' @param model (`any`) \cr
#'   Fitted model with predict interface.
#' @param data (`data.frame()` or `data.table()`) \cr
#'   Training data.
#' @param feature_set (`character()`) \cr
#'   Features to compute ALE for.
#' @param target_feature_name (`character(1)`) \cr
#'   Target variable name.
#' @param n_intervals (`integer(1)`) \cr
#'   Equal-frequency intervals for numeric features.
#' @param predict_fun (`function()` or `NULL`) \cr
#'   \code{function(model, data)} returning predictions; \code{NULL} = default.
#'
#' @return (`list()`) \cr
#'   Named list of data.tables per \code{feature_set}. Each has columns:
#'   \item{row_id}{Row index in \code{data}.}
#'   \item{feat_val}{Feature value at that row.}
#'   \item{x_left, x_right}{Interval/category boundaries (numeric) or left/right category (factor).}
#'   \item{d_l}{Local effect (finite difference).}
#'   \item{interval_index}{Interval or category index.}
#'   \item{int_n, int_s1, int_s2}{Per-interval count and sum(d_l), sum(d_l^2) for heterogeneity.}
#'
#' @details
#' Numeric features: builds \code{n_intervals} quantile-based intervals,
#' assigns each row to an interval, and computes finite differences between
#' interval boundaries via \code{predict_fun}.
#'
#' Categorical features: use factor levels as given (typically pre-ordered by
#' \code{order_categorical_levels} in \code{prepare_split_data_ale}). For each
#' row, \code{d_l} is the difference in predictions when the focal feature is
#' set to the next vs. previous level; single-level factors get \code{d_l = 0}.
#'
#' Sample-level columns (\code{row_id}, \code{feat_val}, \code{d_l}, etc.)
#' support subsetting by node and downstream heterogeneity calculation.
#'
#' @keywords internal
calculate_ale = function(model, data, feature_set, target_feature_name, n_intervals = 10, predict_fun = NULL) {
  checkmate::assert_data_frame(data, .var.name = "data")
  checkmate::assert_character(feature_set, min.len = 1, .var.name = "feature_set")
  checkmate::assert_character(target_feature_name, len = 1, .var.name = "target_feature_name")
  checkmate::assert_subset(target_feature_name, colnames(data), .var.name = "target_feature_name")
  checkmate::assert_subset(feature_set, colnames(data), .var.name = "feature_set")
  checkmate::assert_integerish(n_intervals, len = 1, lower = 1, any.missing = FALSE, .var.name = "n_intervals")
  checkmate::assert_function(predict_fun, null.ok = TRUE, .var.name = "predict_fun")
  if (is.null(predict_fun)) predict_fun = default_predict_fun
  if (data.table::is.data.table(data)) {
    X = data[, setdiff(colnames(data), target_feature_name), with = FALSE]
  } else {
    X = data[, setdiff(colnames(data), target_feature_name), drop = FALSE]
  }
  eff_list = lapply(feature_set, function(feat) {
    if (is.factor(data[[feat]])) {
      ale_categorical_feature(model = model, data = data, X = X,
        feature = feat, target_feature_name = target_feature_name, predict_fun = predict_fun)
    } else {
      ale_numeric_feature(model = model, data = data, X = X,
        feature = feat, target_feature_name = target_feature_name, n_intervals = n_intervals, predict_fun = predict_fun)
    }
  })
  names(eff_list) = feature_set
  eff_list
}

#' ALE for a single numeric feature.
#'
#' @param model (`any`) \cr
#'   Fitted model. See \code{\link{calculate_ale}}.
#' @param data (`data.frame()` or `data.table()`) \cr
#'   Training data.
#' @param X (`data.frame()` or `data.table()`) \cr
#'   Features (excl. target).
#' @param feature (`character(1)`) \cr
#'   Feature name.
#' @param target_feature_name (`character(1)`) \cr
#'   Target name.
#' @param n_intervals (`integer(1)`) \cr
#'   Number of intervals.
#' @param predict_fun (`function()` or `NULL`) \cr
#'   Prediction function.
#'
#' @return (`data.table()`) \cr
#'   ALE data with \code{row_id}, \code{feat_val}, \code{d_l}, \code{interval_index}, etc.
#' @keywords internal
ale_numeric_feature = function(model, data, X, feature, target_feature_name, n_intervals = 10, predict_fun = NULL) {
  x_num = data[[feature]]
  n_rows = nrow(data)
  # If constant or all NA: return minimal DT
  if (length(unique(na.omit(x_num))) <= 1L) {
    return(data.table::data.table(
      row_id         = seq_len(n_rows),
      feat_val       = x_num,
      x_left         = x_num,
      x_right        = x_num,
      d_l             = 0,
      interval_index = 1L,
      int_n          = length(x_num),
      int_s1         = 0,
      int_s2         = 0
    ))
  }
  # Quantile-based interval boundaries
  q = stats::quantile(x_num, 0:n_intervals / n_intervals, type = 7, na.rm = TRUE)
  interval_index = findInterval(x_num, q, left.open = TRUE)
  interval_index[interval_index == 0L] = 1L
  max_id = length(q) - 1L
  interval_index[interval_index > max_id] = max_id
  # Build boundary datasets
  data_lower = data.table::copy(X)
  data_upper = data.table::copy(X)
  data_lower[[feature]] = q[interval_index]
  data_upper[[feature]] = q[interval_index + 1L]
  # Predictions at boundaries
  loss_lower = predict_fun(model, data_lower) # maybe predict together and split later
  loss_upper = predict_fun(model, data_upper)
  d_l = (loss_upper - loss_lower)

  DT = data.table::data.table(
    row_id         = seq_len(n_rows),
    feat_val       = x_num,
    x_left         = q[interval_index],
    x_right        = q[interval_index + 1L],
    d_l             = d_l,
    interval_index = interval_index
  )

  # Compute per-interval statistics
  DT[, `:=`(
    int_n    = .N,
    int_s1   = sum(d_l),
    int_s2   = sum(d_l^2)
  ), by = interval_index]
  DT
}

#' ALE for a single categorical feature.
#'
#' @param model (`any`) \cr
#'   Fitted model. See \code{\link{calculate_ale}}.
#' @param data (`data.frame()` or `data.table()`) \cr
#'   Training data.
#' @param X (`data.frame()` or `data.table()`) \cr
#'   Features (excl. target).
#' @param feature (`character(1)`) \cr
#'   Feature name.
#' @param target_feature_name (`character(1)`) \cr
#'   Target name.
#' @param predict_fun (`function()` or `NULL`) \cr
#'   Prediction function.
#'
#' @return (`data.table()`) \cr
#'   ALE data with \code{row_id}, \code{feat_val}, \code{d_l}, \code{interval_index}, etc.
#' @keywords internal
ale_categorical_feature = function(model, data, X, feature, target_feature_name, predict_fun = NULL) {
  data_copy = data
  x_cat = droplevels(data_copy[[feature]])
  K = nlevels(x_cat)
  n_rows = nrow(data_copy)
  # If only one level, d_l is zero
  if (K <= 1) {
    return(data.table::data.table(
      row_id         = seq_len(n_rows),
      feat_val       = x_cat,
      x_left         = x_cat,
      x_right        = x_cat,
      d_l             = 0,
      interval_index = as.numeric(x_cat),
      int_n          = length(x_cat),
      int_s1         = 0,
      int_s2         = 0
    ))
  }
  levels_orig = levels(x_cat)
  levels_id = as.numeric(x_cat)
  # Indices for adjacent category replacement
  row_ind_plus = seq_len(nrow(data_copy))[levels_id < K] # not the highest level
  row_ind_neg = seq_len(nrow(data_copy))[levels_id > 1] # not the lowest level

  x_plus = data.table::copy(X)
  x_neg = data.table::copy(X)
  x_plus[row_ind_plus, feature] = levels_orig[levels_id[row_ind_plus] + 1L]
  x_neg[row_ind_neg, feature] = levels_orig[levels_id[row_ind_neg] - 1L]
  y_hat_plus = predict_fun(model, x_plus)
  y_hat_neg = predict_fun(model, x_neg)
  delta = y_hat_plus - y_hat_neg

  DT = data.table::data.table(
    row_id         = seq_len(n_rows),
    feat_val       = data_copy[[feature]],
    x_left         = x_neg[[feature]],
    x_right        = x_plus[[feature]],
    d_l             = delta,
    interval_index = levels_id
  )

  # Full-interval statistics (each category treated as an interval)
  # Note: :=, .N, and column names in by clause are data.table special syntax
  DT[, `:=`( # nolint: object_usage_linter
    int_n    = .N, # nolint: object_usage_linter
    int_s1   = sum(d_l),
    int_s2   = sum(d_l^2)
  ), by = interval_index]
  DT
}

#### Old ale_categorical_feature ####
# ale_categorical_feature = function(model, data, X, feature, target_feature_name, predict_fun = NULL) {
#   data_copy = data
#   x_cat = droplevels(data_copy[[feature]])
#   K = nlevels(x_cat)
#   n_rows = nrow(data_copy)
#   # If only one level, d_l is zero
#   if (K <= 1) {
#     return(data.table::data.table(
#       row_id         = seq_len(n_rows),
#       feat_val       = x_cat,
#       x_left         = x_cat,
#       x_right        = x_cat,
#       d_l             = 0,
#       interval_index = as.numeric(x_cat),
#       int_n          = length(x_cat),
#       int_s1         = 0,
#       int_s2         = 0
#     ))
#   }
#   levels_orig = levels(x_cat)
#   levels_id = as.numeric(x_cat)
#   # Indices for adjacent category replacement
#   row_ind_plus = seq_len(nrow(data_copy))[levels_id < K] # not the highest level
#   row_ind_neg = seq_len(nrow(data_copy))[levels_id > 1] # not the lowest level
#
#   x_plus = data.table::copy(X)
#   x_neg = data.table::copy(X)
#   x_plus[row_ind_plus, feature] = levels_orig[levels_id[row_ind_plus] + 1L]
#   x_neg[row_ind_neg, feature] = levels_orig[levels_id[row_ind_neg] - 1L]
#   y_hat_plus = predict_fun(model, x_plus)
#   y_hat_neg = predict_fun(model, x_neg)
#   delta = y_hat_plus - y_hat_neg
#
#   DT = data.table::data.table(
#     row_id         = seq_len(n_rows),
#     feat_val       = data_copy[[feature]],
#     x_left         = x_neg[[feature]],
#     x_right        = x_plus[[feature]],
#     d_l             = delta,
#     interval_index = levels_id
#   )
#
#   # Full-interval statistics (each category treated as an interval)
#   # Note: :=, .N, and column names in by clause are data.table special syntax
#   DT[, `:=`( # nolint: object_usage_linter
#     int_n    = .N, # nolint: object_usage_linter
#     int_s1   = sum(d_l),
#     int_s2   = sum(d_l^2)
#   ), by = interval_index]
#   DT
# }


#### Old calculate_ale ####
# calculate_ale = function(model, data, feature_set, target_feature_name, h, predict_fun = NULL) {
#   if (is.null(predict_fun)) {
#     predict_fun = function(model, data) {
#       pred = model$predict_newdata(data)
#       if (inherits(pred, "Prediction")) pred$response else pred
#     }
#   }
#   X = data[, setdiff(colnames(data), target_feature_name), drop = FALSE]
#   effect = lapply(feature_set, function(feature) {
#     if (class(data[, feature]) == "factor") {
#       # For categorical features, calculate ALE using adjacent category differences
#       data[, feature] = droplevels(data[, feature])
#       K = nlevels(data[, feature])
#       levs_orig = levels(data[, feature])
#       x_ord = as.numeric(data[, feature])
#
#       # Indices for boundary interpolation
#       row_ind_plus = (1:nrow(data))[x_ord < K] # not the highest level
#       row_ind_neg = (1:nrow(data))[x_ord > 1] # not the lowest level
#
#       x_plus = X
#       x_neg = X
#       x_plus[row_ind_plus, feature] = levs_orig[x_ord[row_ind_plus] + 1]
#       x_neg[row_ind_neg, feature] = levs_orig[x_ord[row_ind_neg] - 1]
#
#       y_hat_plus = predict_fun(model, x_plus)
#       y_hat_neg = predict_fun(model, x_neg)
#
#       # Calculate finite differences
#       Delta = y_hat_plus - y_hat_neg
#
#       DT = data.table::data.table(
#         "feat_val" = data[, feature],
#         "x_left" = x_neg[, feature],
#         "x_right" = x_plus[, feature],
#         "d_l" = Delta,
#         "interval_index" = as.numeric(data[, feature]),
#         "interval_width" = 1
#       )
#     } else {
#       # For numeric features, use quantile-based intervals
#       # Create interval boundaries
#       q = quantile(data[[feature]], 0:h / h)
#
#       # Assign data points to intervals
#       interval_index = findInterval(data[[feature]], q, left.open = TRUE)
#       # Leftmost interval should be 1, not 0
#       interval_index[interval_index == 0] = 1
#
#       # Create data with interval boundaries (exclude target for prediction)
#       data_lower = data_upper = X
#       data_lower[[feature]] = q[interval_index]
#       data_upper[[feature]] = q[interval_index + 1]
#
#       # Get predictions at boundaries
#       loss_lower = predict_fun(model, data_lower)
#       loss_upper = predict_fun(model, data_upper)
#
#       # Calculate finite differences
#       d_l = (loss_upper - loss_lower)
#       interval_width = q[interval_index + 1] - q[interval_index]
#
#       DT = data.table::data.table(
#         "feat_val" = data[, feature],
#         "x_left" = q[interval_index],
#         "x_right" = q[interval_index + 1],
#         "d_l" = d_l,
#         "interval_index" = interval_index,
#         "interval_width" = interval_width
#       )
#     }
#     DT
#   })
#   names(effect) = feature_set
#   return(effect)
# }
