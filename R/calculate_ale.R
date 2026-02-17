#' Calculate Accumulated Local Effects (ALE)
#'
#' Computes sample-level ALE data as local finite differences for each specified
#' feature. Output is used for tree splitting, heterogeneity measures, or visualization.
#'
#' @param model Fitted model object with a predict interface.
#' @param data Data frame or data.table. Training data (features and target).
#' @param feature.set Character. Names of features to compute ALE for; must exist in \code{data}.
#' @param target.feature.name Character. Name of the target variable in \code{data}.
#' @param n.intervals Integer. Number of equal-frequency intervals for numeric features (default: 10).
#' @param predict.fun Function or NULL. \code{function(model, data)} returning a numeric vector of predictions; NULL uses an mlr3-style default.
#'
#' @return Named list of data.tables, one per element of \code{feature.set}. Each data.table has columns:
#'   \item{row.id}{Row index in \code{data}.}
#'   \item{feat.val}{Feature value at that row.}
#'   \item{x.left, x.right}{Interval/category boundaries (numeric) or left/right category (factor).}
#'   \item{dL}{Local effect (finite difference).}
#'   \item{interval.index}{Interval or category index.}
#'   \item{int_n, int_s1, int_s2}{Per-interval count and sum(dL), sum(dL^2) for heterogeneity.}
#'
#' @details
#' Numeric features: builds \code{n.intervals} quantile-based intervals, assigns each row to an interval, and computes finite differences between interval boundaries via \code{predict.fun}.
#'
#' Categorical features: use factor levels as given (typically pre-ordered by \code{order_categorical_levels} in \code{prepare_split_data_ale}). For each row, \code{dL} is the difference in predictions when the focal feature is set to the next vs. previous level; single-level factors get \code{dL = 0}.
#'
#' Sample-level columns (\code{row.id}, \code{feat.val}, \code{dL}, etc.) support subsetting by node and downstream heterogeneity calculation.
#'
#' @export
calculate_ale = function(model, data, feature.set, target.feature.name, n.intervals = 10, predict.fun = NULL) {
  if (is.null(predict.fun)) {
    predict.fun = function(model, data) {
      pred = model$predict_newdata(data)
      if (inherits(pred, "Prediction")) pred$response else pred
    }
  }
  if (data.table::is.data.table(data)) {
    X = data[, setdiff(colnames(data), target.feature.name), with = FALSE]
  } else {
    X = data[, setdiff(colnames(data), target.feature.name), drop = FALSE]
  }
  eff.list = lapply(feature.set, function(feat) {
    if (is.factor(data[[feat]])) {
      ale_categorical_feature(model = model, data = data, X = X,
        feature = feat, target.feature.name = target.feature.name, predict.fun = predict.fun)
    } else {
      ale_numeric_feature(model = model, data = data, X = X,
        feature = feat, target.feature.name = target.feature.name, n.intervals = n.intervals, predict.fun = predict.fun)
    }
  })
  names(eff.list) = feature.set
  eff.list
}
ale_numeric_feature = function(model, data, X, feature, target.feature.name, n.intervals = 10, predict.fun = NULL) {
  x.num = data[[feature]]
  n.rows = nrow(data)
  # If constant or all NA: return minimal DT
  if (length(unique(na.omit(x.num))) <= 1L) {
    return(data.table::data.table(
      row.id         = seq_len(n.rows),
      feat.val       = x.num,
      x.left         = x.num,
      x.right        = x.num,
      dL             = 0,
      interval.index = 1L,
      int_n          = length(x.num),
      int_s1         = 0,
      int_s2         = 0
    ))
  }
  # Quantile-based interval boundaries
  q = stats::quantile(x.num, 0:n.intervals / n.intervals, type = 7, na.rm = TRUE)
  interval.index = findInterval(x.num, q, left.open = TRUE)
  interval.index[interval.index == 0L] = 1L
  max_id = length(q) - 1L
  interval.index[interval.index > max_id] = max_id
  # Build boundary datasets
  data.lower = data.table::copy(X)
  data.upper = data.table::copy(X)
  data.lower[[feature]] = q[interval.index]
  data.upper[[feature]] = q[interval.index + 1L]
  # Predictions at boundaries
  loss.lower = predict.fun(model, data.lower) # maybe predict together and split later
  loss.upper = predict.fun(model, data.upper)
  dL = (loss.upper - loss.lower)

  DT = data.table::data.table(
    row.id         = seq_len(n.rows),
    feat.val       = x.num,
    x.left         = q[interval.index],
    x.right        = q[interval.index + 1L],
    dL             = dL,
    interval.index = interval.index
  )

  # Compute per-interval statistics
  DT[, `:=`(
    int_n    = .N,
    int_s1   = sum(dL),
    int_s2   = sum(dL^2)
  ), by = interval.index]
  DT
}
ale_categorical_feature = function(model, data, X, feature, target.feature.name, predict.fun = NULL) {
  data.copy = data
  x.cat = droplevels(data.copy[[feature]])
  K = nlevels(x.cat)
  n.rows = nrow(data.copy)
  # If only one level, dL is zero
  if (K <= 1) {
    return(data.table::data.table(
      row.id         = seq_len(n.rows),
      feat.val       = x.cat,
      x.left         = x.cat,
      x.right        = x.cat,
      dL             = 0,
      interval.index = as.numeric(x.cat),
      int_n          = length(x.cat),
      int_s1         = 0,
      int_s2         = 0
    ))
  }
  levels.orig = levels(x.cat)
  levels.id = as.numeric(x.cat)
  # Indices for adjacent category replacement
  row.ind.plus = seq_len(nrow(data.copy))[levels.id < K] # not the highest level
  row.ind.neg = seq_len(nrow(data.copy))[levels.id > 1] # not the lowest level

  X.plus = data.table::copy(X)
  X.neg = data.table::copy(X)
  X.plus[row.ind.plus, feature] = levels.orig[levels.id[row.ind.plus] + 1L]
  X.neg[row.ind.neg, feature] = levels.orig[levels.id[row.ind.neg] - 1L]
  y.hat.plus = predict.fun(model, X.plus)
  y.hat.neg = predict.fun(model, X.neg)
  delta = y.hat.plus - y.hat.neg

  DT = data.table::data.table(
    row.id         = seq_len(n.rows),
    feat.val       = data.copy[[feature]],
    x.left         = X.neg[[feature]],
    x.right        = X.plus[[feature]],
    dL             = delta,
    interval.index = levels.id
  )

  # Full-interval statistics (each category treated as an interval)
  # Note: :=, .N, and column names in by clause are data.table special syntax
  DT[, `:=`( # nolint: object_usage_linter
    int_n    = .N, # nolint: object_usage_linter
    int_s1   = sum(dL),
    int_s2   = sum(dL^2)
  ), by = interval.index]
  DT
}

#### Old ale_categorical_feature ####
# ale_categorical_feature = function(model, data, X, feature, target.feature.name, predict.fun = NULL) {
#   data.copy = data
#   x.cat = droplevels(data.copy[[feature]])
#   K = nlevels(x.cat)
#   n.rows = nrow(data.copy)
#   # If only one level, dL is zero
#   if (K <= 1) {
#     return(data.table::data.table(
#       row.id         = seq_len(n.rows),
#       feat.val       = x.cat,
#       x.left         = x.cat,
#       x.right        = x.cat,
#       dL             = 0,
#       interval.index = as.numeric(x.cat),
#       int_n          = length(x.cat),
#       int_s1         = 0,
#       int_s2         = 0
#     ))
#   }
#   levels.orig = levels(x.cat)
#   levels.id = as.numeric(x.cat)
#   # Indices for adjacent category replacement
#   row.ind.plus = seq_len(nrow(data.copy))[levels.id < K] # not the highest level
#   row.ind.neg = seq_len(nrow(data.copy))[levels.id > 1] # not the lowest level
#
#   X.plus = data.table::copy(X)
#   X.neg = data.table::copy(X)
#   X.plus[row.ind.plus, feature] = levels.orig[levels.id[row.ind.plus] + 1L]
#   X.neg[row.ind.neg, feature] = levels.orig[levels.id[row.ind.neg] - 1L]
#   y.hat.plus = predict.fun(model, X.plus)
#   y.hat.neg = predict.fun(model, X.neg)
#   delta = y.hat.plus - y.hat.neg
#
#   DT = data.table::data.table(
#     row.id         = seq_len(n.rows),
#     feat.val       = data.copy[[feature]],
#     x.left         = X.neg[[feature]],
#     x.right        = X.plus[[feature]],
#     dL             = delta,
#     interval.index = levels.id
#   )
#
#   # Full-interval statistics (each category treated as an interval)
#   # Note: :=, .N, and column names in by clause are data.table special syntax
#   DT[, `:=`( # nolint: object_usage_linter
#     int_n    = .N, # nolint: object_usage_linter
#     int_s1   = sum(dL),
#     int_s2   = sum(dL^2)
#   ), by = interval.index]
#   DT
# }


#### Old calculate_ale ####
# calculate_ale = function(model, data, feature.set, target.feature.name, h, predict.fun = NULL) {
#   if (is.null(predict.fun)) {
#     predict.fun = function(model, data) {
#       pred = model$predict_newdata(data)
#       if (inherits(pred, "Prediction")) pred$response else pred
#     }
#   }
#   X = data[, setdiff(colnames(data), target.feature.name), drop = FALSE]
#   effect = lapply(feature.set, function(feature) {
#     if (class(data[, feature]) == "factor") {
#       # For categorical features, calculate ALE using adjacent category differences
#       data[, feature] = droplevels(data[, feature])
#       K = nlevels(data[, feature])
#       levs.orig = levels(data[, feature])
#       x.ord = as.numeric(data[, feature])
#
#       # Indices for boundary interpolation
#       row.ind.plus = (1:nrow(data))[x.ord < K] # not the highest level
#       row.ind.neg = (1:nrow(data))[x.ord > 1] # not the lowest level
#
#       X.plus = X
#       X.neg = X
#       X.plus[row.ind.plus, feature] = levs.orig[x.ord[row.ind.plus] + 1]
#       X.neg[row.ind.neg, feature] = levs.orig[x.ord[row.ind.neg] - 1]
#
#       y.hat.plus = predict.fun(model, X.plus)
#       y.hat.neg = predict.fun(model, X.neg)
#
#       # Calculate finite differences
#       Delta = y.hat.plus - y.hat.neg
#
#       DT = data.table::data.table(
#         "feat.val" = data[, feature],
#         "x.left" = X.neg[, feature],
#         "x.right" = X.plus[, feature],
#         "dL" = Delta,
#         "interval.index" = as.numeric(data[, feature]),
#         "interval.width" = 1
#       )
#     } else {
#       # For numeric features, use quantile-based intervals
#       # Create interval boundaries
#       q = quantile(data[[feature]], 0:h / h)
#
#       # Assign data points to intervals
#       interval.index = findInterval(data[[feature]], q, left.open = TRUE)
#       # Leftmost interval should be 1, not 0
#       interval.index[interval.index == 0] = 1
#
#       # Create data with interval boundaries (exclude target for prediction)
#       data.lower = data.upper = X
#       data.lower[[feature]] = q[interval.index]
#       data.upper[[feature]] = q[interval.index + 1]
#
#       # Get predictions at boundaries
#       loss.lower = predict.fun(model, data.lower)
#       loss.upper = predict.fun(model, data.upper)
#
#       # Calculate finite differences
#       dL = (loss.upper - loss.lower)
#       interval.width = q[interval.index + 1] - q[interval.index]
#
#       DT = data.table::data.table(
#         "feat.val" = data[, feature],
#         "x.left" = q[interval.index],
#         "x.right" = q[interval.index + 1],
#         "dL" = dL,
#         "interval.index" = interval.index,
#         "interval.width" = interval.width
#       )
#     }
#     DT
#   })
#   names(effect) = feature.set
#   return(effect)
# }
