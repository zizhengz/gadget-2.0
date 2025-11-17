#' Find the Best Split Point for ALE-based Tree Splitting
#'
#' This function finds the best split point for a single feature using ALE-specific
#' heterogeneity reduction as the objective function. It handles both categorical
#' and numerical variables with appropriate preprocessing.
#'
#' @param z Feature vector (numeric or categorical).
#' @param Y List of ALE effect data for each feature.
#' @param n.quantiles Integer or NULL. Number of quantiles for numerical splitting (optional).
#' @param is.categorical Logical. Whether the feature is categorical.
#' @param min.node.size Integer. Minimum number of observations per node.
#' @param model Fitted model object. Used for categorical recalculation.
#' @param data Data frame. Original data for categorical recalculation.
#' @param target.feature.name Character. Target feature name.
#' @param h Integer. Number of intervals for ALE calculation.
#' @param predict.fun Function or NULL. Prediction function.
#'
#' @note This function assumes inputs have been validated by the calling layer (e.g., aleStrategy).
#'
#' @return Data frame with split.point and split.objective.
#'
#' @details
#' This function implements ALE-specific splitting logic:
#' \itemize{
#'   \item For numerical features: Uses quantile-based candidate points
#'   \item For categorical features: Uses all possible category combinations
#'   \item Applies ALE-specific preprocessing (boundary robustification, categorical recalculation)
#'   \item Calculates heterogeneity reduction as objective function
#' }
#' @seealso \code{\link{search_best_split_ale}} for multi-feature splitting,
#'   \code{\link{process_ale_split_objective}} for ALE-specific preprocessing,
#'   \code{\link{calculate_ale_heterogeneity_cpp}} for heterogeneity calculation
search_best_split_point_ale = function(z, Y, n.quantiles = NULL, is.categorical = FALSE,
  min.node.size = 1, model = NULL,
  data = NULL, target.feature.name = NULL, h,
  predict.fun = NULL, split.feature) {
  # Create split point candidates
  if (is.categorical) {
    # Handle categorical variables
    z = droplevels(z)
    if (length(levels(z)) <= 1) {
      return(data.frame(
        split.point = NA,
        split.objective = Inf,
        objective.value.j = I(list(NA_real_)),
        left.objective.value.j = I(list(NA_real_)),
        right.objective.value.j = I(list(NA_real_))
      ))
    }
    splits = levels(z)[1:(length(levels(z)) - 1)]
  } else {
    # Handle numerical variables
    if (!is.null(n.quantiles)) {
      if (length(unique(z)) < n.quantiles) {
        splits = unique(z)
      } else {
        qprobs = seq(0, 1, length.out = n.quantiles + 2)[-c(1, n.quantiles + 2)]
        splits = unique(quantile(z, qprobs, type = 7))
      }
    } else {
      splits = unique(z)
      # splits = splits[splits < max(z)]
    }
  }
  # Early termination if no splits available
  if (length(splits) == 0) {
    return(data.frame(
      split.point = NA,
      split.objective = Inf,
      objective.value.j = I(list(NA_real_)),
      left.objective.value.j = I(list(NA_real_)),
      right.objective.value.j = I(list(NA_real_))
    ))
  }
  # Evaluate each split point with left/right transformation
  split_results = lapply(splits, function(split_point) {
    # Split data into left and right
    if (is.categorical) {
      left_idx = which(z == split_point)
      right_idx = which(z != split_point)
    } else {
      left_idx = which(z <= split_point)
      right_idx = which(z > split_point)
    }
    # Check minimum node size
    if (length(left_idx) < min.node.size || length(right_idx) < min.node.size) {
      return(list(total = Inf, per_feature = NA_real_))
    }
    # Transform Y for left child
    Y_left = node_transform_ale(
      Y = Y,
      idx = left_idx,
      split.feature = split.feature,
      node_type = "left",
      model = model,
      data = data,
      target.feature.name = target.feature.name,
      h = h,
      predict.fun = predict.fun
    )
    # Transform Y for right child
    Y_right = node_transform_ale(
      Y = Y,
      idx = right_idx,
      split.feature = split.feature,
      node_type = "right",
      model = model,
      data = data,
      target.feature.name = target.feature.name,
      h = h,
      predict.fun = predict.fun
    )
    # Calculate objective for left and right (per feature)
    obj_left_j = unlist(calculate_ale_heterogeneity_cpp(Y_left))
    obj_right_j = unlist(calculate_ale_heterogeneity_cpp(Y_right))
    names(obj_left_j) = names(Y)
    names(obj_right_j) = names(Y)
    per_feature = obj_left_j + obj_right_j
    names(per_feature) = names(Y)
    total = sum(per_feature)
    list(total = total, per_feature = per_feature, left = obj_left_j, right = obj_right_j)
  })
  # Find best split point
  totals = vapply(split_results, function(x) x$total, numeric(1))
  best_idx = which.min(totals)
  if (best_idx == 0 || is.infinite(totals[[best_idx]])) {
    return(data.frame(
      split.point = NA,
      split.objective = Inf,
      objective.value.j = I(list(NA_real_)),
      left.objective.value.j = I(list(NA_real_)),
      right.objective.value.j = I(list(NA_real_))
    ))
  }
  best.split.point = splits[best_idx]
  # Refine split point for numerical variables
  if (!is.factor(z)) {
    right_values = z[which(z > best.split.point)]
    left_values = z[which(z <= best.split.point)]
    if (length(right_values) > 0 && length(left_values) > 0) {
      right = min(right_values)
      left = max(left_values)
      best.split.point = (left + right) / 2
    }
  }
  return(data.frame(
    split.point = best.split.point,
    split.objective = totals[[best_idx]],
    objective.value.j = I(list(split_results[[best_idx]]$per_feature)),
    left.objective.value.j = I(list(split_results[[best_idx]]$left)),
    right.objective.value.j = I(list(split_results[[best_idx]]$right))
  ))
}

#' Map Split Points for Display
#'
#' Maps categorical split points from numeric codes to level names and ensures
#' consistent character column type for display purposes.
#'
#' @param split.points Vector of split points (numeric codes for categorical, numeric values for numeric).
#' @param split.features Vector of feature names corresponding to split points.
#' @param is.categorical Logical vector indicating which features are categorical.
#' @param Z Original data frame containing the features.
#'
#' @return Character vector of formatted split points.
#'
#' @note This function assumes inputs have been validated by the calling layer.
map_split_points_for_display = function(split.points, split.features, is.categorical, Z) {
  result = as.character(split.points) # Start with character conversion for all
  if (any(is.categorical)) {
    idx = which(is.categorical)
    result[idx] = mapply(function(f, sp) {
      if (is.na(sp)) {
        return(NA_character_)
      }
      # For categorical features, sp should already be a level name
      # Just return it as character
      as.character(sp)
    }, split.features[idx], split.points[idx], SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
  return(result)
}

#' Find the Best Split for ALE-based Tree Splitting
#'
#' This function finds the best split across all features using ALE-specific
#' heterogeneity reduction as the objective function.
#'
#' @param Z Data frame. Split feature set.
#' @param Y List of ALE effect data for each feature.
#' @param min.node.size Integer. Minimum number of observations per node.
#' @param n.quantiles Integer or NULL. Number of quantiles for numerical splitting (optional).
#' @param model Fitted model object. Used for categorical recalculation.
#' @param data Data frame. Original data for categorical recalculation.
#' @param target.feature.name Character. Target feature name.
#' @param h Integer. Number of intervals for ALE calculation.
#' @param predict.fun Function or NULL. Prediction function.
#'
#' @note This function assumes inputs have been validated by the calling layer (e.g., aleStrategy).
#'
#' @return Data frame with split results for all features.
#'
#' @details
#' This function evaluates all features and finds the best split point for each one.
#' It uses ALE-specific preprocessing and objective functions.
#' @seealso \code{\link{search_best_split_point_ale}} for single feature splitting,
#'   \code{\link{calculate_ale_heterogeneity_cpp}} for heterogeneity calculation
search_best_split_ale = function(Z, Y, min.node.size, n.quantiles = NULL,
  model = NULL, data = NULL, target.feature.name = NULL,
  h, predict.fun = NULL) {
  t1 = proc.time()
  res = data.table::rbindlist(
    lapply(names(Z), function(feat_name) {
      z = Z[[feat_name]]
      df = search_best_split_point_ale(
        z = z,
        Y = Y,
        n.quantiles = n.quantiles,
        is.categorical = is.factor(z),
        min.node.size = min.node.size,
        model = model,
        data = data,
        target.feature.name = target.feature.name,
        h = h,
        predict.fun = predict.fun,
        split.feature = feat_name
      )
      df
    })
  )
  t2 = proc.time()

  res$split.feature = names(Z)
  res$is.categorical = unname(sapply(Z, is.factor))
  res$best.split = res$split.objective == min(res$split.objective, na.rm = TRUE)
  res$split.runtime = (t2 - t1)[[3]]
  # Map split points for display (categorical codes to level names, numeric to character)
  res$split.point = map_split_points_for_display(
    split.points = res$split.point,
    split.features = res$split.feature,
    is.categorical = res$is.categorical,
    Z = Z
  )
  feature_names = names(Y)
  res = data.table::rbindlist(lapply(seq_len(nrow(res)), function(i) {
    v = res$objective.value.j[[i]]
    vl = if (!is.null(res$left.objective.value.j)) res$left.objective.value.j[[i]] else NULL
    vr = if (!is.null(res$right.objective.value.j)) res$right.objective.value.j[[i]] else NULL
    if (length(v) == 1 && is.na(v)) v = rep(NA_real_, length(feature_names))
    if (is.null(vl) || (length(vl) == 1 && is.na(vl))) vl = rep(NA_real_, length(feature_names))
    if (is.null(vr) || (length(vr) == 1 && is.na(vr))) vr = rep(NA_real_, length(feature_names))
    data.frame(
      split.feature = res$split.feature[i],
      is.categorical = res$is.categorical[i],
      split.point = res$split.point[i],
      split.objective = res$split.objective[i],
      feature = feature_names,
      objective.value.j = as.numeric(v),
      left.objective.value.j = as.numeric(vl),
      right.objective.value.j = as.numeric(vr),
      split.runtime = res$split.runtime[i],
      best.split = res$best.split[i],
      stringsAsFactors = FALSE
    )
  }))
  return(res[, c("split.feature", "is.categorical", "split.point",
    "split.objective", "feature", "objective.value.j",
    "left.objective.value.j", "right.objective.value.j",
    "split.runtime", "best.split")])
}
