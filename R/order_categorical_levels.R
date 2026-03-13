#' Order Levels of a Categorical Feature
#'
#' Given x_cat (factor), data, feature, target_feature_name, 
#' order_method: builds KxK distance matrix from other features (ECDF diff for numeric, L1 for categorical); 
#' embeds in 1D via MDS/PCA/random/raw; reorders levels. 
#' Returns factor with reordered levels (or x_cat unchanged if K<=1 or no other features).
#'
#' @param x_cat Factor. Focal feature values; must be \code{droplevels()}-ed so
#'   unused levels are dropped.
#' @param data Data frame or data.table. Full dataset including the target column.
#' @param feature Character. Name of the focal categorical feature (column in \code{data}).
#' @param target_feature_name Character. Name of the target column; excluded from
#'   \dQuote{other features} when building the distance matrix.
#' @param order_method Character. How to turn the level-distance matrix into an order:
#'   \code{"mds"} (default, 1D classic MDS), \code{"pca"} (first PC),
#'   \code{"random"} (random permutation), or \code{"raw"} (keep existing level order).
#'
#' @return Factor with the same length and values as \code{x_cat}, with
#'   \code{levels} reordered and \code{ordered = TRUE}. If \code{nlevels(x_cat) <= 1}
#'   or there are no other features, returns \code{x_cat} unchanged.
#'
#' @details
#' For each pair of levels, a distance is computed from all other features
#' (all columns in \code{data} except \code{feature} and \code{target_feature_name}):
#' \itemize{
#'   \item Numeric: sum over features of max absolute difference of ECDFs (KS-style).
#'   \item Categorical: sum over features of L1 distance of conditional level distributions.
#' }
#' The \eqn{K \times K} distance matrix is embedded in 1D via \code{order_method};
#' that 1D order defines the new level order. Single-level factors or no other
#' features yield \code{x_cat} unchanged.
#'
order_categorical_levels = function(x_cat, data, feature, target_feature_name, order_method = "mds") {
  levels_orig = levels(x_cat)
  K = nlevels(x_cat)
  if (order_method == "raw") {
    return(factor(x_cat, levels = levels_orig, ordered = TRUE))
  }
  # Use all features except the focal feature and the target
  other_feats = setdiff(colnames(data), c(feature, target_feature_name))
  if (K <= 1L || length(other_feats) == 0L) {
    return(x_cat)
  }
  # Build all level pairs
  pairs = expand.grid(from = levels_orig, to = levels_orig, stringsAsFactors = FALSE)
  total_dists = numeric(nrow(pairs))

  for (feat in other_feats) {
    val = data[[feat]]
    if (all(is.na(val))) next
    if (is.numeric(val)) {
      # Numeric other feature: compare ECDFs via KS distance
      grid = stats::quantile(val, probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)
      ecdf_list = tapply(val, x_cat, function(v) {
        if (length(stats::na.omit(v)) == 0L) {
          return(numeric(100))
        }
        stats::ecdf(v)(grid)
      })
      ecdf_mat = do.call(cbind, ecdf_list)[, levels_orig, drop = FALSE]
      diffs = abs(ecdf_mat[, pairs$from] - ecdf_mat[, pairs$to])
      total_dists = total_dists + apply(diffs, 2, max)
    } else {
      # Categorical / discrete other feature: compare conditional distributions via L1 distance
      tbl = table(x_cat, val)
      prob_mat = tbl / ifelse(rowSums(tbl) == 0, 1, rowSums(tbl))
      diffs = abs(prob_mat[pairs$from, ] - prob_mat[pairs$to, ])
      total_dists = total_dists + rowSums(diffs)
    }
  }
  # Construct K x K distance matrix
  dist_mat = matrix(total_dists, nrow = K, dimnames = list(levels_orig, levels_orig))

  if (order_method == "random") {
    sorted_lev = sample(levels_orig, length(levels_orig), replace = FALSE)
  } else if (order_method == "mds") {
    mds_ord = stats::cmdscale(dist_mat, k = 1)
    scores = mds_ord[, 1]
    sorted_lev = rownames(dist_mat)[order(scores)]
  } else if (order_method == "pca") {
    pca_res = stats::prcomp(dist_mat, center = TRUE, scale. = FALSE)
    scores = pca_res$x[, 1]
    sorted_lev = rownames(dist_mat)[order(scores)]
  } else {
    # Fallback: keep original order if unknown method is passed
    return(x_cat)
  }

  factor(x_cat, levels = sorted_lev, ordered = TRUE)
}
