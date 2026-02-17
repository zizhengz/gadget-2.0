#' Order Levels of a Categorical Feature
#'
#' Computes a data-driven ordering of factor levels from other features, then
#' returns the factor with levels reordered. Intended for use in effect/split
#' strategies (e.g. ALE, PD) so categories are ordered by similarity rather than
#' alphabetically or by appearance.
#'
#' @param x.cat Factor. Focal feature values; must be \code{droplevels()}-ed so
#'   unused levels are dropped.
#' @param data Data frame or data.table. Full dataset including the target column.
#' @param feature Character. Name of the focal categorical feature (column in \code{data}).
#' @param target.feature.name Character. Name of the target column; excluded from
#'   \dQuote{other features} when building the distance matrix.
#' @param order.method Character. How to turn the level-distance matrix into an order:
#'   \code{"mds"} (default, 1D classic MDS), \code{"pca"} (first PC),
#'   \code{"random"} (random permutation), or \code{"raw"} (keep existing level order).
#'
#' @return Factor with the same length and values as \code{x.cat}, with
#'   \code{levels} reordered and \code{ordered = TRUE}. If \code{nlevels(x.cat) <= 1}
#'   or there are no other features, returns \code{x.cat} unchanged.
#'
#' @details
#' For each pair of levels, a distance is computed from all other features
#' (all columns in \code{data} except \code{feature} and \code{target.feature.name}):
#' \itemize{
#'   \item Numeric: sum over features of max absolute difference of ECDFs (KS-style).
#'   \item Categorical: sum over features of L1 distance of conditional level distributions.
#' }
#' The \eqn{K \times K} distance matrix is embedded in 1D via \code{order.method};
#' that 1D order defines the new level order. Single-level factors or no other
#' features yield \code{x.cat} unchanged.
#'
order_categorical_levels = function(x.cat, data, feature, target.feature.name, order.method = "mds") {
  levels.orig = levels(x.cat)
  K = nlevels(x.cat)
  if (order.method == "raw") {
    return(factor(x.cat, levels = levels.orig, ordered = TRUE))
  }
  # Use all features except the focal feature and the target
  other.feats = setdiff(colnames(data), c(feature, target.feature.name))
  if (K <= 1L || length(other.feats) == 0L) {
    return(x.cat)
  }
  # Build all level pairs
  pairs = expand.grid(from = levels.orig, to = levels.orig, stringsAsFactors = FALSE)
  total.dists = numeric(nrow(pairs))

  for (feat in other.feats) {
    val = data[[feat]]
    if (all(is.na(val))) next
    if (is.numeric(val)) {
      # Numeric other feature: compare ECDFs via KS distance
      grid = stats::quantile(val, probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)
      ecdf.list = tapply(val, x.cat, function(v) {
        if (length(stats::na.omit(v)) == 0L) {
          return(numeric(100))
        }
        stats::ecdf(v)(grid)
      })
      ecdf.mat = do.call(cbind, ecdf.list)[, levels.orig, drop = FALSE]
      diffs = abs(ecdf.mat[, pairs$from] - ecdf.mat[, pairs$to])
      total.dists = total.dists + apply(diffs, 2, max)
    } else {
      # Categorical / discrete other feature: compare conditional distributions via L1 distance
      tbl = table(x.cat, val)
      prob.mat = tbl / ifelse(rowSums(tbl) == 0, 1, rowSums(tbl))
      diffs = abs(prob.mat[pairs$from, ] - prob.mat[pairs$to, ])
      total.dists = total.dists + rowSums(diffs)
    }
  }
  # Construct K x K distance matrix
  dist.mat = matrix(total.dists, nrow = K, dimnames = list(levels.orig, levels.orig))

  if (order.method == "random") {
    sorted.lev = sample(levels.orig, length(levels.orig), replace = FALSE)
  } else if (order.method == "mds") {
    mds.ord = stats::cmdscale(dist.mat, k = 1)
    scores = mds.ord[, 1]
    sorted.lev = rownames(dist.mat)[order(scores)]
  } else if (order.method == "pca") {
    pca.res = stats::prcomp(dist.mat, center = TRUE, scale. = FALSE)
    scores = pca.res$x[, 1]
    sorted.lev = rownames(dist.mat)[order(scores)]
  } else {
    # Fallback: keep original order if unknown method is passed
    return(x.cat)
  }

  factor(x.cat, levels = sorted.lev, ordered = TRUE)
}
