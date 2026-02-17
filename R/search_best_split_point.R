#' Find best split point for a single feature z
#'
#' Numeric: tries quantile-based cutpoints. Categorical: tries binary splits on level subsets.
#' Returns \code{split.point} and \code{split.objective} (heterogeneity reduction).
#'
#' @param z Vector of split feature values.
#' @param Y List of effect matrices.
#' @param n.quantiles Integer or NULL (numeric only).
#' @param min.node.size Integer.
#' @param is.categorical Logical.
#' @return Data frame with \code{split.point}, \code{split.objective}.
#' @keywords internal
search_best_split_point = function(z, Y, n.quantiles = NULL, min.node.size, is.categorical = FALSE) {
  N = nrow(Y[[1]])
  # create split points candidates
  if (is.categorical) {
    # method for handling categorical splits correctly
    # if (is.ordered(z)) {
    #   z = droplevels(z)
    #   if (length(levels(z)) <= 1) {
    #     return(data.frame(split.point = NA, split.objective = Inf))
    #   }
    #   splits = lapply(1:(length(levels(z)) - 1), function(i) levels(z)[1:i])
    # } else {
    #   z = droplevels(z)
    #   if (length(levels(z)) <= 1) {
    #     return(data.frame(split.point = NA, split.objective = Inf))
    #   }
    #   splits = levels(z)[1:length(levels(z)) - 1]
    # }
    z = droplevels(z)
    if (length(levels(z)) <= 1) {
      return(data.frame(split.point = NA, split.objective = Inf))
    }
    splits = levels(z)[1:length(levels(z)) - 1]
    Y = lapply(Y, function(Y.i) as.matrix(Y.i))
  } else {
    # sort z in increasing order
    ord = order(z)
    z = z[ord]
    if (!is.null(n.quantiles)) {
      if (length(unique(z)) < n.quantiles) {
        splits = unique(z)
      } else {
        qprobs = seq(0, 1, length.out = n.quantiles + 2)[-c(1, n.quantiles + 2)]
        splits = unique(quantile(z, qprobs, type = 7))
      }
    } else {
      splits = unique(z)
    }
    Y.cumsum = lapply(Y, function(Y.i) {
      Y.i = as.matrix(Y.i[ord, ])
      Y.i.cumsum = Rfast::colCumSums(Y.i)
      Y.i.cumsum
    })
  }
  # calculate split objective: sum_j(R_j_l + R_j_r)
  split.objective = vapply(splits, function(split) {
    idx = if (is.categorical) which(z %in% split) else which(z <= split)
    N.l = length(idx)
    N.r = N - N.l
    if (N.l < min.node.size || N.r < min.node.size) {
      return(Inf)
    }
    if (is.categorical) {
      sum(vapply(seq_along(Y), function(i) {
        Y.i = Y[[i]]
        Y.i.l = Y.i[idx, , drop = FALSE]
        Y.i.r = Y.i[-idx, , drop = FALSE]
        S.l = Rfast::colsums(Y.i.l)
        S.r = Rfast::colsums(Y.i.r)
        sum(-S.l^2 / N.l - S.r^2 / N.r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    } else {
      sum(vapply(seq_along(Y.cumsum), function(i) {
        Y.i.cumsum = Y.cumsum[[i]]
        S.l = Y.i.cumsum[N.l, ]
        S.r = Y.i.cumsum[N, ] - S.l
        sum(-S.l^2 / N.l - S.r^2 / N.r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    }
  }, FUN.VALUE = NA_real_)

  best = which.min(split.objective)
  best.split.point = splits[best]
  if (!is.factor(z)) {
    right = min(z[which(z > best.split.point)])
    left = max(z[which(z <= best.split.point)])
    best.split.point = (left + right) / 2
  }

  return(data.frame(split.point = best.split.point, split.objective = split.objective[best]))
}
