search_best_split_point = function(z, Y, n.quantiles = NULL, is.categorical = FALSE, min.node.size) {
  checkmate::assert_vector(z)
  checkmate::assert_list(Y)
  N = nrow(Y[[1]])
  # create split points candidates
  if (is.categorical) {
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
      if (length(unique(z)) < 10) {
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
      list(Y.i.cumsum = Rfast::colCumSums(Y.i), Y.i2.cumsum = Rfast::colCumSums(Y.i^2))
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
        SS.l = Rfast::colsums(Y.i.l^2)
        SS.r = Rfast::colsums(Y.i.r^2)
        S.l = Rfast::colsums(Y.i.l)
        S.r = Rfast::colsums(Y.i.r)
        sum(SS.l - S.l^2 / N.l, na.rm = TRUE) + sum(SS.r - S.r^2 / N.r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    } else {
      sum(vapply(seq_along(Y.cumsum), function(i) {
        Y.i.cumsum = Y.cumsum[[i]]$Y.i.cumsum
        Y.i2.cumsum = Y.cumsum[[i]]$Y.i2.cumsum
        S.l = Y.i.cumsum[N.l, ]
        S.r = Y.i.cumsum[N, ] - S.l
        SS.l = Y.i2.cumsum[N.l, ]
        SS.r = Y.i2.cumsum[N, ] - SS.l
        sum(SS.l - S.l^2 / N.l, na.rm = TRUE) + sum(SS.r - S.r^2 / N.r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    }
  }, FUN.VALUE = NA_real_)

  best = which.min(split.objective)
  best.split.point = splits[best]
  if (!is.categorical) {
    right = min(z[which(z > best.split.point)])
    left = max(z[which(z <= best.split.point)])
    best.split.point = (left + right) / 2
  }

  return(data.frame(split.point = best.split.point, split.objective = split.objective[best]))
}
