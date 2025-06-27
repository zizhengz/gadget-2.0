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
    Y = lapply(Y, function(Y_i) as.matrix(Y_i))
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
    Y_cumsum = lapply(Y, function(Y_i) {
      Y_i = as.matrix(Y_i[ord, ])
      list(Y_i_cumsum = Rfast::colCumSums(Y_i), Y_i2_cumsum = Rfast::colCumSums(Y_i^2))
    })
  }
  # calculate split objective: sum_j(R_j_l + R_j_r)
  split.objective = vapply(splits, function(split) {
    idx = if (is.categorical) which(z %in% split) else which(z <= split)
    N_L = length(idx)
    N_R = N - N_L
    if (N_L < min.node.size || N_R < min.node.size) {
      return(Inf)
    }
    if (is.categorical) {
      sum(vapply(seq_along(Y), function(i) {
        Y_i = Y[[i]]
        Y_i_L = Y_i[idx, , drop = FALSE]
        Y_i_R = Y_i[-idx, , drop = FALSE]
        SS_L = Rfast::colsums(Y_i_L^2)
        SS_R = Rfast::colsums(Y_i_R^2)
        S_L = Rfast::colsums(Y_i_L)
        S_R = Rfast::colsums(Y_i_R)
        sum(SS_L - S_L^2 / N_L, na.rm = TRUE) + sum(SS_R - S_R^2 / N_R, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    } else {
      sum(vapply(seq_along(Y_cumsum), function(i) {
        Y_i_cumsum = Y_cumsum[[i]]$Y_i_cumsum
        Y_i2_cumsum = Y_cumsum[[i]]$Y_i2_cumsum
        S_L = Y_i_cumsum[N_L, ]
        S_R = Y_i_cumsum[N, ] - S_L
        SS_L = Y_i2_cumsum[N_L, ]
        SS_R = Y_i2_cumsum[N, ] - SS_L
        sum(SS_L - S_L^2 / N_L, na.rm = TRUE) + sum(SS_R - S_R^2 / N_R, na.rm = TRUE)
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
