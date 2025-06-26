search_split_listY = function(z, Y_list, n.quantiles = NULL, is.categorical = FALSE, min.node.size) {
  checkmate::assert_vector(z)
  checkmate::assert_list(Y_list)
  N = nrow(Y_list[[1]])

  if (is.categorical) {
    if (length(levels(z)) <= 1) {
      return(data.frame(split.point = NA, objective.value = Inf))
    }
    splits = levels(z)[1:length(levels(z)) - 1]
    Y_list = lapply(Y_list, function(Y) as.matrix(Y))
    current.objective = sum(vapply(seq_along(Y_list), function(i) {
      sum(Rfast::colsums(Y_list[[i]]^2) - Rfast::colsums(Y_list[[i]])^2 / N)
    }, NA_real_))
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
    cumsum_list = lapply(Y_list, function(Y) {
      Y = as.matrix(Y[ord, ])
      list(cum_y = Rfast::colCumSums(Y), cum_y2 = Rfast::colCumSums(Y^2))
    })
    current.objective = sum(vapply(seq_along(cumsum_list), function(i) {
      sum(cumsum_list[[i]]$cum_y2[N, ] - (cumsum_list[[i]]$cum_y[N, ])^2 / N)
    }, NA_real_))
  }

  objective = vapply(splits, function(split) {
    idx = if (is.categorical) which(z %in% split) else which(z <= split)
    N_L = length(idx)
    N_R = N - N_L
    if (N_L < min.node.size || N_R < min.node.size) {
      return(Inf)
    }
    if (is.categorical) {
      sum(vapply(seq_along(Y_list), function(i) {
        Y = Y_list[[i]]
        Y_L = Y[idx, , drop = FALSE]
        Y_R = Y[-idx, , drop = FALSE]
        SS_L = Rfast::colsums(Y_L^2)
        SS_R = Rfast::colsums(Y_R^2)
        S_L = Rfast::colsums(Y_L)
        S_R = Rfast::colsums(Y_R)
        sum(SS_L - S_L^2 / N_L) + sum(SS_R - S_R^2 / N_R)
      }, NA_real_))
    } else {
      sum(vapply(seq_along(cumsum_list), function(i) {
        cum_y = cumsum_list[[i]]$cum_y
        cum_y2 = cumsum_list[[i]]$cum_y2
        S_L = cum_y[N_L, ]
        S_R = cum_y[N, ] - S_L
        SS_L = cum_y2[N_L, ]
        SS_R = cum_y2[N, ] - SS_L
        sum(SS_L - S_L^2 / N_L) + sum(SS_R - S_R^2 / N_R)
      }, NA_real_))
    }
  }, FUN.VALUE = NA_real_)

  best = which.min(objective)
  split.point = splits[[best]]
  if (!is.categorical) {
    right = min(z[which(z > split.point)])
    left = max(z[which(z <= split.point)])
    split.point = (left + right) / 2
  }

  return(data.frame(split.point = split.point, objective.value = objective[best], current.objective = current.objective))
}
