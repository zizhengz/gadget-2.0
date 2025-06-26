search_split = function(z, Y, n.quantiles = NULL, is.categorical = FALSE, min.node.size) {
  checkmate::assert_vector(z)
  checkmate::assert_data_frame(Y)
  N = nrow(Y)

  if (is.categorical) {
    if (length(levels(z)) <= 1) {
      return(data.frame(split.point = NA, objective.value = Inf))
    }
    splits = levels(z)[1:length(levels(z)) - 1]

    Y = as.matrix(Y)
    sum_y = Rfast::colsums(Y)
    sum_y2 = Rfast::colsums(Y^2)
    current.objective = sum(sum_y2 - sum_y^2 / N)

    objective = vapply(splits, function(split) {
      idx = which(z %in% split)
      N_L = length(idx)
      N_R = N - N_L
      if (N_L < min.node.size || N_R < min.node.size) {
        return(Inf)
      }

      Y_L = Y[idx, , drop = FALSE]
      Y_R = Y[-idx, , drop = FALSE]
      SS_L = Rfast::colsums(Y_L^2)
      SS_R = Rfast::colsums(Y_R^2)
      S_L = Rfast::colsums(Y_L)
      S_R = Rfast::colsums(Y_R)
      sum(SS_L - S_L^2 / N_L) + sum(SS_R - S_R^2 / N_R)
    }, NA_real_)

  } else {
    ord = order(z)
    z = z[ord]
    Y = as.matrix(Y[ord, ])
    cum_sum_y = Rfast::colCumSums(Y)
    cum_sum_y2 = Rfast::colCumSums(Y^2)
    current.objective = sum(cum_sum_y2[N, ] - cum_sum_y[N, ]^2 / N)

    if (!is.null(n.quantiles)) {
      if (length(unique(z)) < 10) {
        splits = unique(z)
      } else {
        # qprobs = seq(0, 1, by = 1 / n.quantiles)
        # splits = unique(quantile(z, qprobs, type = 1))
        qprobs = seq(0, 1, length.out = n.quantiles + 2)[-c(1, n.quantiles + 2)]
        splits = unique(quantile(z, qprobs, type = 7))
      }
    } else {
      splits = unique(z)
    }
    objective = vapply(splits, function(split) {
      idx = which(z <= split)
      N_L = length(idx)
      N_R = N - N_L
      if (N_L < min.node.size || N_R < min.node.size) {
        return(Inf)
      }

      S_L = cum_sum_y[N_L, ]
      S_R = cum_sum_y[N, ] - S_L
      SS_L = cum_sum_y2[N_L, ]
      SS_R = cum_sum_y2[N, ] - SS_L
      sum(SS_L - S_L^2 / N_L) + sum(SS_R - S_R^2 / N_R)
    }, NA_real_)
  }
  best = which.min(objective)
  split.point = splits[best]
  if (!is.categorical) {
    right = min(z[which(z > split.point)])
    left = max(z[which(z <= split.point)])
    split.point = (left + right) / 2
  }
  data.frame(split.point = split.point, objective.value = objective[best], current.objective = current.objective)
}
