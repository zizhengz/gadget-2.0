#' Find best split point for a single feature z
#'
#' Given z, Y, n_quantiles, min_node_size, is_categorical: for numeric, sorts z, builds quantile cutpoints, evaluates objective (sum of -S_l^2/N_l - S_r^2/N_r) per cutpoint; for categorical, evaluates one-level vs rest. Returns data frame with \code{split_point}, \code{split_objective}.
#'
#' @param z Vector of split feature values.
#' @param Y List of effect matrices.
#' @param n_quantiles Integer or NULL (numeric only).
#' @param min_node_size Integer.
#' @param is_categorical Logical.
#' @return Data frame with \code{split_point}, \code{split_objective}.
#' @keywords internal
search_best_split_point = function(z, Y, n_quantiles = NULL, min_node_size, is_categorical = FALSE) {
  N = nrow(Y[[1]])
  # create split points candidates
  if (is_categorical) {
    # method for handling categorical splits correctly
    # if (is.ordered(z)) {
    #   z = droplevels(z)
    #   if (length(levels(z)) <= 1) {
    #     return(data.frame(split_point = NA, split_objective = Inf))
    #   }
    #   splits = lapply(1:(length(levels(z)) - 1), function(i) levels(z)[1:i])
    # } else {
    #   z = droplevels(z)
    #   if (length(levels(z)) <= 1) {
    #     return(data.frame(split_point = NA, split_objective = Inf))
    #   }
    #   splits = levels(z)[1:length(levels(z)) - 1]
    # }
    z = droplevels(z)
    if (length(levels(z)) <= 1) {
      return(data.frame(split_point = NA, split_objective = Inf))
    }
    splits = levels(z)[1:length(levels(z)) - 1]
    Y = lapply(Y, function(Y_i) as.matrix(Y_i))
  } else {
    # sort z in increasing order
    ord = order(z)
    z = z[ord]
    if (!is.null(n_quantiles)) {
      if (length(unique(z)) < n_quantiles) {
        splits = unique(z)
      } else {
        qprobs = seq(0, 1, length.out = n_quantiles + 2)[-c(1, n_quantiles + 2)]
        splits = unique(quantile(z, qprobs, type = 7))
      }
    } else {
      splits = unique(z)
    }
    Y_cumsum = lapply(Y, function(Y_i) {
      Y_i = as.matrix(Y_i[ord, ])
      Y_i_cumsum = apply(Y_i, 2, cumsum)
      Y_i_cumsum
    })
  }
  # calculate split objective: sum_j(R_j_l + R_j_r)
  split_objective = vapply(splits, function(split) {
    idx = if (is_categorical) which(z %in% split) else which(z <= split)
    N_l = length(idx)
    N_r = N - N_l
    if (N_l < min_node_size || N_r < min_node_size) {
      return(Inf)
    }
    if (is_categorical) {
      sum(vapply(seq_along(Y), function(i) {
        Y_i = Y[[i]]
        Y_i_l = Y_i[idx, , drop = FALSE]
        Y_i_r = Y_i[-idx, , drop = FALSE]
        S_l = colSums(Y_i_l, na.rm = TRUE)
        S_r = colSums(Y_i_r, na.rm = TRUE)
        sum(-S_l^2 / N_l - S_r^2 / N_r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    } else {
      sum(vapply(seq_along(Y_cumsum), function(i) {
        Y_i_cumsum = Y_cumsum[[i]]
        S_l = Y_i_cumsum[N_l, ]
        S_r = Y_i_cumsum[N, ] - S_l
        sum(-S_l^2 / N_l - S_r^2 / N_r, na.rm = TRUE)
      }, NA_real_), na.rm = TRUE)
    }
  }, FUN.VALUE = NA_real_)

  best = which.min(split_objective)
  best_split_point = splits[best]
  if (!is.factor(z)) {
    right = min(z[which(z > best_split_point)])
    left = max(z[which(z <= best_split_point)])
    best_split_point = (left + right) / 2
  }

  return(data.frame(split_point = best_split_point, split_objective = split_objective[best]))
}
