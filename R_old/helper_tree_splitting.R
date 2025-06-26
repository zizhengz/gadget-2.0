#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR TREE SPLITTING
#---------------------------------------------------------------------------------------------------

#' Evaluate and select the best binary split for all features
#'
#' @param Y A list or data structure representing response values
#'   (e.g. centered ICE curves) that the supplied \code{objective}
#'   knows how to consume.
#' @param X A \code{data.frame} (or matrix) of predictor variables.
#' @param n.splits Integer. How many split points the optimizer should
#'   return per feature (default 1 for binary splits).
#' @param min.node.size Integer. Minimum number of observations that
#'   each child node must retain.
#' @param optimizer A function with signature
#'   \code{optimizer(x, y, n.splits, min.node.size, grid, objective,
#'   n.quantiles, ...)} that returns at least
#'   \code{$split.points} and \code{$objective.value}.
#' @param grid Either a numeric vector or list of candidate split
#'   points passed untouched to \code{optimizer()}.
#' @param objective Objective function with interface
#'   \code{objective(y, x, requires.x, ...)} used inside the optimizer
#'   to score splits.
#' @param n.quantiles Integer specifying how many quantile-based
#'   candidate points the optimizer should pre-compute.
#' @param ... Further arguments passed directly to
#'   \code{optimizer()}.
#'
#' @return A **data.table** with columns
#'   \describe{
#'     \item{feature}{Feature name.}
#'     \item{objective.value}{Numeric value returned by \code{objective}.}
#'     \item{runtime}{Elapsed time (sec) spent inside \code{optimizer()}.}
#'     \item{split.points}{A list column; each cell holds the vector of
#'       split points (may be \code{NA}).}
#'     \item{best.split}{Logical flag indicating which feature attains
#'       the minimum objective.}
#'   }
#'
#' @importFrom checkmate assert_data_frame assert_integerish assert_function
#' @importFrom data.table rbindlist
#'
#' @keywords internal
split_parent_node = function(Y, X, n.splits = 1, min.node.size = 40, optimizer, grid,
  objective, n.quantiles, ...) {

  checkmate::assert_data_frame(X)
  checkmate::assert_integerish(n.splits)
  checkmate::assert_integerish(min.node.size)
  checkmate::assert_function(objective, args = c("y", "x", "requires.x"))
  checkmate::assert_function(optimizer, args = c("xval", "y"))

  # find best split points per feature
  opt.feature = lapply(names(X), function(feat_name) {

    t1 = proc.time()
    feat = as.data.frame(X)[, feat_name, drop = FALSE]
    if (length(unique(feat)[[1]]) == 1) {
      res = list(split.points = NA, objective.value = Inf)
    } else {
      res = optimizer(x = feat, y = Y, n.splits = n.splits, min.node.size = min.node.size, grid = grid,
        objective = objective, n.quantiles = n.quantiles, ...)
    }

    t2 = proc.time()
    res$runtime = (t2 - t1)[[3]]
    return(res)
  })

  # result = data.table::rbindlist(lapply(opt.feature, as.data.frame), idcol = "feature")
  # result = result[, list(split.points = list(split.points)), by = c("feature", "objective.value", "runtime"), with = TRUE]
  # result$best.split = result$objective.value == min(result$objective.value)
  # result$feature = names(X)
  result = data.table::rbindlist(
    lapply(seq_along(opt.feature), function(i) {
      item = opt.feature[[i]]
      if (is.null(item$split.points)) item$split.points = NA
      data.table(
        feature = names(X)[i],
        objective.value = item$objective.value,
        runtime = item$runtime,
        split.points = list(item$split.points)
      )
    })
  )

  result$best.split = result$objective.value == min(result$objective.value)

  if (all(is.infinite(result$objective.value))) {
    result$best.split = FALSE
  }

  return(result)
}


#' @importFrom checkmate assert_data_table
generate_node_index = function(Y, X, result) {
  assert_data_table(result)
  feature = unique(result$feature[result$best.split])
  split.points = unlist(result$split.points[result$best.split])

  if (is.vector(X)) {
    xval = X
  } else {
    xval = X[, feature]
  }

  cuts = c(min(xval), split.points, max(xval))
  sp = cut(xval, breaks = unique(cuts), include.lowest = TRUE)
  return(list(class = sp, index = split(seq_along(xval), sp)))
}


#' Identify the best split point for a single feature
#'
#' @param xval A one‐column \code{data.frame} or matrix of predictor
#'   values for the feature under inspection.
#' @param y Response object (list, matrix, or data.frame) that the
#'   \code{objective} function understands.
#' @param n.splits Integer; must be \code{1}. The argument is checked
#'   but otherwise unused.
#' @param min.node.size Minimum number of observations each child node
#'   must contain.
#' @param grid Optional vector/list of grid points forwarded to
#'   \code{perform_split()} (e.g., ICE/PDP evaluation grid).
#' @param objective Objective function with interface
#'   \code{objective(y, x, requires.x, ...)} used inside
#'   \code{perform_split()}.
#' @param n.quantiles Number of quantile-based candidate points to pass
#'   to \code{generate_split_candidates()}.
#' @param ... Additional arguments propagated to \code{perform_split()}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{split.points}{Numeric value of the best split.}
#'     \item{objective.value}{Objective value attained at that split.}
#'   }
#'
#' @importFrom checkmate assert_choice
#'
#' @keywords internal
find_best_binary_split = function(xval, y, n.splits = 1, min.node.size, grid,
  objective, n.quantiles, ...) {
  assert_choice(n.splits, choices = 1)

  # use different split candidates to perform split
  q = generate_split_candidates(xval[, 1], n.quantiles = n.quantiles, min.node.size = min.node.size)

  if (length(q) == 0) {
    return(list(split.points = NA_real_, objective.value = Inf))
  }

  if (is.factor(xval[, 1]) || is.character(xval[, 1])) {
    splits.objective.values = numeric(length(levels(xval[, 1])))
    if (length(levels(xval[, 1])) == 2) {
      split.levels = levels(xval[, 1])[1]
      splits.objective.values = perform_split_cat(
        split.levels = split.levels,
        xval = xval,
        y = y,
        min.node.size = min.node.size,
        grid = grid,
        objective = objective
      )
      best = which.min(splits.objective.values)
      return(list(split.points = levels(xval[, 1])[1], objective.value = splits.objective.values[best]))
    } else {
      for (i in seq_along(levels(xval[, 1]))) {
        split.levels = levels(xval[, 1])[i]
        res = perform_split_cat(
          split.levels = split.levels,
          xval = xval,
          y = y,
          min.node.size = min.node.size,
          grid = grid,
          objective = objective
        )
        splits.objective.values[i] = sum(res)
      }
      best = which.min(splits.objective.values)
      return(list(split.points = levels(xval[, 1])[best], objective.value = splits.objective.values[best]))
    }
  }
  else {
    splits.objective.values = vapply(q, FUN = function(i) {
      perform_split(i, xval = xval, y = y, min.node.size = min.node.size, grid = grid,
        objective = objective, ...)
    }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
    # select the split point yielding the minimal objective
    best = which.min(splits.objective.values)
    return(list(split.points = q[best], objective.value = splits.objective.values[best]))
  }
}


#' Generate candidate split points for a numeric feature
#'
#' @param xval Numeric vector (or length‐\eqn{n} atomic) containing the
#'   feature values of the parent node.
#' @param n.quantiles Integer or \code{NULL}. If not \code{NULL}, the
#'   function takes sample quantiles of \code{xval} (after enforcing
#'   \code{min.node.size}) at probabilities
#'   \eqn{0, 1/n, 2/n, \dots, 1}. When \code{NULL}, all eligible values
#'   in \code{xval} become candidates.
#' @param min.node.size Integer. Each child node must have at least
#'   this many observations.
#'
#' @return Numeric vector of candidate split points.
#'
#' @importFrom checkmate assert_integerish
#'
#' @keywords internal
generate_split_candidates = function(xval, n.quantiles, min.node.size, max.subsets = 32L) {

  if (length(xval) < 2 * min.node.size + 1) {
    return(numeric(0))
  }

  assert_integerish(min.node.size, upper = floor((length(xval) - 1) / 2))

  if (is.factor(xval) || is.character(xval)) {
    return(generate_categorical_split_candidates(xval, min.node.size, max.subsets))
  } else {
    xval = sort.int(xval)
    # chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
    if ((min.node.size + 1) <= (length(xval) - min.node.size)) {
      chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
    } else {
      chunk.ind = integer(0)
    }
    xadj = xval[chunk.ind]

    if (!is.null(n.quantiles)) {

      if (length(unique(xval)) < 10) {
        q = unique(xval)
      } else {
        qprobs = seq(0, 1, by = 1 / n.quantiles)
        q = unique(quantile(xadj, qprobs, type = 1))
      }

    } else {
      q = unique(xadj)
    }

    # use a value between two subsequent points
    q = adjust_split_point(q, xval)

    return(q)
  }
}

generate_categorical_split_candidates = function(xval, min.node.size = 1L, max.subsets = NULL) {
  levels = levels(xval)
  k = length(levels)

  if (k <= 1) {
    return(list())
  }

  all_subsets = unlist(
    lapply(1:(k - 1), function(i) combn(levels, i, simplify = FALSE)),
    recursive = FALSE
  )

  valid_subsets = Filter(function(subset_levels) {
    node.left = xval %in% subset_levels
    node.right = !node.left
    sum(node.left) >= min.node.size && sum(node.right) >= min.node.size
  }, all_subsets)

  if (!is.null(max.subsets) && length(valid_subsets) > max.subsets) {
    valid_subsets = valid_subsets[seq_len(max.subsets)]
  }

  return(valid_subsets)
}


#' Compute the objective value of a candidate binary split
#'
#' @param split.points Numeric vector of candidate cut points. Only the
#'   first element is used after being adjusted by
#'   \code{get_closest_point()}.
#' @param xval A single-column \code{data.frame} (or matrix) with the
#'   feature values of the parent node.
#' @param y A list (usually per-feature ICE/ALE/SHAP matrices) that the
#'   supplied \code{objective} function can understand.
#' @param min.node.size Integer. Minimum number of observations
#'   required in each child node.
#' @param grid A named list of evaluation grids; the element
#'   corresponding to the current feature is passed down to
#'   \code{objective()} so that losses can be computed on the proper
#'   subset of grid points.
#' @param objective Objective function with interface
#'   \code{objective(y, x, split.feat, y.parent, grid, sub.number,
#'   ...)}. Must return a numeric scalar (or vector that can be summed).
#' @param ... Additional arguments forwarded to \code{objective()}.
#'
#' @return A single numeric value: the summed objective of the two
#'   child nodes, or \code{Inf} if \code{min.node.size} is violated.
#'
#' @keywords internal
perform_split = function(split.points, xval, y, min.node.size, grid, objective, ...) {

  feat = names(xval)
  xval = xval[, 1]
  split.points = sort.int(split.points)
  split.points = get_closest_point(split.points, xval, min.node.size)

  # assign intervalnr. according to split points
  node.number = findInterval(x = xval, split.points, rightmost.closed = TRUE) + 1
  # compute size of each childnode
  node.size = tabulate(node.number)
  # if minimum node size is violated, return Inf
  if (min(node.size) < min.node.size) {
    return(Inf)
  }
  # compute objective in each interval and sum it up
  y.list = list()
  y.list[[1]] = lapply(y, function(feat) {
    feat[which(node.number == 1), ]
  })
  y.list[[2]] = lapply(y, function(feat) {
    feat[which(node.number == 2), ]
  })

  # x.list only needed if this is used in the objective
  requires.x = formals(objective)[["requires.x"]]
  if (isTRUE(requires.x)) {
    x.list = split(xval, node.number)
  } else {
    x.list = NULL
  }
  res = vapply(seq_along(y.list), FUN = function(i) {
    grid_sub = if (i == 1) {
      grid[[feat]][which(as.numeric(grid[[feat]]) <= split.points)]
    } else {
      grid[[feat]][which(as.numeric(grid[[feat]]) > split.points)]
    }
    # sum for (multiple) features in S
    sum(unlist(objective(y = y.list[[i]], x = x.list[[i]],
                         split.feat = feat, y.parent = NULL,
                         grid = grid_sub, sub.number = i, ...)))
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  # sum for left and right region
  sum(res)
}


perform_split_cat = function(split.levels, xval, y, min.node.size, grid, objective, ...) {
  feat = colnames(xval)[1]
  xval_vec = xval[[1]]

  node.left = xval_vec %in% split.levels
  node.right = !node.left

  if (sum(node.left) < min.node.size || sum(node.right) < min.node.size) {
    return(Inf)
  }

  y.left = lapply(y, function(feat) feat[node.left, , drop = FALSE])
  y.right = lapply(y, function(feat) feat[node.right, , drop = FALSE])
  y.list = list(y.left, y.right)

  requires.x = formals(objective)[["requires.x"]]
  if (isTRUE(requires.x)) {
    x.left = xval[node.left, , drop = FALSE]
    x.right = xval[node.right, , drop = FALSE]
    x.list = list(x.left, x.right)
  } else {
    x.list = list(NULL, NULL)
  }

  # -- Grid --
  grid_vals = grid[[feat]]
  if (is.factor(grid_vals)) grid_vals = as.character(grid_vals)
  split.char = as.character(split.levels)
  grid_sub = list(
    grid_vals[grid_vals %in% split.char],
    grid_vals[!grid_vals %in% split.char]
  )

  res = vapply(seq_along(y.list), function(i) {
    loss = sum(unlist(objective(
      y = y.list[[i]],
      x = x.list[[i]],
      split.feat = feat,
      y.parent = NULL,
      grid = grid_sub[[i]],
      sub.number = i,
      ...
    )))
    if (!is.finite(loss)) Inf else loss
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)

  return(res)
}

adjust_nsplits = function(xval, n.splits) {
  # max. number of splits to be performed must be unique.x-1
  unique.x = length(unique(xval))
  if (n.splits >= unique.x) {
    n.splits = unique.x - 1
  }
  return(n.splits)
}

#' Map candidate split points to the nearest admissible values
#'
#' @param split.points Numeric vector of preliminary candidate cut
#'   points (e.g., quantiles or mid‐points).
#' @param xval Numeric vector of the feature values in the parent node.
#' @param min.node.size Integer. Minimum number of observations each
#'   child node must preserve.
#'
#' @return Numeric vector of the same length as \code{split.points}
#'   containing adjusted split locations.
#'
#' @keywords internal
get_closest_point = function(split.points, xval, min.node.size = 10) {

  if (length(xval) < 2 * min.node.size + 1) {
    return(numeric(0))
  }

  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed if many duplicated values exist)
  # chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  if ((min.node.size + 1) <= (length(xval) - min.node.size)) {
    chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  } else {
    chunk.ind = integer(0)
  }
  if (length(chunk.ind) > length(unique(xval))) {
    xadj = unique(xval)
  } else {
    xadj = unique(xval[chunk.ind])
  } # unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))

  split.adj = numeric(length(split.points))
  for (i in seq_along(split.adj)) {
    d = xadj - split.points[i]
    ind.closest = which.min(abs(d))
    split.adj[i] = xadj[ind.closest]
    xadj = xadj[-ind.closest] # remove already chosen value
  }

  return(sort.int(split.adj))
}


#' Shift candidate split points into open intervals
#'
#' @param split.points Numeric vector of preliminary split locations
#'   (typically returned by \code{generate_split_candidates()} or
#'   \code{get_closest_point()}).
#' @param xval Numeric vector of feature values from the parent node.
#'
#' @return Numeric vector of the same length as \code{split.points},
#'   adjusted so that each element falls strictly inside an open
#'   interval between consecutive unique values of \code{xval}. Any
#'   duplicates are removed via \code{unique()} before returning.
#'
#' @keywords internal
adjust_split_point = function(split.points, xval) {
  if (is.factor(xval)) {
    return(split.points)
  }
  # use a value between two subsequent points
  q = split.points
  x.unique = sort.int(unique(xval))
  ind = which(x.unique %in% q)
  ind = ind[ind < length(x.unique)]
  if (length(ind) != length(q)) {
    eps = min(diff(x.unique)) / 2
  } else {
    eps = (x.unique[ind + 1] - x.unique[ind]) / 2
  }
  q = q + eps #+ c(diff(q)/2, 0)
  q[q < x.unique[2]] = mean(x.unique[1:2])
  q[q > x.unique[length(x.unique) - 1]] = mean(x.unique[(length(x.unique) - 1):length(x.unique)])

  return(unique(q))
}


#' Prepare centered ICE matrices and covariate data for split search
#'
#' @param effect An object returned by \pkg{iml}\::\code{FeatureEffect}
#'   (with \code{method = "ice"}). Must contain slots
#'   \code{\$results} and \code{\$features}.
#' @param testdata The original data set passed to
#'   \code{FeatureEffect$new()} (or a subset of it) as a
#'   \code{data.frame}/\code{data.table}.
#' @param Z Character vector of column names to be used as splitting features
#'
#' @return A named \code{list} with three elements:
#'   \describe{
#'     \item{\code{X}}{A \code{data.table} containing the selected
#'       covariates, coercing factors to numeric when needed.}
#'     \item{\code{Y}}{A list of \code{data.table}s, one per
#'       feature, each in wide ICE matrix form and centered by row
#'       means. Each row corresponds to a ice curve}
#'     \item{\code{grid}}{A list; each element is a character vector of
#'       column names (i.e.\ grid points) used for the corresponding
#'       ICE matrix in \code{Y}.}
#'   }
#'
#' @importFrom data.table setDT
#' @importFrom tidyr pivot_wider
#'
#' @keywords internal
compute_data_for_ice_splitting = function(effect, testdata, Z) {
  # Z = if (is.null(Z)) setdiff(colnames(testdata), target.feature) else Z
  # df = setDT(testdata[, Z, drop = FALSE])
  # df = lapply(df, function(feat) {
  #   feat = as.numeric(feat)
  # })
  # df = setDT(df)
  df = setDT(copy(testdata[, Z, drop = FALSE]))
  for (col in Z) {
    x = df[[col]]
    if (is.character(x) || is.factor(x)) {
      df[[col]] = as.factor(x)
    } else if (is.numeric(x) && length(unique(x)) <= 7) {
      df[[col]] = as.factor(x)
    }
  }
  ice_feat = effect$features

  effectdata = effect$results

  ice = lapply(effectdata, function(feat) {
    if (!is.null(feat$.class)) feat = feat[, setdiff(colnames(feat), ".class")]
    if (is.factor(feat$.borders)) feat$.borders = as.numeric(feat$.borders)
    Y = tidyr::pivot_wider(feat, names_from = .borders, values_from = .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]

    # center ICE curves by their mean
    Y = Y - rowMeans(Y)
    Y = setDT(Y)
  })
  # names(ice) = ice_feat

  grid = lapply(ice, function(feat) {
    colnames(feat)
  })


  return(list(X = df, Y = ice, grid = grid))
}



#' @importFrom data.table setDT
compute_data_for_ale_splitting = function(effect, testdata, Z) {

  # effect: effect object of IML method FeatureEffect
  # testdata: X
  # Output: A data.frame where each row corresponds to a ice curve

  df = data.table::setDT(testdata[, Z, drop = FALSE])
  df = lapply(df, function(feat) {
    feat = as.numeric(feat)
  })
  df = setDT(df)

  grid = lapply(effect, function(feat) {
    feat$feat.val
  })

  return(list(X = df, Y = effect, grid = grid))
}
