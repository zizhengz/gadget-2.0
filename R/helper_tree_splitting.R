#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR TREE SPLITTING
#---------------------------------------------------------------------------------------------------

#' @importFrom checkmate assert_data_frame assert_integerish assert_function
#' @importFrom data.table rbindlist
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
    feat = as.data.frame(X)[,feat_name, drop=FALSE]
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

  #result = data.table::rbindlist(lapply(opt.feature, as.data.frame), idcol = "feature")
  #result = result[, list(split.points = list(split.points)), by = c("feature", "objective.value", "runtime"), with = TRUE]
  #result$best.split = result$objective.value == min(result$objective.value)
  #result$feature = names(X)
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



# performs a binary split
#' @importFrom checkmate assert_choice
find_best_binary_split = function(xval, y, n.splits = 1, min.node.size, grid,
                                  objective, n.quantiles, ...) {
  assert_choice(n.splits, choices = 1)

  # use different split candidates to perform split
  q = generate_split_candidates(xval[,1], n.quantiles = n.quantiles, min.node.size = min.node.size)
  splits = vapply(q, FUN = function(i) {

    perform_split(i, xval = xval, y = y, min.node.size = min.node.size, grid = grid,
                  objective = objective, ...)
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  # select the split point yielding the minimal objective
  best = which.min(splits)

  return(list(split.points = q[best], objective.value = splits[best]))
}

generate_split_candidates = function(xval, n.quantiles, min.node.size) {

  assert_integerish(min.node.size, upper = floor((length(xval) - 1)/2))
  xval = sort.int(xval)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  xadj = xval[chunk.ind]

  if (!is.null(n.quantiles)) {

    if (length(unique(xval)) < 10) {
      q = unique(xval)
    } else{
      qprobs = seq(0, 1, by = 1/n.quantiles)
      q = unique(quantile(xadj, qprobs, type = 1))
    }

  } else {
    q = unique(xadj)
  }

  # use a value between two subsequent points
  q = adjust_split_point(q, xval)

  return(q)
}

# Performs a single split and measures the objective
perform_split = function(split.points, xval, y, min.node.size, grid, objective, ...) {

  feat = names(xval)
  xval = xval[,1]
  split.points = sort.int(split.points)
  split.points = get_closest_point(split.points, xval, min.node.size)

  # assign intervalnr. according to split points
  node.number = findInterval(x = xval, split.points, rightmost.closed = TRUE) + 1
  # compute size of each childnode
  node.size = tabulate(node.number)
  # if minimum node size is violated, return Inf
  if (min(node.size) < min.node.size)
    return(Inf)
  # compute objective in each interval and sum it up
  y.list = list()
  y.list[[1]] = lapply(y, function(feat){
    feat[which(node.number==1),]
  })
  y.list[[2]] = lapply(y, function(feat){
    feat[which(node.number==2),]
  })

  # x.list only needed if this is used in the objective
  requires.x = formals(objective)[["requires.x"]]
  if (isTRUE(requires.x))
    x.list = split(xval, node.number) else
      x.list = NULL
  res = vapply(seq_along(y.list), FUN = function(i) {
    if(i == 1) grid_sub = grid[[feat]][which(as.numeric(grid[[feat]]) <= split.points)]
    else if(i == 2) grid_sub = grid[[feat]][which(as.numeric(grid[[feat]]) > split.points)]
    sum(unlist(objective(y = y.list[[i]], x = x.list[[i]],split.feat = feat, y.parent = NULL, grid = grid_sub, sub.number = i, ...)))
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  sum(res)
}



adjust_nsplits = function(xval, n.splits) {
  # max. number of splits to be performed must be unique.x-1
  unique.x = length(unique(xval))
  if (n.splits >= unique.x)
    n.splits = unique.x - 1
  return(n.splits)
}



# replace split.points with closest value from xval taking into account min.node.size
get_closest_point = function(split.points, xval, min.node.size = 10) {
  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed if many duplicated values exist)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  if(length(chunk.ind) > length(unique(xval))) xadj = unique(xval)
  else xadj = unique(xval[chunk.ind]) # unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))

  split.adj = numeric(length(split.points))
  for (i in seq_along(split.adj)) {
    d = xadj - split.points[i]
    ind.closest = which.min(abs(d))
    split.adj[i] = xadj[ind.closest]
    xadj = xadj[-ind.closest] # remove already chosen value
  }

  return(sort.int(split.adj))
}

adjust_split_point = function(split.points, xval) {
  # use a value between two subsequent points
  q = split.points
  x.unique = sort.int(unique(xval))
  ind = which(x.unique %in% q)
  ind = ind[ind < length(x.unique)]
  if (length(ind) != length(q)) {
    eps = min(diff(x.unique))/2
  } else {
    eps = (x.unique[ind + 1] - x.unique[ind])/2
  }
  q = q + eps #+ c(diff(q)/2, 0)
  q[q < x.unique[2]] = mean(x.unique[1:2])
  q[q > x.unique[length(x.unique) - 1]] = mean(x.unique[(length(x.unique) - 1):length(x.unique)])

  return(unique(q))
}

#' @importFrom data.table setDT
#' @importFrom tidyr spread
compute_data_for_ice_splitting = function(effect, testdata, Z) {

  # effect: effect object of IML method FeatureEffect
  # testdata: data
  # Output: A data.frame where each row corresponds to a ice curve

  # Z = if (is.null(Z)) setdiff(colnames(testdata), target.feature) else Z
  df = setDT(testdata[,Z, drop = FALSE])
  df = lapply(df, function(feat){
    feat = as.numeric(feat)
    })
  df = setDT(df)

  ice_feat = effect$features

  effectdata = effect$results

  ice = lapply(effectdata, function(feat){
    if(!is.null(feat$.class)) feat = feat[,setdiff(colnames(feat), ".class")]
    if(is.factor(feat$.borders)) feat$.borders = as.numeric(feat$.borders)
    Y = tidyr::spread(feat, .borders, .value)
    Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]

    # center ICE curves by their mean
    Y = Y - rowMeans(Y)
    Y = setDT(Y)
  })
  #names(ice) = ice_feat

  grid = lapply(ice, function(feat){
    colnames(feat)
  })


  return(list(X = df, Y = ice, grid = grid))
}

#' @importFrom data.table
compute_data_for_ale_splitting = function(effect, testdata, Z) {

  # effect: effect object of IML method FeatureEffect
  # testdata: X
  # Output: A data.frame where each row corresponds to a ice curve

  df = setDT(testdata[,Z, drop = FALSE])
  df = lapply(df, function(feat){
    feat = as.numeric(feat)
  })
  df = setDT(df)

  grid = lapply(effect, function(feat){
    feat$feat.val
  })

  return(list(X = df, Y = effect, grid = grid))
}




