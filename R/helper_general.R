#---------------------------------------------------------------------------------------------------
# GENERAL HELPER FUNCTIONS
#---------------------------------------------------------------------------------------------------

# extract information of trees
extract_split_criteria = function(tree) {

  list.split.criteria = lapply(tree, function(depth) {
    lapply(depth, function(node) {

      if (is.null(node)) {
        df = NULL
      } else if (node$improvement.met | node$stop.criterion.met | node$depth == length(tree)) {
        df = data.frame("depth" = node$depth, "id" = node$id,
          "objective.value" = node$objective.value,
          "objective.value.parent" = node$objective.value.parent,
          "intImp" = NA,
          "intImp.parent" = NA,
          "split.feature" = "final",
          "split.value" = NA,
          "split.feature.parent" = node$split.feature.parent,
          "node.final" = TRUE)
      } else {
        df = data.frame("depth" = node$depth, "id" = node$id,
          "objective.value" = node$objective.value,
          "objective.value.parent" = node$objective.value.parent,
          "intImp" = node$intImp,
          "intImp.parent" = node$intImp.parent,
          "split.feature" = node$split.feature,
          "split.value" = node$split.value,
          "split.feature.parent" = node$split.feature.parent,
          "node.final" = FALSE)
      }
      df
    })
  })
  # list.split.criteria = list.clean(list.split.criteria, function(x) length(x) == 0L, TRUE)
  list.split.criteria = Filter(function(x) length(x) > 0L, list.split.criteria)
  df.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  df.split.criteria = as.data.frame(do.call(rbind, df.split.criteria))
  n.final = length(which(df.split.criteria$node.final == TRUE))
  df.split.criteria$n.final = n.final


  return(df.split.criteria)
}



# objective functions
SS_L2 = function(y, x, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    # y_sub = feat[,setdiff(colnames(y), c("type",".id",".feature"))]
    ypred = colMeans(as.matrix(feat), na.rm = TRUE)
    sum(t((t(feat) - ypred)^2), na.rm = TRUE)
  })
  L2
}

SS_ALE = function(y, x, split.feat, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
      interval.n = .N), by = c("interval.index", "x.left", "x.right")]

    df = merge(feat, delta.aggr, by = "interval.index")
    sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE)
  })
  L2
}

SS_SHAP = function(y, x, split.feat, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    gam_mod = mgcv::gam(phi ~ s(feat.val, k = 3), data = feat)
    sum(gam_mod$residuals^2, na.rm = TRUE)
  })
  L2
}
