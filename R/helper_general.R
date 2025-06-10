#---------------------------------------------------------------------------------------------------
# GENERAL HELPER FUNCTIONS
#---------------------------------------------------------------------------------------------------

#' Extract split-node information from a tree list
#'
#' Go through a list‐of‐lists representation of trees and converts
#' node information into a long **data.table**/**data.frame**.
#'
#' @param tree `list`. Each element is one depth level; each depth
#'   contains a list of nodes created by your recursive split function.
#'
#' @return A `data.frame` with one row per node and columns
#'   \describe{
#'     \item{depth}{Integer depth of the node (root = 1).}
#'     \item{id}{Node identifier within its depth level.}
#'     \item{objective.value}{Value of the optimisation criterion.}
#'     \item{intImp}{Internal improvement of the split (or \code{NA} for finals).}
#'     \item{split.feature}{Feature used for the split; \code{"final"} for leaf nodes.}
#'     \item{split.value}{Numeric split point (or \code{NA}).}
#'     \item{node.final}{\code{TRUE} if the node is terminal.}
#'     \item{n.final}{Total count of terminal nodes in the tree (scalar, repeated).}
#'   }
#'
#' @export
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



#' L2 (sum-of-squares) objective
#'
#' @param y A \code{list} of numeric matrices / data.frames.
#' @param x Ignored (kept for a uniform objective-function interface).
#' @param requires.x Logical; always \code{FALSE} here.
#' @param ... Further arguments (currently unused).
#'
#' @return A \code{list} of numeric scalars: the L2 loss for each
#'   element of \code{y}.
#'
#' @keywords internal
SS_L2 = function(y, x, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    # y_sub = feat[,setdiff(colnames(y), c("type",".id",".feature"))]
    ypred = colMeans(as.matrix(feat), na.rm = TRUE)
    sum(t((t(feat) - ypred)^2), na.rm = TRUE)
  })
  L2
}


#' ALE stability objective (sum-of-squares)
#'
#' @inheritParams SS_L2
#' @param split.feat Character string: the feature that is currently
#'   considered for splitting
#'
#' @return A \code{list} of numeric scalars: the ALE L² loss per
#'   element of \code{y}.
#'
#' @keywords internal
SS_ALE = function(y, x, split.feat, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
      interval.n = .N), by = c("interval.index", "x.left", "x.right")]

    df = merge(feat, delta.aggr, by = "interval.index")
    sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE)
  })
  L2
}


#' SHAP objective
#'
#' Fits a cubic regression spline to SHAP values via
#' \code{mgcv::gam()} and returns the residual sum-of-squares (RSS).
#'
#' @inheritParams SS_ALE
#'
#' @return A \code{list} of numeric scalars: the RSS for each element
#'   of \code{y}.
#'
#' @importFrom mgcv gam
#'
#' @keywords internal
SS_SHAP = function(y, x, split.feat, requires.x = FALSE, ...) {
  L2 = lapply(y, function(feat) {
    gam_mod = mgcv::gam(phi ~ s(feat.val, k = 3), data = feat)
    sum(gam_mod$residuals^2, na.rm = TRUE)
  })
  L2
}
