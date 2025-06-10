
#' R6 class: \code{Node}
#'
#' @docType class
#' @name Node
#'
#' @field id           Integer. Identifier within its depth level
#'   (\code{1} = left, \code{2} = right).
#' @field depth        Integer. Depth of the node; root starts at
#'   \code{1}.
#' @field subset.idx   Integer vector with row indices of
#'   \code{testdata} that fall into this node.
#' @field objective.value Numeric vector of objective values for the
#'   node (one per feature).
#' @field objective.value.parent Numeric vector holding the parent's
#'   objective values.
#' @field grid         Named list; each element is the set of grid
#'   points (column names).
#' @field id.parent    Integer. ID of the parent node (\code{NULL} for
#'   root).
#' @field child.type   Character. Either \code{"<="} or \code{">"} to
#'   indicate left / right traversal.
#' @field split.feature Character. Feature chosen for splitting this
#'   node.
#' @field split.value  Numeric. Threshold applied to
#'   \code{split.feature}.
#' @field children     Two-element list with \code{left.child} and
#'   \code{right.child} (each another \code{Node} or \code{NULL}).
#' @field stop.criterion.met Logical. Whether the minimal node size or
#'   improvement threshold has been reached.
#' @field improvement.met Logical. Whether the improvement threshold
#'   (\code{impr.par}) was not met.
#' @field intImp       Numeric. Interaction importance of the node.
#' @field local        Optional cached copy of centered ICE / ALE /
#'   SHAP data used for fast re-computation.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new(id, depth, subset.idx, grid, …)}}{Constructor.}
#'   \item{\code{$computeSplit(X, Y, objective, …)}}{Find best split,
#'     update node metadata.}
#'   \item{\code{$computeChildren(X, Y, testdata, objective, …)}}{Create
#'     \code{left.child} and \code{right.child} as new \code{Node}
#'     instances.}
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric assert_character
#' @importFrom data.table setDT
#'
#' @keywords internal
Node = R6::R6Class("Node", list(
  id = NULL,

  # on which depth is the node
  depth = NULL,

  # ids of the instances of data that are in this node
  subset.idx = NULL,
  objective.value = NULL, # objective value in a node
  objective.value.parent = NULL,

  # grid values included in node
  grid = NULL,

  # Parent information
  id.parent = NULL,
  child.type = NULL, # left or right type

  # Split information (if splitting has already taken place)
  split.feature = NULL,
  split.feature.parent = NULL,
  split.value = NULL,
  split.value.parent = NULL,

  # Append the children of this node
  children = list(),

  stop.criterion.met = FALSE,
  improvement.met = NULL,

  intImp = NULL,
  intImp.parent = NULL,
  # rsqrt = NULL,
  store.data = FALSE,
  local = NULL,



  initialize = function(id, depth = NULL, subset.idx, grid, id.parent = NULL, child.type = NULL, objective.value.parent = NULL, objective.value = NULL, store.data, improvement.met, intImp) {

    assert_numeric(id, len = 1)
    assert_numeric(depth, len = 1, null.ok = TRUE)

    assert_numeric(subset.idx, min.len = 1)
    assert_numeric(id.parent, len = 1, null.ok = TRUE)
    assert_character(child.type, null.ok = TRUE)

    self$id = id
    self$depth = depth
    self$subset.idx = subset.idx
    self$id.parent = id.parent
    self$child.type = child.type
    self$intImp = intImp
    self$objective.value.parent = objective.value.parent
    self$objective.value = objective.value
    self$grid = grid
    self$store.data = store.data
    self$local = NULL


    self$stop.criterion.met = FALSE
    self$improvement.met = improvement.met
  },

  computeSplit = function(X, Y, objective, method, impr.par, optimizer, min.split, n.quantiles, grid) {

    if (length(self$subset.idx) < min.split | self$improvement.met == TRUE) {
      self$stop.criterion.met = TRUE
    } else {

      if (method == "SS_L2_pd") {
        if (self$store.data == TRUE & !is.null(self$local)) {
          Y_sub = self$local
        } else {
          Y_sub = lapply(names(Y), function(feat) {
            # subset in this node
            Y_j = Y[[feat]][self$subset.idx, ]
            # delete grid points not in this node
            Y_j[, which(!(colnames(Y_j) %in% self$grid[[feat]]))] = NA
            # mean-centering ice curves with less grid points
            Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
            Y_j = setDT(Y_j)
            return(Y_j)
          })
          names(Y_sub) = names(Y)
        }

      }

      else if (method == c("SS_L2_ale")) {
        if (self$store.data == TRUE & !is.null(self$local)) {
          Y_sub = self$local
        }
        else {
          Y_sub = lapply(names(Y), function(feat) {
            Y_j = Y[[feat]][self$subset.idx, ] # subset in this node
            Y_j = setDT(Y_j)
            return(Y_j)
          })
          names(Y_sub) = names(Y)
        }

      }

      else if (method == c("SS_L2_shap")) {
        if (self$store.data == TRUE & !is.null(self$local)) {
          Y_sub = self$local
        }
        else {
          Y_sub = lapply(names(Y), function(feat) {
            Y_j = Y[[feat]][self$subset.idx, ] # subset in this node
            Y_j = setDT(Y_j)
            return(Y_j)
          })
          names(Y_sub) = names(Y)
        }

      }

      objective.value.root = objective(y = Y, x = X, split.feat = NULL)
      if (is.null(self$objective.value)) {
        self$objective.value.parent = objective.value.root
        self$split.feature.parent = NA
        self$split.value.parent = NA
        self$intImp.parent = NA
      }
      self$objective.value = objective(y = Y_sub, x = X[self$subset.idx, ], split.feat = self$split.feature, y.parent = NULL, grid = grid)


      tryCatch({
        split = split_parent_node(Y = Y_sub, X = X[self$subset.idx, ], objective = objective, optimizer = find_best_binary_split, min.node.size = min.split, n.quantiles = n.quantiles, grid = grid)


        if (is.null(self$intImp)) {
          self$intImp = 0
        }
        intImp = (sum(unlist(self$objective.value)) - split$objective.value[split$best.split][1]) / sum(unlist(objective.value.root))

        if (self$intImp == 0) {
          if (intImp < impr.par) {
            self$improvement.met = TRUE
          } else {
            self$split.feature = split$feature[split$best.split][1]
            self$split.value = unlist(split$split.points[split$best.split])[1]
            self$intImp = intImp

          }
        }
        else {
          if (intImp < self$intImp * impr.par) {
            self$improvement.met = TRUE
          } else {
            self$split.feature = split$feature[split$best.split][1]
            self$split.value = unlist(split$split.points[split$best.split])[1]
            self$intImp.parent = self$intImp
            self$intImp = intImp
          }
        }
      },
      error = function(cond) {
        # message(paste0("Min.node.size is reached in node ", self$id))
        self$stop.criterion.met = TRUE
      }
      )
    }
  },

  computeChildren = function(X, Y, testdata, objective, method, model, predict.function, shap.recalc) {
    # browser()
    if (self$stop.criterion.met | self$improvement.met) {
      # no further split is performed
      self$children = list("left.child" = NULL, "right.child" = NULL)
    } else {
      if (is.null(self$split.feature)) {
        stop("Please compute the split first via computeSplit().")
      }

      idx.left = which(X[self$subset.idx, self$split.feature, with = FALSE] <= self$split.value)
      idx.right = which(X[self$subset.idx, self$split.feature, with = FALSE] > self$split.value)

      idx.left = self$subset.idx[idx.left]
      if (length(idx.left) == 0) idx.left = 0
      idx.right = self$subset.idx[idx.right]
      if (length(idx.right) == 0) idx.right = 0

      # grid.left = self$grid
      # grid.left[[X[,self$split.feature]]] = grid.left[[X[,self$split.feature]]][as.numeric(grid.left[[X[,self$split.feature]]]) <= self$split.value]
      # grid.right = self$grid
      # grid.right[[X[,self$split.feature]]] = grid.right[[X[,self$split.feature]]][as.numeric(grid.right[[X[,self$split.feature]]]) > self$split.value]

      # Explicitly extract current split feature name
      feat_name = self$split.feature
      # Copy current grid
      grid.left = self$grid
      grid.right = self$grid
      # Update grid range for left and right child
      grid.left[[feat_name]] = grid.left[[feat_name]][as.numeric(grid.left[[feat_name]]) <= self$split.value]
      grid.right[[feat_name]] = grid.right[[feat_name]][as.numeric(grid.right[[feat_name]]) > self$split.value]


      if (method == "SS_L2_pd") {
        Y_left = lapply(names(Y), function(feat) {
          # subset in this node
          Y_j = Y[[feat]][idx.left, ]
          # delete grid points not in this node
          Y_j[, which(!(colnames(Y_j) %in% grid.left[[feat]]))] = NA
          # mean-centering ice curves with less grid points
          Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
          Y_j = setDT(Y_j)
          return(Y_j)
        })
        Y_right = lapply(names(Y), function(feat) {
          # subset in this node
          Y_j = Y[[feat]][idx.right, ]
          # delete grid points not in this node
          Y_j[, which(!(colnames(Y_j) %in% grid.right[[feat]]))] = NA
          # mean-centering ice curves with less grid points
          Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
          Y_j = setDT(Y_j)
          return(Y_j)
        })
        names(Y_left) = names(Y_right) = names(Y)
      }

      if (method == "SS_L2_ale") {
        Y_left = lapply(names(Y), function(feat) {
          # browser()
          Y_j = Y[[feat]][idx.left, ] # subset in this node
          if (feat == self$split.feature & is.numeric(Y_j[, feat.val])) { # adjust when smooth function is not able to learn jumps
            # max_val = ifelse(length(which(Y_j$interval.index==max(Y_j$interval.index))) == 1, max(Y_j$interval.index)-1, max(Y_j$interval.index))
            max_fraction = max(round(0.1 * nrow(Y_j), 0), 10)
            max_f = sort(Y_j[, feat.val], decreasing = TRUE)[max_fraction]
            max_val = unique(Y_j[which(Y_j[, feat.val] >= max_f), ]$interval.index)
            feat_val_max = which(Y_j$interval.index %in% max_val)
            feat_val_not_max = -which(Y_j$interval.index %in% max_val)

            if (!is.na(sd(Y_j$dL[feat_val_not_max]))) {
              if (sd(Y_j$dL[feat_val_max]) > 2 * sd(Y_j$dL[feat_val_not_max])) {
                Y_j$dL[feat_val_max] = rnorm(n = length(feat_val_max), mean = mean(Y_j$dL[feat_val_not_max]), sd = sd(Y_j$dL[feat_val_not_max]))
              }
            }
          }
          if (length(unique(Y_j[, feat.val])) == 1) {
            Y_j$dL = 0
          } else if (feat == self$split.feature & is.factor(Y_j[, feat.val])) {
            Y_j = ale_deriv(model, feat, testdata[idx.left, ], "y", 10, predict.fun = predict.function)[[1]]
          }

          Y_j = setDT(Y_j)
          return(Y_j)
        })
        Y_right = lapply(names(Y), function(feat) {
          Y_j = Y[[feat]][idx.right, ] # subset in this node
          if (feat == self$split.feature & is.numeric(Y_j[, feat.val])) { # adjust when smooth function is not able to learn jumps
            # min_val = ifelse(length(which(Y_j$interval.index==min(Y_j$interval.index))) == 1, min(Y_j$interval.index)+1, min(Y_j$interval.index))
            min_fraction = max(round(0.1 * nrow(Y_j), 0), 10)

            min_f = sort(Y_j[, feat.val], decreasing = FALSE)[min_fraction]
            min_val = unique(Y_j[which(Y_j[, feat.val] <= min_f), ]$interval.index)
            feat_val_min = which(Y_j$interval.index %in% min_val)
            feat_val_not_min = -which(Y_j$interval.index %in% min_val)
            if (!is.na(sd(Y_j$dL[feat_val_not_min]))) {
              if (sd(Y_j$dL[feat_val_min]) > 2 * sd(Y_j$dL[feat_val_not_min])) {
                Y_j$dL[feat_val_min] = rnorm(n = length(feat_val_min), mean = mean(Y_j$dL[feat_val_not_min]), sd = sd(Y_j$dL[feat_val_not_min]))
              }
            }
          }
          if (length(unique(Y_j[, feat.val])) == 1) {
            Y_j$dL = 0
          } else if (feat == self$split.feature & is.factor(Y_j[, feat.val])) {
            Y_j = ale_deriv(model, feat, testdata[idx.right, ], "y", 10, predict.fun = predict.function)[[1]]
          }
          Y_j = setDT(Y_j)
          return(Y_j)
        })

        names(Y_left) = names(Y_right) = names(Y)
      }

      if (method == c("SS_L2_shap")) {
        if (shap.recalc == TRUE) {
          pred.fun = predict.function
          Y_left = fast_shap_values(model, S = names(Y), data = testdata[idx.left, ], target = "y", nsim = 500, pred.fun = pred.fun)
          Y_right = fast_shap_values(model, S = names(Y), data = testdata[idx.right, ], target = "y", nsim = 500, pred.fun = pred.fun)

        }
        else {
          Y_left = lapply(names(Y), function(feat) {
            Y_j = Y[[feat]][idx.left, ] # subset in this node
            if (feat == self$split.feature) {
              pred.fun = predict.function
              Y_j = fast_shap_values(model, S = feat, data = testdata[idx.left, ], target = "y", nsim = 500, pred.fun = pred.fun)[[1]]
            }
            Y_j = setDT(Y_j)
            return(Y_j)
          })
          Y_right = lapply(names(Y), function(feat) {
            Y_j = Y[[feat]][idx.right, ] # subset in this node
            if (feat == self$split.feature) {
              pred.fun = predict.function
              Y_j = fast_shap_values(model, S = feat, data = testdata[idx.right, ], target = "y", nsim = 500, pred.fun = pred.fun)[[1]]
            }
            Y_j = setDT(Y_j)
            return(Y_j)
          })
          names(Y_left) = names(Y_right) = names(Y)
        }
      }

      obj.left = objective(y = Y_left, x = X[idx.left, ], split.feat = self$split.feature, y.parent = NULL, grid = grid.left[[self$split.feature]])
      obj.right = objective(y = Y_right, x = X[idx.right, ], split.feat = self$split.feature, y.parent = NULL, grid = grid.right[[self$split.feature]])
      obj.parent = self$objective.value

      left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = "<=", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.left, objective.value = obj.left, objective.value.parent = obj.parent, store.data = self$store.data)
      right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = ">", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.right, objective.value = obj.right, objective.value.parent = obj.parent, store.data = self$store.data)

      # left.child$split.value = right.child$split.value = self$split.value
      left.child$split.feature.parent = right.child$split.feature.parent = self$split.feature
      left.child$split.value.parent = right.child$split.value.parent = self$split.value

      if (self$store.data == TRUE) {
        left.child$local = Y_left
        right.child$local = Y_right
      }

      self$children = list("left.child" = left.child, "right.child" = right.child)
    }
  }
))



#' Compute single tree based on Class 'Node'
#'
#' @param effect An \pkg{iml}\::\code{FeatureEffect} object created
#'   with \code{method = "ice" \| "ale"} (depending on \code{objective}).
#' @param testdata Original data set (\code{data.frame}/\code{data.table})
#'   used for model training or evaluation.
#' @param model   Fitted prediction model (passed through to SHAP/ALE
#'   recalculations).
#' @param predict.function Function of signature
#'   \code{predict.function(model, newdata)} returning numeric
#'   predictions.
#' @param objective Character. One of \code{"SS_L2_pd"},
#'   \code{"SS_L2_ale"}, or \code{"SS_L2_shap"}.
#' @param Z Character vector of covariate names kept as splitting
#'   candidates.
#' @param target.feature Character. Label for y-axis in downstream
#'   plots (not used inside the algorithm).
#' @param n.split Integer. Maximum tree depth (root counts as depth 1).
#' @param impr.par Numeric in (0, 1]. Relative improvement threshold:
#'   splits with improvement below \code{impr.par} are not pursued.
#' @param min.split Integer. Minimum observations that each child node
#'   must hold.
#' @param n.quantiles Integer. Number of quantile based candidate
#'   points passed to \code{generate_split_candidates()}.
#' @param store.data Logical. If \code{TRUE}, caches node-local ICE /
#'   ALE / SHAP matrices inside each \code{Node} to accelerate later
#'   operations (at the cost of memory).
#' @param shap.recalc Logical. If \code{TRUE} re-computes SHAP values
#'   for child nodes instead of sub-setting the parent cache.
#'
#' @return A nested list representing the tree: level 1 contains the
#'   root (\code{Node}), level 2 the two children, and so on.  Each
#'   element is either a \code{Node} object or \code{NULL} (for pruned
#'   leaves).
#'
#' @section Supported objectives:
#' \describe{
#'   \item{\code{SS_L2_pd}}{Sum-of-squares on centered ICE curves
#'     (partial dependence).}
#'   \item{\code{SS_L2_ale}}{Sum-of-squares on accumulated local effect
#'     derivatives, including numerical stabilization in the tails.}
#'   \item{\code{SS_L2_shap}}{Spline-smoothed residual variance of SHAP
#'     values.}
#' }
#'
#' @export
compute_tree = function(effect, testdata, model,
  predict.function,
  objective = "SS_L2_pd",
  Z,
  target.feature,
  n.split,
  impr.par = 0.1,
  min.split = 10,
  n.quantiles = 100,
  store.data = FALSE,
  shap.recalc = FALSE) {

  if (objective == "SS_L2_pd") {

    split.objective = function(y, x, requires.x = FALSE, split.feat = NULL, y.parent = NULL, grid, ...) {

      if (is.null(split.feat)) {
        y = y
      } else if (split.feat %in% names(y)) {
        y[[split.feat]][, which(!(colnames(y[[split.feat]]) %in% grid))] = NA
        y[[split.feat]] = y[[split.feat]] - rowMeans(y[[split.feat]], na.rm = TRUE)
      }
      if (is.null(y.parent)) {
        L2 = lapply(y, function(feat) {

          ypred = colMeans(as.matrix(feat), na.rm = TRUE)
          sum(t((t(feat) - ypred)^2), na.rm = TRUE)
        })
      }
      else {
        L2 = lapply(names(y), function(feat) {
          ypred = colMeans(as.matrix(y[[feat]]), na.rm = TRUE)
          ypred_parent = colMeans(as.matrix(y.parent[[feat]]), na.rm = TRUE)
          sum(t((t(y[[feat]]) - ypred)^2), na.rm = TRUE) - sum(t((t(y.parent[[feat]]) - ypred_parent)^2), na.rm = TRUE)
        })
      }

      L2
    }

    input.data = compute_data_for_ice_splitting(effect, testdata = testdata, Z = Z)
  }

  else if (objective == "SS_L2_ale") {

    split.objective = function(y, x, requires.x = FALSE, split.feat = NULL, sub.number = NULL, y.parent = NULL, ...) {
      if (is.null(split.feat)) {
        y = y
      } else if (split.feat %in% names(y)) {
        if ((!is.null(sub.number))) {
          if (sub.number == 1 & is.numeric(y[[split.feat]][, feat.val]) & nrow(y[[split.feat]]) > 20) { # adjust when smooth function is not able to learn jumps
            # max_val = ifelse(length(which(y[[split.feat]]$interval.index==max(y[[split.feat]]$interval.index))) == 1, max(y[[split.feat]]$interval.index)-1, max(y[[split.feat]]$interval.index))
            max_fraction = max(round(0.1 * nrow(y[[split.feat]]), 0), 10)
            max_f = sort(y[[split.feat]][, feat.val], decreasing = TRUE)[max_fraction]
            max_val = unique(y[[split.feat]][which(y[[split.feat]][, feat.val] >= max_f), ]$interval.index)
            feat_val_max = which(y[[split.feat]]$interval.index %in% max_val)
            feat_val_not_max = -which(y[[split.feat]]$interval.index %in% max_val)

            if (!is.na(sd(y[[split.feat]]$dL[feat_val_not_max]))) {
              if (sd(y[[split.feat]]$dL[feat_val_max]) > 2 * sd(y[[split.feat]]$dL[feat_val_not_max])) {
                y[[split.feat]]$dL[feat_val_max] = rnorm(n = length(feat_val_max), mean = mean(y[[split.feat]]$dL[feat_val_not_max]), sd = sd(y[[split.feat]]$dL[feat_val_not_max]))
              }
            }
          }
          else if (sub.number == 2 & is.numeric(y[[split.feat]][, feat.val]) & nrow(y[[split.feat]]) > 20) { # adjust when smooth function is not able to learn jumps
            # min_val = ifelse(length(which(y[[split.feat]]$interval.index==min(y[[split.feat]]$interval.index))) == 1, min(y[[split.feat]]$interval.index)+1, min(y[[split.feat]]$interval.index))
            min_fraction = max(round(0.1 * nrow(y[[split.feat]]), 0), 10)
            min_f = sort(y[[split.feat]][, feat.val], decreasing = FALSE)[min_fraction]
            min_val = unique(y[[split.feat]][which(y[[split.feat]][, feat.val] <= min_f), ]$interval.index)
            feat_val_min = which(y[[split.feat]]$interval.index %in% min_val)
            feat_val_not_min = -which(y[[split.feat]]$interval.index %in% min_val)

            if (!is.na(sd(y[[split.feat]]$dL[feat_val_not_min]))) {
              if (sd(y[[split.feat]]$dL[feat_val_min]) > 2 * sd(y[[split.feat]]$dL[feat_val_not_min])) {
                y[[split.feat]]$dL[feat_val_min] = rnorm(n = length(feat_val_min), mean = mean(y[[split.feat]]$dL[feat_val_not_min]), sd = sd(y[[split.feat]]$dL[feat_val_not_min]))
              }
            }

          }
        }

        if (length(unique(y[[split.feat]][, feat.val])) == 1) {
          y[[split.feat]]$dL = 0
        } else if (is.factor(y[[split.feat]][, feat.val])) {

          idx = which(testdata[, split.feat] %in% unique(y[[split.feat]]$feat.val))
          y[[split.feat]] = ale_deriv(model, split.feat, testdata[idx, ], target = "y", h = 10, predict.fun = predict.function)[[1]]
        }

      }


      if (is.null(y.parent)) {
        L2 = lapply(y, function(feat) {

          delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
            interval.n = .N), by = c("interval.index", "x.left", "x.right")]

          df = merge(feat, delta.aggr, by = "interval.index")
          sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE)
        })
      }
      else {
        L2 = lapply(names(y), function(feat) {

          delta.aggr = y[[feat]][, list(dL = mean(dL, na.rm = TRUE),
            interval.n = .N), by = c("interval.index", "x.left", "x.right")]
          delta.aggr.parent = y.parent[[feat]][, list(dL = mean(dL, na.rm = TRUE),
            interval.n = .N), by = c("interval.index", "x.left", "x.right")]

          df = merge(y[[feat]], delta.aggr, by = "interval.index")
          df.parent = merge(y.parent[[feat]], delta.aggr.parent, by = "interval.index")
          sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE) - sum(((df.parent$dL.x - df.parent$dL.y)^2), na.rm = TRUE)
        })
      }
      L2
    }

    input.data = compute_data_for_ale_splitting(effect, testdata = testdata, Z = Z)
  }

  else if (objective == "SS_L2_shap") {

    split.objective = function(y, x, requires.x = FALSE, sub.number = NULL, split.feat = NULL, y.parent = NULL, grid = NULL, ...) {

      if (is.null(split.feat)) {
        y = y
      } else if (split.feat %in% names(y)) {
        pred.fun = predict.function
        idx = which(testdata[, split.feat] %in% unique(y[[split.feat]]$feat.val))
        y[[split.feat]] = fast_shap_values(model, S = split.feat, data = testdata[idx, ], target = "y", nsim = 100, pred.fun = pred.fun)[[1]]
      }

      if (is.null(y.parent)) {
        L2 = lapply(names(y), function(feat_name) {
          feat = y[[feat_name]]

          if (is.numeric(feat$feat.val) & length(unique(feat$feat.val)) > 3) {
            gam_mod = mgcv::gam(phi ~ s(feat.val, k = 3), data = feat)
            sum(gam_mod$residuals^2, na.rm = TRUE)
          }
          else {
            var_interval = feat %>%
              dplyr::group_by(feat.val) %>%
              dplyr::summarise(variance = var(phi, na.rm = TRUE))
            sum(var_interval$variance)
          }

        })
      }
      else {
        L2 = lapply(names(y), function(feat) {
          if (is.numeric(y[[feat]]$feat.val)) {
            gam_mod = mgcv::gam(phi ~ s(feat.val, k = 3), data = y[[feat]])
            gam_mod_parent = mgcv::gam(phi ~ s(feat.val, k = 3), data = y.parent[[feat]])
            sum(gam_mod$residuals^2, na.rm = TRUE) - sum(gam_mod_parent$residuals^2, na.rm = TRUE)
          }
          else {
            var_interval = y[[feat]] %>%
              dplyr::group_by(feat.val) %>%
              dplyr::summarise(variance = var(phi, na.rm = TRUE))
            var_interval_parent = y.parent[[feat]] %>%
              dplyr::group_by(feat.val) %>%
              dplyr::summarise(variance = var(phi, na.rm = TRUE))
            sum(var_interval$variance) - sum(var_interval_parent$variance)
          }

        })
      }
      L2
    }

    input.data = compute_data_for_ale_splitting(effect, testdata = testdata, Z = Z)
  }

  else {
    stop(paste("Objective", objective, "is not supported."))
  }


  # Initialize the parent node of the tree
  parent = Node$new(id = 0, depth = 1, subset.idx = seq_len(nrow(input.data$X)), grid = input.data$grid, improvement.met = FALSE, intImp = 0, store.data = store.data)

  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {

    leaves = tree[[depth]]

    tree[[depth + 1]] = list()

    for (node.idx in seq_along(leaves)) {

      node.to.split = leaves[[node.idx]]

      if (!is.null(node.to.split)) {

        node.to.split$computeSplit(X = input.data$X, Y = input.data$Y, objective = split.objective, method = objective, impr.par = impr.par, optimizer = find_best_binary_split, min.split = min.split, n.quantiles = n.quantiles, grid = input.data$grid)

        node.to.split$computeChildren(X = input.data$X, Y = input.data$Y, testdata = testdata, objective = split.objective, method = objective, model = model, predict.function = predict.function, shap.recalc = shap.recalc)


        tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)
      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL, NULL))
      }
    }
  }

  return(tree)
}
