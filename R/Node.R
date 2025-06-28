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

  # idx of the instances of data that are in this node
  subset.idx = NULL,
  # objective value in a node
  objective.value = NULL,
  objective.value.parent = NULL,

  # grid values included in node
  grid = NULL,

  id.parent = NULL,
  child.type = NULL, # "==" or "<=" for left; "!=" or ">" for right

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
  # store.data = FALSE,
  # local = NULL,

  initialize = function(id, depth = NULL, subset.idx, grid, id.parent = NULL,
    child.type = NULL, objective.value.parent = NULL, # store.data,
    objective.value = NULL, improvement.met = FALSE, intImp = NULL) {

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
    # self$store.data = store.data
    # self$local = NULL
    self$stop.criterion.met = FALSE
    self$improvement.met = improvement.met
  },
  #### old functions ####
  # computeSplit = function(X, Y, objective.function, effect.method, impr.par, optimizer, min.split,
  #   n.quantiles, grid) {
  #
  #   if (length(self$subset.idx) < min.split | self$improvement.met == TRUE) {
  #     self$stop.criterion.met = TRUE
  #   } else {
  #
  #     if (effect.method == "SS_L2_pd") {
  #       if (self$store.data == TRUE & !is.null(self$local)) {
  #         Y_sub = self$local
  #       } else {
  #         Y_sub = lapply(names(Y), function(feat) {
  #           # subset in this node
  #           Y_j = Y[[feat]][self$subset.idx, ]
  #           # delete grid points not in this node
  #           Y_j[, which(!(colnames(Y_j) %in% self$grid[[feat]]))] = NA
  #           # mean-centering ice curves with less grid points
  #           Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
  #           Y_j = setDT(Y_j)
  #           return(Y_j)
  #         })
  #         names(Y_sub) = names(Y)
  #       }
  #     }
  #
  #     else if (effect.method == c("SS_L2_ale")) {
  #       if (self$store.data == TRUE & !is.null(self$local)) {
  #         Y_sub = self$local
  #       }
  #       else {
  #         Y_sub = lapply(names(Y), function(feat) {
  #           Y_j = Y[[feat]][self$subset.idx, ] # subset in this node
  #           Y_j = setDT(Y_j)
  #           return(Y_j)
  #         })
  #         names(Y_sub) = names(Y)
  #       }
  #
  #     }
  #
  #     else if (effect.method == c("SS_L2_shap")) {
  #       if (self$store.data == TRUE & !is.null(self$local)) {
  #         Y_sub = self$local
  #       }
  #       else {
  #         Y_sub = lapply(names(Y), function(feat) {
  #           Y_j = Y[[feat]][self$subset.idx, ] # subset in this node
  #           Y_j = setDT(Y_j)
  #           return(Y_j)
  #         })
  #         names(Y_sub) = names(Y)
  #       }
  #
  #     }
  #
  #     # we dont need to recalculate it every node
  #     objective.value.root = objective.function(y = Y, x = X, split.feat = NULL)
  #     if (is.null(self$objective.value)) {
  #       self$objective.value.parent = objective.value.root
  #       self$split.feature.parent = NA
  #       self$split.value.parent = NA
  #       self$intImp.parent = NA
  #     }
  #     self$objective.value = objective.function(y = Y_sub, x = X[self$subset.idx, ],
  #       split.feat = self$split.feature,
  #       y.parent = NULL, grid = grid)
  #
  #     tryCatch({
  #       split = split_parent_node(Y = Y_sub, X = X[self$subset.idx, ],
  #         objective = objective.function,
  #         optimizer = find_best_binary_split,
  #         min.node.size = min.split, n.quantiles = n.quantiles,
  #         grid = grid)
  #
  #       if (is.null(self$intImp)) {
  #         self$intImp = 0
  #       }
  #       intImp = (sum(unlist(self$objective.value)) - split$objective.value[split$best.split][1]) / sum(unlist(objective.value.root))
  #
  #       if (self$intImp == 0) {
  #         if (intImp < impr.par) {
  #           self$improvement.met = TRUE
  #         } else {
  #           self$split.feature = split$feature[split$best.split][1]
  #           self$split.value = unlist(split$split.points[split$best.split])[1]
  #           self$intImp = intImp
  #         }
  #       }
  #       else {
  #         if (intImp < self$intImp * impr.par) {
  #           self$improvement.met = TRUE
  #         } else {
  #           self$split.feature = split$feature[split$best.split][1]
  #           self$split.value = unlist(split$split.points[split$best.split])[1]
  #           self$intImp.parent = self$intImp
  #           self$intImp = intImp
  #         }
  #       }
  #     },
  #     error = function(cond) {
  #       # message(paste0("Min.node.size is reached in node ", self$id))
  #       self$stop.criterion.met = TRUE
  #     }
  #     )
  #   }
  # },
  #
  # computeChildren = function(X, Y, testdata, objective, effect.method, model, predict.function, shap.recalc) {
  #   # browser()
  #   if (self$stop.criterion.met | self$improvement.met) {
  #     # no further split is performed
  #     self$children = list("left.child" = NULL, "right.child" = NULL)
  #   } else {
  #     if (is.null(self$split.feature)) {
  #       stop("Please compute the split first via computeSplit().")
  #     }
  #     if (is.factor(X[[self$split.feature]])) {
  #       idx.left = which(X[self$subset.idx, self$split.feature, with = FALSE] == self$split.value)
  #       idx.right = which(X[self$subset.idx, self$split.feature, with = FALSE] != self$split.value)
  #     } else {
  #       idx.left = which(X[self$subset.idx, self$split.feature, with = FALSE] <= self$split.value)
  #       idx.right = which(X[self$subset.idx, self$split.feature, with = FALSE] > self$split.value)
  #     }
  #
  #     idx.left = self$subset.idx[idx.left]
  #     if (length(idx.left) == 0) idx.left = 0
  #     idx.right = self$subset.idx[idx.right]
  #     if (length(idx.right) == 0) idx.right = 0
  #
  #     # grid.left = self$grid
  #     # grid.left[[X[,self$split.feature]]] = grid.left[[X[,self$split.feature]]][as.numeric(grid.left[[X[,self$split.feature]]]) <= self$split.value]
  #     # grid.right = self$grid
  #     # grid.right[[X[,self$split.feature]]] = grid.right[[X[,self$split.feature]]][as.numeric(grid.right[[X[,self$split.feature]]]) > self$split.value]
  #
  #     # Explicitly extract current split feature name
  #     feat_name = self$split.feature
  #     # Copy current grid
  #     grid.left = self$grid
  #     grid.right = self$grid
  #     # Update grid range for left and right child
  #     if (is.factor(X[[self$split.feature]])) {
  #       grid.left[[feat_name]] = grid.left[[feat_name]][grid.left[[feat_name]] == self$split.value]
  #       grid.right[[feat_name]] = grid.right[[feat_name]][grid.right[[feat_name]] != self$split.value]
  #     } else {
  #       grid.left[[feat_name]] = grid.left[[feat_name]][as.numeric(grid.left[[feat_name]]) <= self$split.value]
  #       grid.right[[feat_name]] = grid.right[[feat_name]][as.numeric(grid.right[[feat_name]]) > self$split.value]
  #     }
  #
  #     if (effect.method == "SS_L2_pd") {
  #       Y_left = lapply(names(Y), function(feat) {
  #         # subset in this node
  #         Y_j = Y[[feat]][idx.left, ]
  #         # delete grid points not in this node
  #         Y_j[, which(!(colnames(Y_j) %in% grid.left[[feat]]))] = NA
  #         # mean-centering ice curves with less grid points
  #         Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
  #         Y_j = setDT(Y_j)
  #         return(Y_j)
  #       })
  #       Y_right = lapply(names(Y), function(feat) {
  #         # subset in this node
  #         Y_j = Y[[feat]][idx.right, ]
  #         # delete grid points not in this node
  #         Y_j[, which(!(colnames(Y_j) %in% grid.right[[feat]]))] = NA
  #         # mean-centering ice curves with less grid points
  #         Y_j = Y_j - rowMeans(Y_j, na.rm = TRUE)
  #         Y_j = setDT(Y_j)
  #         return(Y_j)
  #       })
  #       names(Y_left) = names(Y_right) = names(Y)
  #     }
  #
  #     if (effect.method == "SS_L2_ale") {
  #       Y_left = lapply(names(Y), function(feat) {
  #         # browser()
  #         Y_j = Y[[feat]][idx.left, ] # subset in this node
  #         if (feat == self$split.feature & is.numeric(Y_j[, feat.val])) { # adjust when smooth function is not able to learn jumps
  #           # max_val = ifelse(length(which(Y_j$interval.index==max(Y_j$interval.index))) == 1, max(Y_j$interval.index)-1, max(Y_j$interval.index))
  #           max_fraction = max(round(0.1 * nrow(Y_j), 0), 10)
  #           max_f = sort(Y_j[, feat.val], decreasing = TRUE)[max_fraction]
  #           max_val = unique(Y_j[which(Y_j[, feat.val] >= max_f), ]$interval.index)
  #           feat_val_max = which(Y_j$interval.index %in% max_val)
  #           feat_val_not_max = -which(Y_j$interval.index %in% max_val)
  #
  #           if (!is.na(sd(Y_j$dL[feat_val_not_max]))) {
  #             if (sd(Y_j$dL[feat_val_max]) > 2 * sd(Y_j$dL[feat_val_not_max])) {
  #               Y_j$dL[feat_val_max] = rnorm(n = length(feat_val_max), mean = mean(Y_j$dL[feat_val_not_max]), sd = sd(Y_j$dL[feat_val_not_max]))
  #             }
  #           }
  #         }
  #         if (length(unique(Y_j[, feat.val])) == 1) {
  #           Y_j$dL = 0
  #         } else if (feat == self$split.feature & is.factor(Y_j[, feat.val])) {
  #           Y_j = ale_deriv(model, feat, testdata[idx.left, ], "y", 10, predict.fun = predict.function)[[1]]
  #         }
  #
  #         Y_j = setDT(Y_j)
  #         return(Y_j)
  #       })
  #       Y_right = lapply(names(Y), function(feat) {
  #         Y_j = Y[[feat]][idx.right, ] # subset in this node
  #         if (feat == self$split.feature & is.numeric(Y_j[, feat.val])) { # adjust when smooth function is not able to learn jumps
  #           # min_val = ifelse(length(which(Y_j$interval.index==min(Y_j$interval.index))) == 1, min(Y_j$interval.index)+1, min(Y_j$interval.index))
  #           min_fraction = max(round(0.1 * nrow(Y_j), 0), 10)
  #
  #           min_f = sort(Y_j[, feat.val], decreasing = FALSE)[min_fraction]
  #           min_val = unique(Y_j[which(Y_j[, feat.val] <= min_f), ]$interval.index)
  #           feat_val_min = which(Y_j$interval.index %in% min_val)
  #           feat_val_not_min = -which(Y_j$interval.index %in% min_val)
  #           if (!is.na(sd(Y_j$dL[feat_val_not_min]))) {
  #             if (sd(Y_j$dL[feat_val_min]) > 2 * sd(Y_j$dL[feat_val_not_min])) {
  #               Y_j$dL[feat_val_min] = rnorm(n = length(feat_val_min), mean = mean(Y_j$dL[feat_val_not_min]), sd = sd(Y_j$dL[feat_val_not_min]))
  #             }
  #           }
  #         }
  #         if (length(unique(Y_j[, feat.val])) == 1) {
  #           Y_j$dL = 0
  #         } else if (feat == self$split.feature & is.factor(Y_j[, feat.val])) {
  #           Y_j = ale_deriv(model, feat, testdata[idx.right, ], "y", 10, predict.fun = predict.function)[[1]]
  #         }
  #         Y_j = setDT(Y_j)
  #         return(Y_j)
  #       })
  #
  #       names(Y_left) = names(Y_right) = names(Y)
  #     }
  #
  #     if (effect.method == c("SS_L2_shap")) {
  #       if (shap.recalc == TRUE) {
  #         pred.fun = predict.function
  #         Y_left = fast_shap_values(model, S = names(Y), data = testdata[idx.left, ], target = "y", nsim = 500, pred.fun = pred.fun)
  #         Y_right = fast_shap_values(model, S = names(Y), data = testdata[idx.right, ], target = "y", nsim = 500, pred.fun = pred.fun)
  #
  #       }
  #       else {
  #         Y_left = lapply(names(Y), function(feat) {
  #           Y_j = Y[[feat]][idx.left, ] # subset in this node
  #           if (feat == self$split.feature) {
  #             pred.fun = predict.function
  #             Y_j = fast_shap_values(model, S = feat, data = testdata[idx.left, ], target = "y", nsim = 500, pred.fun = pred.fun)[[1]]
  #           }
  #           Y_j = setDT(Y_j)
  #           return(Y_j)
  #         })
  #         Y_right = lapply(names(Y), function(feat) {
  #           Y_j = Y[[feat]][idx.right, ] # subset in this node
  #           if (feat == self$split.feature) {
  #             pred.fun = predict.function
  #             Y_j = fast_shap_values(model, S = feat, data = testdata[idx.right, ], target = "y", nsim = 500, pred.fun = pred.fun)[[1]]
  #           }
  #           Y_j = setDT(Y_j)
  #           return(Y_j)
  #         })
  #         names(Y_left) = names(Y_right) = names(Y)
  #       }
  #     }
  #
  #     obj.left = objective.function(y = Y_left, x = X[idx.left, ], split.feat = self$split.feature, y.parent = NULL, grid = grid.left[[self$split.feature]])
  #     obj.right = objective.function(y = Y_right, x = X[idx.right, ], split.feat = self$split.feature, y.parent = NULL, grid = grid.right[[self$split.feature]])
  #     obj.parent = self$objective.value
  #
  #     if (is.factor(X[[self$split.feature]])) {
  #       left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = "==", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.left, objective.value = obj.left, objective.value.parent = obj.parent, store.data = self$store.data)
  #       right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = "!=", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.right, objective.value = obj.right, objective.value.parent = obj.parent, store.data = self$store.data)
  #     } else {
  #       left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = "<=", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.left, objective.value = obj.left, objective.value.parent = obj.parent, store.data = self$store.data)
  #       right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = ">", improvement.met = self$improvement.met, intImp = self$intImp, grid = grid.right, objective.value = obj.right, objective.value.parent = obj.parent, store.data = self$store.data)
  #     }
  #
  #     # left.child$split.value = right.child$split.value = self$split.value
  #     left.child$split.feature.parent = right.child$split.feature.parent = self$split.feature
  #     left.child$split.value.parent = right.child$split.value.parent = self$split.value
  #
  #     if (self$store.data == TRUE) {
  #       left.child$local = Y_left
  #       right.child$local = Y_right
  #     }
  #
  #     self$children = list("left.child" = left.child, "right.child" = right.child)
  #   }
  # },
  #### new functions ####
  split_node = function(Z, Y, effect.method, objective.value.root,
    min.node.size, n.quantiles, impr.par) {
    checkmate::assert_data_frame(Z)
    checkmate::assert_list(Y)
    if (length(self$subset.idx) < min.node.size | self$improvement.met == TRUE) {
      self$stop.criterion.met = TRUE
    } else {
      if (effect.method == "PD") {
        Y_curr = re_mean_center_PD(Y = Y, idx = self$subset.idx, grid = self$grid)
      }
      if (self$id == 1) {
        self$split.feature.parent = NA
        self$split.value.parent = NA
        self$intImp.parent = NA
      }
      tryCatch({
        split.res = search_best_split(Z = Z[self$subset.idx, ], Y = Y_curr,
          min.node.size = min.node.size, n.quantiles = n.quantiles)

        split.feature = split.res$split.feature[split.res$best.split][1]
        split.value = split.res$split.point[split.res$best.split][1]
        is.categorical = split.res$is.categorical[split.res$best.split][1]

        z_sub = Z[[split.feature]][self$subset.idx]
        if (is.categorical) {
          idx.left = self$subset.idx[which(z_sub == split.value)]
          idx.right = self$subset.idx[which(z_sub != split.value)]
        } else {
          idx.left = self$subset.idx[which(z_sub <= split.value)]
          idx.right = self$subset.idx[which(z_sub > split.value)]
        }
        if (length(idx.left) == 0) idx.left = 0
        if (length(idx.right) == 0) idx.right = 0

        grid.left = self$grid
        grid.right = self$grid
        if (split.feature %in% names(Y)) {
          if (is.categorical) {
            grid.left[[split.feature]] = grid.left[[split.feature]][grid.left[[split.feature]] == split.value]
            grid.right[[split.feature]] = grid.right[[split.feature]][grid.right[[split.feature]] != split.value]
          } else {
            grid.left[[split.feature]] = grid.left[[split.feature]][as.numeric(grid.left[[split.feature]]) <= split.value]
            grid.right[[split.feature]] = grid.right[[split.feature]][as.numeric(grid.right[[split.feature]]) > split.value]
          }
        }

        if (effect.method == "PD") {
          Y_curr_left = re_mean_center_PD(Y = Y, idx = idx.left, grid = grid.left)
          Y_curr_right = re_mean_center_PD(Y = Y, idx = idx.right, grid = grid.right)
        }
        left.objective.value = node_heterogeneity(Y_curr_left)
        right.objective.value = node_heterogeneity(Y_curr_right)
        intImp = (self$objective.value - left.objective.value - right.objective.value) / objective.value.root

        # threshold for root node: impr.par; for child node: intImp(from parent node) * impr.par
        threshold = if (is.null(self$intImp)) impr.par else self$intImp * impr.par
        if (intImp < threshold) {
          self$improvement.met = TRUE
        } else {
          self$split.feature = split.feature
          self$split.value = if (is.categorical) split.value else as.numeric(split.value)
          self$intImp = intImp
        }
        left.child = Node$new(id = 2 * self$id, depth = self$depth + 1,
          subset.idx = idx.left, grid = grid.left, id.parent = self$id,
          child.type = if (is.factor(Z[[self$split.feature]])) "==" else "<=",
          objective.value.parent = self$objective.value, intImp = self$intImp,
          objective.value = left.objective.value, improvement.met = self$improvement.met)
        right.child = Node$new(id = 2 * self$id + 1, depth = self$depth + 1,
          subset.idx = idx.right, grid = grid.right, id.parent = self$id,
          child.type = if (is.factor(Z[[self$split.feature]])) "!=" else ">",
          objective.value.parent = self$objective.value, intImp = self$intImp,
          objective.value = right.objective.value, improvement.met = self$improvement.met)
        left.child$split.feature.parent = right.child$split.feature.parent = self$split.feature
        left.child$split.value.parent = right.child$split.value.parent = self$split.value
        left.child$intImp.parent = right.child$intImp.parent = self$intImp
        self$children = list("left.child" = left.child, "right.child" = right.child)
      },
      error = function(cond) {
        self$stop.criterion.met = TRUE
      }
      )
    }
    if (self$stop.criterion.met | self$improvement.met) {
      self$children = list("left.child" = NULL, "right.child" = NULL)
    }
  }
))
