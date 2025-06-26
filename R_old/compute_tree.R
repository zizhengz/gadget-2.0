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
  Z = NULL,
  target.feature,
  n.split = 2,
  impr.par = 0.1,
  min.split = 10,
  n.quantiles = 100,
  store.data = FALSE,
  shap.recalc = FALSE) {

  if (objective == "SS_L2_pd") {

    split.objective = function(y, x, requires.x = FALSE, split.feat = NULL, y.parent = NULL, grid, ...) {

      if (is.null(split.feat)) {
        y = y
      } else if (split.feat %in% names(y)) { # feature of interest act as splitting feature
        y[[split.feat]][, which(!(colnames(y[[split.feat]]) %in% grid))] = NA
        y[[split.feat]] = y[[split.feat]] - rowMeans(y[[split.feat]], na.rm = TRUE)
      }
      # y.parent set to be NULL in all case?
      if (is.null(y.parent)) {
        # L2 = lapply(y, function(feat) {
        #   ypred = colMeans(as.matrix(feat), na.rm = TRUE)
        #   sum(t((t(feat) - ypred)^2), na.rm = TRUE)
        # })
        L2 = lapply(y, function(feat) {
          N = nrow(feat)
          # cum_sum_y = Rfast::colCumSums(as.matrix(feat)) # column-wise cumulative sum for x_j ≤ certain split t
          # cum_sum_y2 = Rfast::colCumSums(as.matrix(feat)^2)
          # sum(cum_sum_y2[N,] - cum_sum_y[N,]^2 / N)
          sum_y = Rfast::colsums(as.matrix(feat))
          sum_y2 = Rfast::colsums(as.matrix(feat)^2)
          sum(sum_y2 - sum_y^2 / N)
        })
      } else {
        # L2 = lapply(names(y), function(feat) {
        #   ypred = colMeans(as.matrix(y[[feat]]), na.rm = TRUE)
        #   ypred_parent = colMeans(as.matrix(y.parent[[feat]]), na.rm = TRUE)
        #   sum(t((t(y[[feat]]) - ypred)^2), na.rm = TRUE) - sum(t((t(y.parent[[feat]]) - ypred_parent)^2), na.rm = TRUE)
        # })
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
