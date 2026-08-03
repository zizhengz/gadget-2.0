### Shared imports and helpers for the 26-07-13 selective-early-stopping examples.

library(xplaineff)
library(iml)
library(mlr3)
library(mlr3learners)
library(ranger)

# Train a ranger model on some given data (target "y") and return its ICE feature effects.
# `test_data_size` rows are held out (as an absolute count) purely to report an out-of-sample
# fit; the model is trained on the remaining rows, while the returned ICE effects still use the
# full data as reference (matching the data the GADGET tree is fitted on downstream).
make_effect = function(dat, grid.size = 50, test_data_size = 200) {
  features = setdiff(colnames(dat), "y")
  test_idx = sample(nrow(dat), size = min(test_data_size, nrow(dat) / 2))
  train_data = dat[-test_idx, ]
  test_data = dat[test_idx, ]
  task = TaskRegr$new("syn", backend = train_data, target = "y")
  learner = lrn("regr.ranger")
  learner$train(task)
  prediction_test = learner$predict_newdata(test_data)$response
  rmse = sqrt(mean((test_data$y - prediction_test)^2))
  r_squared = 1 - sum((test_data$y - prediction_test)^2) /
    sum((test_data$y - mean(test_data$y))^2)
  cat(sprintf("  model hold-out (n_test = %d): RMSE = %.3f, R2 = %.3f\n",
    nrow(test_data), rmse, r_squared))
  predictor = Predictor$new(learner, data = dat[, features], y = dat$y)
  FeatureEffects$new(predictor, grid.size = grid.size, method = "ice")
}

# Fit a PD GADGET tree, optionally with selective early stopping.
fit_tree = function(dat, effect, gadget_improvements = NULL,
  tau = NULL, n_split = 4, impr_par = 0.05, min_node_size = 30) {
  tree = GadgetTree$new(strategy = PdStrategy$new(), n_split = n_split,
    impr_par = impr_par, min_node_size = min_node_size)
  tree$fit(data = dat, target_feature_name = "y", effect = effect,
    gadget_improvements = gadget_improvements,
    gadget_impr_args = if (is.null(tau)) NULL else list(tau = tau))
  tree
}

# Walk the tree, printing each node's still-interacting features (vecb_remaining_features).
walk_remaining = function(node, prefix = "root") {
  if (is.null(node)) return(invisible())
  remaining_features = node$vecb_remaining_features
  # This is null in case selective early stopping is disabled.
  tag = if (is.null(remaining_features)) "NULL (disabled)" else
    paste(names(remaining_features)[remaining_features], collapse = ",")
  cat(sprintf("  %-9s id=%-2d depth=%d  remaining: %s\n", prefix, node$id, node$depth, tag))
  if (!is.null(node$children)) {
    walk_remaining(node$children$left_child, paste0(prefix, ".L"))
    walk_remaining(node$children$right_child, paste0(prefix, ".R"))
  }
}

split_cols = c("depth", "id", "node_type", "split_feature", "split_value", "int_imp")
show_tree = function(tree) print(tree$extract_split_info()[, split_cols])

# Selective early stopping methods (Section 5.1), Methods 1-4.
methods = c("plain_risk", "risk_reduction", "interaction_fraction", "interaction_fraction_total")
