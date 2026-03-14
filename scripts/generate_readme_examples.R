# Generate README example outputs
# Run: Rscript scripts/generate_readme_examples.R

options(warn = -1)
dir.create("figures", showWarnings = FALSE)

library(gadget)
library(mlr3)
library(mlr3learners)
library(ISLR2)

# ---- ALE + Bike ----
cat("\n=== ALE + Bike ===\n")
set.seed(123)
bike = Bikeshare[sample(seq_len(nrow(Bikeshare)), 1000), ]
bike$workingday = as.factor(bike$workingday)
bike_data = bike[, c("hr", "temp", "workingday", "bikers")]
names(bike_data)[names(bike_data) == "bikers"] = "target"

task = TaskRegr$new(id = "bike", backend = bike_data, target = "target")
learner = lrn("regr.ranger")
learner$train(task)

tree_ale_bike = GadgetTree$new(
  strategy = AleStrategy$new(),
  n_split = 2,
  impr_par = 0.01,
  min_node_size = 50
)
tree_ale_bike$fit(
  data = bike_data,
  target_feature_name = "target",
  model = learner,
  n_intervals = 10
)

split_ale_bike = tree_ale_bike$extract_split_info()
cat("Split info (ALE Bike):\n")
print(split_ale_bike)

pdf("figures/ale_bike_tree.pdf", width = 6, height = 4)
tree_ale_bike$plot_tree_structure()
dev.off()

pl_ale_bike = tree_ale_bike$plot(
  data = bike_data,
  target_feature_name = "target",
  mean_center = TRUE,
  show_plot = FALSE
)
if (length(pl_ale_bike) > 0 && length(pl_ale_bike[[1]]) > 0) {
  pdf("figures/ale_bike_effects.pdf", width = 8, height = 5)
  print(pl_ale_bike[[1]][[1]])
  dev.off()
}

# ---- ALE + Synthetic ----
cat("\n=== ALE + Synthetic ===\n")
set.seed(1234)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
y = ifelse(x3 > 0, 3 * x1, -3 * x1) + x3 + rnorm(n, sd = 0.3)
syn_data = data.frame(x1, x2, x3, y)

task_syn = TaskRegr$new("syn", backend = syn_data, target = "y")
learner_syn = lrn("regr.ranger")
learner_syn$train(task_syn)

tree_ale_syn = GadgetTree$new(strategy = AleStrategy$new(), n_split = 2, min_node_size = 10)
tree_ale_syn$fit(model = learner_syn, data = syn_data, target_feature_name = "y", n_intervals = 10)

split_ale_syn = tree_ale_syn$extract_split_info()
cat("Split info (ALE Syn):\n")
print(split_ale_syn)

pdf("figures/ale_syn_tree.pdf", width = 6, height = 4)
tree_ale_syn$plot_tree_structure()
dev.off()

pl_ale_syn = tree_ale_syn$plot(data = syn_data, target_feature_name = "y", show_plot = FALSE)
if (length(pl_ale_syn) > 0 && length(pl_ale_syn[[1]]) > 0) {
  pdf("figures/ale_syn_effects.pdf", width = 8, height = 5)
  print(pl_ale_syn[[1]][[1]])
  dev.off()
}

# ---- PD + Bike (requires iml) ----
if (requireNamespace("iml", quietly = TRUE)) {
  cat("\n=== PD + Bike ===\n")
  library(iml)
  bike_x = bike_data[, c("hr", "temp", "workingday")]
  bike_y = bike_data$target
  predictor_bike = iml::Predictor$new(model = learner, data = bike_x, y = bike_y)
  effect_bike = iml::FeatureEffects$new(predictor_bike, method = "ice", grid.size = 20)

  tree_pd_bike = GadgetTree$new(strategy = PdStrategy$new(), n_split = 2, min_node_size = 50)
  tree_pd_bike$fit(data = bike_data, target_feature_name = "target", effect = effect_bike)

  split_pd_bike = tree_pd_bike$extract_split_info()
  cat("Split info (PD Bike):\n")
  print(split_pd_bike)

  pdf("figures/pd_bike_tree.pdf", width = 6, height = 4)
  tree_pd_bike$plot_tree_structure()
  dev.off()

  pl_pd_bike = tree_pd_bike$plot(
    effect = effect_bike,
    data = bike_data,
    target_feature_name = "target",
    features = c("hr", "temp"),
    show_plot = FALSE
  )
  if (length(pl_pd_bike) > 0 && length(pl_pd_bike[[1]]) > 0) {
    pdf("figures/pd_bike_effects.pdf", width = 8, height = 5)
    print(pl_pd_bike[[1]][[1]])
    dev.off()
  }

  # ---- PD + Synthetic ----
  cat("\n=== PD + Synthetic ===\n")
  predictor_syn = iml::Predictor$new(
    model = learner_syn,
    data = syn_data[, c("x1", "x2", "x3")],
    y = syn_data$y
  )
  effect_syn = iml::FeatureEffects$new(predictor_syn, method = "ice", grid.size = 20)

  tree_pd_syn = GadgetTree$new(strategy = PdStrategy$new(), n_split = 2, min_node_size = 10)
  tree_pd_syn$fit(data = syn_data, target_feature_name = "y", effect = effect_syn)

  split_pd_syn = tree_pd_syn$extract_split_info()
  cat("Split info (PD Syn):\n")
  print(split_pd_syn)

  pdf("figures/pd_syn_tree.pdf", width = 6, height = 4)
  tree_pd_syn$plot_tree_structure()
  dev.off()

  pl_pd_syn = tree_pd_syn$plot(
    effect = effect_syn,
    data = syn_data,
    target_feature_name = "y",
    show_plot = FALSE
  )
  if (length(pl_pd_syn) > 0 && length(pl_pd_syn[[1]]) > 0) {
    pdf("figures/pd_syn_effects.pdf", width = 8, height = 5)
    print(pl_pd_syn[[1]][[1]])
    dev.off()
  }

  # Save split info as text for README
  sink("figures/split_info_ale_bike.txt")
  print(split_ale_bike)
  sink()
  sink("figures/split_info_ale_syn.txt")
  print(split_ale_syn)
  sink()
  sink("figures/split_info_pd_bike.txt")
  print(split_pd_bike)
  sink()
  sink("figures/split_info_pd_syn.txt")
  print(split_pd_syn)
  sink()
} else {
  cat("iml not installed, skipping PD examples\n")
}

cat("\nDone. Outputs in figures/\n")
