# GADGET: General Additive Decomposition based on Global Effect Tree

GADGET is an R package designed to interpret machine learning models by detecting and visualizing feature interactions. It decomposes the global effect of features into a tree structure using Accumulated Local Effects (ALE) or Partial Dependence (PD), allowing for a hierarchical understanding of how features interact to influence model predictions.

## Features

- **Interaction Detection**: Automatically identifies feature interactions by recursively splitting data to minimize effect heterogeneity.
- **ALE & PD Support**: Supports both Accumulated Local Effects (ALE) and Partial Dependence (PD) based strategies.
- **Visualization**: Provides tools to visualize the interaction tree structure and regional effects.
- **Extensible Architecture**: Built on R6 classes, making it easy to extend with custom strategies.
- **High Performance**: Core computations are optimized with C++ (Rcpp) for speed.

## Installation

You can install the development version of GADGET from GitHub:

```r
# install.packages("devtools")
devtools::install_github("zizhengz/GADGET")
```

## Dependencies

- **Core**: `R6`, `ggplot2`, `ggraph`, `igraph`, `patchwork`, `data.table`, `Rcpp` (installed automatically via DESCRIPTION)
- **Recommended for examples**: `mlr3`, `mlr3learners`, `iml`, `ranger`
- **Tip**: Avoid loading the legacy `mlr` package together with `mlr3` in the same R session, as they (and their dependencies) can conflict. When using **gadget**, just load the `mlr3` ecosystem.

## Documentation

- In R: `?gadget` for the package overview; `?gadgetTree`, `?aleStrategy`, `?pdStrategy` for the main API.
- The draft manuscript for the R Journal is in the [`paper/`](paper/) directory (see `paper/README.md` for building).

## Quick Start

Here is a basic example of how to use GADGET to analyze a machine learning model:

```r
library(gadget)
library(mlr3)
library(mlr3learners)

# 1. Train a model (example using ranger)
task = tsk("bike_sharing")
learner = lrn("regr.ranger")
learner$train(task)
data = task$data()

# 2. Initialize the GADGET tree with ALE strategy
# We want to understand the interaction structure affecting the 'count' target
ale_tree = gadgetTree$new(
  strategy = aleStrategy$new(),
  n.split = 3,           # Maximum depth
  min.node.size = 50     # Minimum samples per node
)

# 3. Fit the tree
# This will compute ALE effects and grow the tree
ale_tree$fit(
  model = learner,
  data = data,
  target.feature.name = "count",
  n.intervals = 20
)

# 4. Visualize the tree structure
ale_tree$plot_tree_structure()

# 5. Inspect split information
print(ale_tree$extract_split_info())
```

### Partial Dependence (PD) example and plot return value

For PD-based trees we typically use the [`iml`](https://cran.r-project.org/package=iml) package to compute ICE/PD effects, and then let `pdStrategy` use them:

```r
library(iml)

# 1. Reuse the trained mlr3 model and data from above
X = as.data.frame(data[, setdiff(names(data), "count")])
y = data$count

predictor = iml::Predictor$new(
  model = learner,
  data  = X,
  y     = y
)

# 2. Compute ICE/PD effects for all features
effect_all = iml::FeatureEffects$new(
  predictor,
  method    = "ice",   # PD is computed as the mean over ICE
  grid.size = 20
)

# 3. Fit a PD-based GADGET tree
pd_tree = gadgetTree$new(
  strategy     = pdStrategy$new(),
  n.split      = 3,
  min.node.size = 50
)

pd_tree$fit(
  effect = effect_all,
  data   = as.data.frame(data),
  target.feature.name = "count"
)

# 4. Plot tree structure
pd_tree$plot_tree_structure()

# 5. Plot regional PD / ICE and inspect the returned object
plots = pd_tree$plot(
  effect = effect_all,
  data   = as.data.frame(data),
  target.feature.name = "count",
  show.plot   = FALSE,   # do not auto-print
  mean.center = FALSE
)

# `plots` is a nested list: depth -> nodes -> ggplot objects
str(plots, max.level = 2)
# For example, first depth, first node:
plots[[1]][[1]]
```

## Methodology

GADGET works by recursively partitioning the feature space. At each step, it seeks a split that maximizes the reduction in "heterogeneity" of the feature effects.
- **Heterogeneity**: A measure of how much a feature's effect varies across different data samples. High heterogeneity suggests the presence of interactions.
- By splitting the data (e.g., `hr < 12` vs `hr >= 12`), GADGET isolates regions where feature effects are more stable, thereby revealing the interaction structure.

## Advanced Usage

### Customizing Strategies

GADGET allows you to specify which features to compute effects for (`feature.set`) and which features to use for splitting (`split.feature`):

```r
ale_tree$fit(
  model = learner,
  data = data,
  target.feature.name = "count",
  n.intervals = 20,
  feature.set = c("temp", "hum"), # Only compute ALE for temp and hum
  split.feature = c("season")     # Only allow splitting by season
)
```

## License

MIT

