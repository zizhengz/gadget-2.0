# GADGET: General Additive Decomposition based on Global Effect Tree

The **gadget** R package implements the GADGET algorithm to interpret machine learning models by detecting and visualizing feature interactions. It decomposes the global effect of features into a tree structure using Accumulated Local Effects (ALE) or Partial Dependence (PD), allowing for a hierarchical understanding of how features interact to influence model predictions.

## Features

- **Interaction Detection**: Automatically identifies feature interactions by recursively splitting data to minimize effect heterogeneity.
- **ALE & PD Support**: Supports both Accumulated Local Effects (ALE) and Partial Dependence (PD) based strategies.
- **Visualization**: Provides tools to visualize the interaction tree structure and regional effects.
- **Extensible Architecture**: Built on R6 classes, making it easy to extend with custom strategies.
- **High Performance**: Core computations are optimized with C++ (Rcpp) for speed.

## Installation

You can install the development version of **gadget** from GitHub:

```r
# install.packages("devtools")
devtools::install_github("zizhengz/gadget-2.0")
```

## Dependencies

- **Core**: `R6`, `ggplot2`, `ggraph`, `igraph`, `patchwork`, `data.table`, `Rcpp` (installed automatically via DESCRIPTION)
- **Recommended for examples**: `mlr3`, `mlr3learners`, `iml`, `ranger`
- **Tip**: Avoid loading the legacy `mlr` package together with `mlr3` in the same R session, as they (and their dependencies) can conflict. When using **gadget**, just load the `mlr3` ecosystem.

## Documentation

- In R: `?gadget` for the package overview; `?gadgetTree`, `?aleStrategy`, `?pdStrategy` for the main API.
- The draft manuscript for the R Journal is in the [`paper/`](paper/) directory (see `paper/README.md` for building).

## Quick Start

Here is a basic example of how to use GADGET with ALE-based trees on the `Bikeshare` data from the `ISLR2` package:

```r
library(gadget)
library(mlr3)
library(mlr3learners)
library(ranger)
library(ISLR2)  # contains the Bikeshare dataset

data("Bikeshare", package = "ISLR2")
set.seed(123)

# Subsample for a lightweight example
bike = Bikeshare[sample(1:nrow(Bikeshare), 1000), ]
bike$workingday = as.factor(bike$workingday)

# Select a small set of features and define the target
bike_data = bike[, c("hr", "temp", "workingday", "bikers")]
names(bike_data)[names(bike_data) == "bikers"] = "target"

task = TaskRegr$new(id = "bike", backend = bike_data, target = "target")
learner = lrn("regr.ranger")
learner$train(task)

# Initialize the GADGET tree with ALE strategy
ale_tree = gadgetTree$new(
  strategy      = aleStrategy$new(),
  n.split       = 2,     # Maximum depth
  impr.par      = 0.01,  # Stopping criterion
  min.node.size = 50     # Minimum samples per node
)

# Fit the tree (ALE effects are computed internally)
ale_tree$fit(
  model = learner,
  data  = bike_data,
  target.feature.name = "target",
  n.intervals = 10
)

# Visualize the tree structure and extract split information
ale_tree$plot_tree_structure()
print(ale_tree$extract_split_info())

# Plot regional ALE effects
ale_tree$plot(
  data              = bike_data,
  target.feature.name = "target",
  mean.center       = FALSE,
  show.point        = TRUE,
  show.plot         = TRUE
)
```

### Partial Dependence (PD) example and plot return value

For PD-based trees we typically use the [`iml`](https://cran.r-project.org/package=iml) package to compute ICE/PD effects, and then let `pdStrategy` use them. The following example is adapted from the synthetic data section in `example.Rmd`:

```r
library(gadget)
library(iml)
library(mlr3)
library(mlr3learners)
library(ranger)

set.seed(1234)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
y  = ifelse(x3 > 0, 3 * x1, -3 * x1) + x3 + rnorm(n, sd = 0.3)
syn.data = data.frame(x1, x2, x3, y)

syn.task    = TaskRegr$new("syn", backend = syn.data, target = "y")
syn.learner = lrn("regr.ranger")
syn.learner$train(syn.task)

syn.predictor = iml::Predictor$new(
  model = syn.learner,
  data  = syn.data[, c("x1", "x2", "x3")],
  y     = syn.data$y
)

# 1. Compute ICE/PD effects for all features
syn.effect = iml::FeatureEffects$new(
  syn.predictor,
  method    = "ice",  # PD is computed as the mean over ICE
  grid.size = 20
)

# 2. Fit a PD-based GADGET tree
syn.tree.pd = gadgetTree$new(
  strategy      = pdStrategy$new(),
  n.split       = 2,
  impr.par      = 0.1,
  min.node.size = 1
)

syn.tree.pd$fit(
  effect = syn.effect,
  data   = syn.data,
  target.feature.name = "y"
)

# 3. Plot tree structure
syn.tree.pd$plot_tree_structure()

# 4. Plot regional PD / ICE and inspect the returned object
plots = syn.tree.pd$plot(
  effect = syn.effect,
  data   = syn.data,
  target.feature.name = "y",
  show.plot   = TRUE,   
  show.point  = TRUE,
  mean.center = TRUE
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

