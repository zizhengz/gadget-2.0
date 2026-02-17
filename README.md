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
devtools::install_github("yourusername/GADGET")
```

## Note on dependencies

- **mlr3 vs mlr**: Do not load both **mlr3** and the legacy **mlr** package in the same R session; they (and their dependencies paradox vs ParamHelpers) conflict. This package uses **mlr3** only. If you see conflicts, restart R and load only `library(mlr3)` and `library(mlr3learners)` when using GADGET.
- **“Built under R version”**: Warnings that a package was built under R 4.5.2 (or another version) are usually harmless. To reinstall from source for your current R version: `install.packages("pkgname", type = "source")` or use `devtools::install()`.

## Documentation

- In R: `?gadget` for the package overview; `?gadgetTree`, `?aleStrategy`, `?pdStrategy`, `?calculate_ale` for the main API.
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

