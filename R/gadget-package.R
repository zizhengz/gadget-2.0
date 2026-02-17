#' gadget: General Additive Decomposition Based on Global Effect Tree
#'
#' @description
#' The **gadget** package implements the GADGET algorithm for interpretable
#' machine learning. It builds a tree by recursively partitioning the feature
#' space to minimize the heterogeneity of feature effects (e.g., Accumulated
#' Local Effects or Partial Dependence), so that within each region the
#' effects are more stable and easier to interpret.
#'
#' @details
#' **Main components:**
#' \itemize{
#'   \item \code{\link{gadgetTree}}: R6 class to grow and visualize effect-based trees.
#'   \item \code{\link{aleStrategy}}: Strategy for ALE-based trees (ALE computed internally).
#'   \item \code{\link{pdStrategy}}: Strategy for PD-based trees (uses precomputed ICE/PD from \pkg{iml}).
#'   \item \code{\link{calculate_ale}}: Compute ALE effects for a fitted model and data.
#' }
#'
#' **Typical workflow:**
#' \enumerate{
#'   \item Train a model (e.g., with \pkg{mlr3}).
#'   \item Create a tree: \code{tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 3, min.node.size = 50)}.
#'   \item Fit: \code{tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 20)}.
#'   \item Visualize: \code{tree$plot_tree_structure()}, \code{tree$plot(...)}, \code{tree$extract_split_info()}.
#' }
#'
#' For PD-based trees, pass an effect object from \code{iml::FeatureEffects(..., method = "ice")}
#' to \code{tree$fit(effect = ..., data = ..., target.feature.name = ...)}.
#'
#' @name gadget-package
#' @aliases gadget gadget-package
#' @seealso
#' \code{\link{gadgetTree}}, \code{\link{aleStrategy}}, \code{\link{pdStrategy}},
#' \code{\link{calculate_ale}}
#' @references
#' Apley, D.W. and Zhu, J. (2016). Visualizing the Effects of Predictors on the Response
#' in Nonlinear and Generalized Linear Models. \emph{Journal of Computational and Graphical Statistics},
#' 25(2), 590–600.
"_PACKAGE"
