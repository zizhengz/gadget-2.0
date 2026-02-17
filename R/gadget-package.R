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
#' **Main components (user-facing):**
#' \itemize{
#'   \item \code{\link{gadgetTree}}: R6 class to grow and visualize effect-based trees.
#'   \item \code{\link{aleStrategy}}: Strategy for ALE-based trees (ALE computed internally from a fitted model).
#'   \item \code{\link{pdStrategy}}: Strategy for PD-based trees (uses precomputed ICE/PD from \pkg{iml} or similar tools).
#' }
#'
#' **Typical workflow:**
#' \enumerate{
#'   \item Train a model (e.g., with \pkg{mlr3}).
#'   \item Create a tree: \code{tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 3, min.node.size = 50)}.
#'   \item Fit: \code{tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 20, ...)}\cr
#'         (more control via \code{feature.set}, \code{split.feature}, \code{order.method}, \code{with_stab},\cr
#'         and tree hyperparameters such as \code{impr.par}, \code{min.node.size}, \code{n.quantiles}, etc.).
#'   \item Visualize: \code{tree$plot_tree_structure()}, \code{tree$plot(...)}, \code{tree$extract_split_info()}.
#' }
#'
#' For PD-based trees, pass an effect object from \code{iml::FeatureEffects(..., method = "ice")}
#' to \code{tree$fit(effect = ..., data = ..., target.feature.name = ...)}.
#'
#' @name gadget-package
#' @aliases gadget gadget-package
#' @seealso
#' \code{\link{gadgetTree}}, \code{\link{aleStrategy}}, \code{\link{pdStrategy}}

#' @references
#' Herbinger, J., Wright, M. N., Nagler, T., Bischl, B., and Casalicchio, G. (2024).
#'   Decomposing Global Feature Effects Based on Feature Interactions.
#'   \emph{Journal of Machine Learning Research}, 25(23-0699), 1–65.
#'   URL: \url{https://jmlr.org/papers/volume25/23-0699/23-0699.pdf}.
#'
#' Apley, D.W. and Zhu, J. (2016). Visualizing the Effects of Predictors on the Response
#'   in Nonlinear and Generalized Linear Models. \emph{Journal of Computational and Graphical Statistics},
#'   25(2), 590–600.
"_PACKAGE"
