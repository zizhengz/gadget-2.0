#' Node: Tree Node for Effect-based Decision Trees (R6 class)
#'
#' Represents a single node in an effect-based decision tree, storing split information, effect statistics, and child nodes.
#'
#' @section Fields:
#' \describe{
#'   \item{id}{Integer. Node identifier within its depth level.}
#'   \item{depth}{Integer. Depth of the node (root starts at 1).}
#'   \item{subset.idx}{Integer vector. Row indices of data that fall into this node.}
#'   \item{objective.value.j}{Numeric vector. Objective values for each feature in this node.}
#'   \item{objective.value}{Numeric. Total objective value for this node.}
#'   \item{objective.value.parent}{Numeric. Parent node's objective value.}
#'   \item{grid}{Named list. Grid values for each feature in this node.}
#'   \item{id.parent}{Integer or NULL. Parent node id.}
#'   \item{child.type}{Character. Split direction ("<=", ">", "==", "!=").}
#'   \item{split.feature}{Character. Feature used for splitting this node.}
#'   \item{split.value}{Numeric or factor. Threshold or level used for splitting.}
#'   \item{children}{List. Contains left and right child nodes (or NULL for terminal nodes).}
#'   \item{stop.criterion.met}{Logical. Whether the minimal node size or improvement threshold has been reached.}
#'   \item{improvement.met}{Logical. Whether the improvement threshold was not met.}
#'   \item{intImp.j}{Numeric vector. Interaction importance for each feature.}
#'   \item{intImp}{Numeric. Overall interaction importance for this node.}
#'   \item{intImp.parent}{Numeric. Parent node's interaction importance.}
#'   \item{strategy}{Strategy object. Used for effect-specific operations.}
#'   \item{cache}{List. Stores cached values for fast re-computation.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(id, depth, subset.idx, grid, ...)}{
#'     Constructor for a node.
#'     @param id (integer)\cr
#'       Node identifier.
#'     @param depth (integer or NULL)\cr
#'       Node depth (root is 1).
#'     @param subset.idx (integer vector)\cr
#'       Row indices of data in this node.
#'     @param grid (list)\cr
#'       Grid values for each feature.
#'     @param id.parent (integer or NULL)\cr
#'       Parent node id.
#'     @param child.type (character or NULL)\cr
#'       Split direction.
#'     @param objective.value.parent (numeric or NULL)\cr
#'       Parent node's objective value.
#'     @param objective.value.j (numeric vector or NULL)\cr
#'       Objective values for each feature.
#'     @param objective.value (numeric or NULL)\cr
#'       Total objective value.
#'     @param improvement.met (logical)\cr
#'       Whether improvement threshold was not met.
#'     @param intImp (numeric or NULL)\cr
#'       Interaction importance.
#'     @param intImp.j (numeric vector or NULL)\cr
#'       Interaction importance for each feature.
#'     @param strategy (object or NULL)\cr
#'       Strategy object for effect-specific logic.
#'   }
#'   \item{split_node(Z, Y, objective.value.root.j, objective.value.root, min.node.size, n.quantiles, impr.par)}{
#'     Split the node using the provided data and effect list.
#'     @param Z (data.frame)\cr
#'       Split feature set.
#'     @param Y (list)\cr
#'       Effect list.
#'     @param objective.value.root.j (numeric vector)\cr
#'       Root node's objective values for each feature.
#'     @param objective.value.root (numeric)\cr
#'       Root node's total objective value.
#'     @param min.node.size (integer)\cr
#'       Minimum node size.
#'     @param n.quantiles (integer or NULL)\cr
#'       Number of quantiles for candidate split points.
#'     @param impr.par (numeric)\cr
#'       Improvement threshold parameter.
#'   }
#'   \item{find_best_split(Z, Y.curr, min.node.size, n.quantiles)}{
#'     Find the best split for the node using the effect list.
#'     @param Z (data.frame)\cr
#'       Split feature set.
#'     @param Y.curr (list)\cr
#'       Effect list for current node.
#'     @param min.node.size (integer)\cr
#'       Minimum node size.
#'     @param n.quantiles (integer or NULL)\cr
#'       Number of quantiles for candidate split points.
#'     @return (list or NULL)\cr
#'       Best split information or NULL if no valid split.
#'   }
#'   \item{create_children(Z, Y, split_info, objective.value.root.j, objective.value.root, impr.par)}{
#'     Create left and right child nodes after splitting.
#'     @param Z (data.frame)\cr
#'       Split feature set.
#'     @param Y (list)\cr
#'       Effect list.
#'     @param split_info (list)\cr
#'       Information about the split.
#'     @param objective.value.root.j (numeric vector)\cr
#'       Root node's objective values for each feature.
#'     @param objective.value.root (numeric)\cr
#'       Root node's total objective value.
#'     @param impr.par (numeric)\cr
#'       Improvement threshold parameter.
#'     @return (list)\cr
#'       List containing left and right child nodes and split statistics.
#'   }
#'   \item{create_child_grids(split.feature, split.value, is.categorical)}{
#'     Create grid values for left and right child nodes.
#'     @param split.feature (character)\cr
#'       Feature used for splitting.
#'     @param split.value (numeric or factor)\cr
#'       Value used for splitting.
#'     @param is.categorical (logical)\cr
#'       Whether the split feature is categorical.
#'     @return (list)\cr
#'       List with grid.left and grid.right.
#'   }
#'   \item{apply_split(split_info, children_info)}{
#'     Update node with split and children information.
#'     @param split_info (list)\cr
#'       Information about the split.
#'     @param children_info (list)\cr
#'       Information about the children.
#'   }
#'   \item{get_cached_or_compute(key, compute_func)}{
#'     Retrieve a cached value or compute and cache it.
#'     @param key (character)\cr
#'       Cache key.
#'     @param compute_func (function)\cr
#'       Function to compute the value if not cached.
#'     @return Value from cache or computed.
#'   }
#' }
#'
#' @details
#' This class is used internally by gadgetTree and strategy objects to represent and manage nodes in effect-based decision trees. Each node stores split information, effect statistics, and references to its children.
#'
#' @examples
#' # Example: Creating a Node (typically done internally)
#' # node <- Node$new(id = 1, depth = 1, subset.idx = 1:100, grid = list(feature1 = 1:10))
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric assert_character
#'
#' @keywords internal
Node = R6::R6Class("Node", list(
  id = NULL,
  depth = NULL,

  # idx of the instances of data that are in this node
  subset.idx = NULL,
  objective.value.j = NULL,
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
  intImp.j = NULL,
  intImp = NULL,
  intImp.parent = NULL,
  strategy = NULL,

  # Cache for computed values
  cache = list(),

  initialize = function(id, depth = NULL, subset.idx, grid, id.parent = NULL,
    child.type = NULL, objective.value.parent = NULL, objective.value.j = NULL,
    objective.value = NULL, improvement.met = FALSE, intImp = NULL, intImp.j = NULL, strategy = NULL) {

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
    self$intImp.j = intImp.j
    self$objective.value.parent = objective.value.parent
    self$objective.value = objective.value
    self$objective.value.j = objective.value.j
    self$grid = grid
    self$stop.criterion.met = FALSE
    self$improvement.met = improvement.met
    self$strategy = strategy
  },
  split_node = function(Z, Y,
    objective.value.root.j, objective.value.root,
    min.node.size, n.quantiles, impr.par) {

    checkmate::assert_data_frame(Z)
    checkmate::assert_list(Y)
    if (length(self$subset.idx) < min.node.size | self$improvement.met == TRUE) {
      self$stop.criterion.met = TRUE
      self$children = list("left.child" = NULL, "right.child" = NULL)
      return(NULL)
    }

    t0 = proc.time()

    # Use strategy for node transformation
    Y.curr = self$strategy$node_transform(Y = Y, grid = self$grid, idx = self$subset.idx)

    # Initialize parent split info for root node
    if (self$id == 1) {
      self$split.feature.parent = NA
      self$split.value.parent = NA
      self$intImp.parent = NA
    }

    stop_and_return = function() {
      self$stop.criterion.met = TRUE
      self$children = list("left.child" = NULL, "right.child" = NULL)
      return(NULL)
    }

    split_info = tryCatch({
      self$find_best_split(Z, Y.curr, min.node.size, n.quantiles)
    }, error = function(e) {
      message("find_best_split error: ", e$message)
      return(NULL)
    })
    if (is.null(split_info)) {
      return(stop_and_return())
    }

    children_info = tryCatch({
      self$create_children(Z, Y, split_info, objective.value.root.j, objective.value.root, impr.par)
    }, error = function(e) {
      #message("create_children error: ", e$message)
      return(NULL)
    })
    if (is.null(children_info)) {
      return(stop_and_return())
    }

    # Check improvement threshold
    if (children_info$improvement < children_info$threshold) {
      self$improvement.met = TRUE
      self$children = list("left.child" = NULL, "right.child" = NULL)
    } else {
      self$apply_split(split_info, children_info)
    }

    t1 = proc.time()
    elapsed = as.numeric((t1 - t0)[3])

    # split_benchmark
    if (!is.null(self$strategy) && !is.null(self$strategy$tree_ref)) {
      self$strategy$tree_ref$split_benchmark[[length(self$strategy$tree_ref$split_benchmark) + 1]] =
        list(node_id = self$id, depth = self$depth, time = elapsed)
    }
  },

  # Helper method to find best split
  find_best_split = function(Z, Y.curr, min.node.size, n.quantiles) {
    split.res = self$strategy$find_best_split(Z = Z[self$subset.idx, ], Y = Y.curr,
      min.node.size = min.node.size, n.quantiles = n.quantiles)
    if (is.null(split.res$best.split) || length(split.res$best.split) == 0) {
      return(NULL)
    }
    list(
      split.feature = split.res$split.feature[split.res$best.split][1],
      split.value = split.res$split.point[split.res$best.split][1],
      is.categorical = split.res$is.categorical[split.res$best.split][1]
    )
  },

  # Helper method to create children
  create_children = function(Z, Y, split_info, objective.value.root.j, objective.value.root, impr.par) {
    split.feature = split_info$split.feature
    split.value = split_info$split.value
    is.categorical = split_info$is.categorical
    # Get indices for children
    z.sub = Z[[split.feature]][self$subset.idx]
    if (is.categorical) {
      idx.left = self$subset.idx[which(z.sub == split.value)]
      idx.right = self$subset.idx[which(z.sub != split.value)]
    } else {
      idx.left = self$subset.idx[which(z.sub <= as.numeric(split.value))]
      idx.right = self$subset.idx[which(z.sub > as.numeric(split.value))]
    }

    if (length(idx.left) == 0) idx.left = 0
    if (length(idx.right) == 0) idx.right = 0

    # Create grids for children
    grid_info = self$create_child_grids(split.feature, split.value, is.categorical)

    # Calculate objective values for children
    Y.curr.left = self$strategy$node_transform(Y = Y, grid = grid_info$grid.left, idx = idx.left)
    Y.curr.right = self$strategy$node_transform(Y = Y, grid = grid_info$grid.right, idx = idx.right)
    left.objective.value.j = self$strategy$heterogeneity(Y.curr.left)
    right.objective.value.j = self$strategy$heterogeneity(Y.curr.right)
    left.objective.value = sum(left.objective.value.j, na.rm = TRUE)
    right.objective.value = sum(right.objective.value.j, na.rm = TRUE)

    # Calculate importance
    intImp.j = (self$objective.value.j - left.objective.value.j - right.objective.value.j) / objective.value.root.j
    intImp = (self$objective.value - left.objective.value - right.objective.value) / objective.value.root
    threshold = if (is.null(self$intImp)) impr.par else self$intImp * impr.par

    # Create child nodes
    left.child = Node$new(
      id = 2 * self$id, depth = self$depth + 1,
      subset.idx = idx.left, grid = grid_info$grid.left, id.parent = self$id,
      child.type = if (is.factor(Z[[split.feature]])) "==" else "<=",
      objective.value.parent = self$objective.value,
      objective.value = left.objective.value,
      objective.value.j = left.objective.value.j,
      intImp = intImp, intImp.j = intImp.j,
      improvement.met = self$improvement.met,
      strategy = self$strategy
    )
    right.child = Node$new(
      id = 2 * self$id + 1, depth = self$depth + 1,
      subset.idx = idx.right, grid = grid_info$grid.right, id.parent = self$id,
      child.type = if (is.factor(Z[[split.feature]])) "!=" else ">",
      objective.value.parent = self$objective.value,
      objective.value = right.objective.value,
      objective.value.j = right.objective.value.j,
      intImp = intImp, intImp.j = intImp.j,
      improvement.met = self$improvement.met,
      strategy = self$strategy
    )

    # Set parent info for children
    left.child$split.feature.parent = right.child$split.feature.parent = split.feature
    left.child$split.value.parent = right.child$split.value.parent = split.value
    left.child$intImp.parent = right.child$intImp.parent = intImp

    list(
      left.child = left.child,
      right.child = right.child,
      intImp = intImp,
      intImp.j = intImp.j,
      improvement = intImp,
      threshold = threshold
    )
  },

  # Helper method to create child grids
  create_child_grids = function(split.feature, split.value, is.categorical) {
    grid.left = self$grid
    grid.right = self$grid
    if (split.feature %in% names(self$grid)) {
      if (is.categorical) {
        grid.left.idx = grid.left[[split.feature]] == split.value
        grid.right.idx = grid.right[[split.feature]] != split.value
      } else {
        grid.left.idx = as.numeric(grid.left[[split.feature]]) <= as.numeric(split.value)
        grid.right.idx = as.numeric(grid.right[[split.feature]]) > as.numeric(split.value)
      }
      grid.left[[split.feature]] = grid.left[[split.feature]][grid.left.idx]
      grid.right[[split.feature]] = grid.right[[split.feature]][grid.right.idx]
    }
    list(grid.left = grid.left, grid.right = grid.right)
  },

  # Helper method to apply split
  apply_split = function(split_info, children_info) {
    self$split.feature = split_info$split.feature
    self$split.value = if (split_info$is.categorical) split_info$split.value else as.numeric(split_info$split.value)
    self$intImp = children_info$intImp
    self$intImp.j = children_info$intImp.j
    self$children = list("left.child" = children_info$left.child, "right.child" = children_info$right.child)
  },

  # Helper method to get cached value or compute and cache
  get_cached_or_compute = function(key, compute_func) {
    if (is.null(self$cache[[key]])) {
      self$cache[[key]] = compute_func()
    }
    self$cache[[key]]
  }
))
