#' Node: Tree Node for Effect-based Decision Trees (R6 class)
#'
#' Represents a single node in an effect-based decision tree, storing split information, effect statistics, and child nodes.
#'
#' @field id Integer. Node identifier within its depth level.
#' @field depth Integer. Depth of the node (root starts at 1).
#' @field subset.idx Integer vector. Row indices of data that fall into this node.
#' @field objective.value.j Numeric vector. Objective values for each feature in this node.
#' @field objective.value Numeric. Total objective value for this node.
#' @field objective.value.parent Numeric. Parent node's objective value.
#' @field grid Named list. Grid values for each feature in this node.
#' @field id.parent Integer or NULL. Parent node id.
#' @field child.type Character. Split direction ("<=", ">", "==", "!=").
#' @field split.feature Character. Feature used for splitting this node.
#' @field split.feature.parent Character. Parent node's split feature.
#' @field split.value Numeric or factor. Threshold or level used for splitting.
#' @field split.value.parent Numeric or factor. Parent node's split value.
#' @field children List. Contains left and right child nodes (or NULL for terminal nodes).
#' @field stop.criterion.met Logical. Whether the minimal node size or improvement threshold has been reached.
#' @field improvement.met Logical. Whether the improvement threshold was not met.
#' @field intImp.j Numeric vector. Interaction importance for each feature.
#' @field intImp Numeric. Overall interaction importance for this node.
#' @field intImp.parent Numeric. Parent node's interaction importance.
#' @field strategy Strategy object. Used for effect-specific operations.
#' @field cache List. Stores cached values for fast re-computation.
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
Node = R6::R6Class("Node", public = list(
  id = NULL,
  depth = NULL,
  subset.idx = NULL,
  objective.value.j = NULL,
  objective.value = NULL,
  objective.value.parent = NULL,
  grid = NULL,
  id.parent = NULL,
  child.type = NULL,
  split.feature = NULL,
  split.feature.parent = NULL,
  split.value = NULL,
  split.value.parent = NULL,
  children = NULL,
  stop.criterion.met = NULL,
  improvement.met = NULL,
  intImp.j = NULL,
  intImp = NULL,
  intImp.parent = NULL,
  strategy = NULL,
  cache = NULL,

  #' @description
  #' Constructor for a node.
  #' @param id Integer. Node identifier.
  #' @param depth Integer or NULL. Node depth (root is 1).
  #' @param subset.idx Integer vector. Row indices of data in this node.
  #' @param grid List. Grid values for each feature.
  #' @param id.parent Integer or NULL. Parent node id.
  #' @param child.type Character or NULL. Split direction.
  #' @param objective.value.parent Numeric or NULL. Parent node's objective value.
  #' @param objective.value.j Numeric vector or NULL. Objective values for each feature.
  #' @param objective.value Numeric or NULL. Total objective value.
  #' @param improvement.met Logical. Whether improvement threshold was not met.
  #' @param intImp Numeric or NULL. Interaction importance.
  #' @param intImp.j Numeric vector or NULL. Interaction importance for each feature.
  #' @param strategy Object or NULL. Strategy object for effect-specific logic.
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

  #' @description
  #' Recursively split the node using the provided data and effect list.
  #' @param Z Data frame. Split feature set.
  #' @param Y List. Effect list.
  #' @param objective.value.root.j Numeric vector. Root node's objective values for each feature.
  #' @param objective.value.root Numeric. Root node's total objective value.
  #' @param min.node.size Integer. Minimum node size.
  #' @param n.quantiles Integer or NULL. Number of quantiles for candidate split points.
  #' @param impr.par Numeric. Improvement threshold parameter.
  #' @param depth Integer. Current node depth.
  #' @param max.depth Integer. Maximum allowed tree depth.
  #' @return NULL
  split_node = function(Z, Y, objective.value.root.j, objective.value.root, min.node.size, n.quantiles, impr.par, depth, max.depth) {
    t0 = proc.time()
    # 1. Stopping criteria
    if (depth >= max.depth || length(self$subset.idx) < min.node.size || isTRUE(self$improvement.met)) {
      self$stop.criterion.met = TRUE
      # Recursion exit: stop splitting at this node
      return(NULL)
    }
    # 2. Find the best split
    split.info = tryCatch({
      self$find_best_split(Z, self$strategy$node_transform(Y = Y, grid = self$grid, idx = self$subset.idx),
        min.node.size, n.quantiles)
    }, error = function(e) {
      message("find_best_split error: ", e$message)
      return(NULL)
    })
    if (is.null(split.info)) {
      self$stop.criterion.met = TRUE
      # No valid split found: stop splitting at this node
      return(NULL)
    }
    # 3. Create left and right child nodes
    children.info = tryCatch({
      self$create_children(Z, Y, split.info, objective.value.root.j, objective.value.root, impr.par)
    }, error = function(e) {
      # message("create_children error: ", e$message)
      return(NULL)
    })
    if (is.null(children.info)) {
      self$stop.criterion.met = TRUE
      # Failed to create children: stop splitting at this node
      return(NULL)
    }
    # 4. Apply the split
    self$apply_split(split.info, children.info)
    t1 = proc.time()
    elapsed = as.numeric((t1 - t0)[3])
    # 5. Record split time
    if (!is.null(self$strategy) && !is.null(self$strategy$tree_ref)) {
      self$strategy$tree_ref$split_benchmark[[length(self$strategy$tree_ref$split_benchmark) + 1]] =
        list(node.id = self$id, depth = self$depth, time = elapsed)
    }
    # 6. Recursively split left and right child nodes
    if (!is.null(self$children$left.child)) {
      self$children$left.child$split_node(
        Z, Y,
        objective.value.root.j,
        objective.value.root,
        min.node.size,
        n.quantiles,
        impr.par,
        depth + 1,
        max.depth)
    }
    if (!is.null(self$children$right.child)) {
      self$children$right.child$split_node(
        Z, Y,
        objective.value.root.j,
        objective.value.root,
        min.node.size,
        n.quantiles,
        impr.par,
        depth + 1,
        max.depth)
    }
  },

  #' @description
  #' Find the best split for the node using the effect list.
  #' @param Z Data frame. Split feature set.
  #' @param Y.curr List. Effect list for current node.
  #' @param min.node.size Integer. Minimum node size.
  #' @param n.quantiles Integer or NULL. Number of quantiles for candidate split points.
  #' @return List or NULL. Best split information or NULL if no valid split.
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

  #' @description
  #' Create left and right child nodes after splitting.
  #' @param Z Data frame. Split feature set.
  #' @param Y List. Effect list.
  #' @param split.info List. Information about the split.
  #' @param objective.value.root.j Numeric vector. Root node's objective values for each feature.
  #' @param objective.value.root Numeric. Root node's total objective value.
  #' @param impr.par Numeric. Improvement threshold parameter.
  #' @return List. Contains left and right child nodes and split statistics.
  create_children = function(Z, Y, split.info, objective.value.root.j, objective.value.root, impr.par) {
    split.feature = split.info$split.feature
    split.value = split.info$split.value
    is.categorical = split.info$is.categorical
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
    grid.info = self$create_child_grids(split.feature, split.value, is.categorical)

    # Calculate objective values for children
    Y.curr.left = self$strategy$node_transform(Y = Y, grid = grid.info$grid.left, idx = idx.left)
    Y.curr.right = self$strategy$node_transform(Y = Y, grid = grid.info$grid.right, idx = idx.right)
    left.objective.value.j = self$strategy$heterogeneity(Y.curr.left)
    right.objective.value.j = self$strategy$heterogeneity(Y.curr.right)
    left.objective.value = sum(left.objective.value.j, na.rm = TRUE)
    right.objective.value = sum(right.objective.value.j, na.rm = TRUE)

    # Calculate importance
    intImp.j = (self$objective.value.j - left.objective.value.j - right.objective.value.j) / objective.value.root.j
    intImp = (self$objective.value - left.objective.value - right.objective.value) / objective.value.root

    # threshold for root node: impr.par; for child node: intImp.parent * impr.par
    threshold = if (self$id == 1) impr.par else self$intImp.parent * impr.par

    # Check if improvement meets threshold
    if (intImp < threshold) {
      self$improvement.met = TRUE
      return(NULL) # Improvement not sufficient: stop splitting at this node
    }

    # Create child nodes
    left.child = Node$new(
      id = 2 * self$id, depth = self$depth + 1,
      subset.idx = idx.left, grid = grid.info$grid.left, id.parent = self$id,
      child.type = if (is.factor(Z[[split.feature]])) "==" else "<=",
      objective.value.parent = self$objective.value,
      objective.value = left.objective.value,
      objective.value.j = left.objective.value.j,
      intImp = NULL, intImp.j = NULL,
      improvement.met = self$improvement.met,
      strategy = self$strategy
    )
    right.child = Node$new(
      id = 2 * self$id + 1, depth = self$depth + 1,
      subset.idx = idx.right, grid = grid.info$grid.right, id.parent = self$id,
      child.type = if (is.factor(Z[[split.feature]])) "!=" else ">",
      objective.value.parent = self$objective.value,
      objective.value = right.objective.value,
      objective.value.j = right.objective.value.j,
      intImp = NULL, intImp.j = NULL,
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
      intImp.j = intImp.j
    )
  },

  #' @description
  #' Create grid values for left and right child nodes.
  #' @param split.feature Character. Feature used for splitting.
  #' @param split.value Numeric or factor. Value used for splitting.
  #' @param is.categorical Logical. Whether the split feature is categorical.
  #' @return List. List with grid.left and grid.right.
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

  #' @description
  #' Update node with split and children information.
  #' @param split.info List. Information about the split.
  #' @param children.info List. Information about the children.
  #' @return NULL
  apply_split = function(split.info, children.info) {
    self$split.feature = split.info$split.feature
    self$split.value = if (split.info$is.categorical) split.info$split.value else as.numeric(split.info$split.value)
    self$intImp = children.info$intImp
    self$intImp.j = children.info$intImp.j
    self$children = list("left.child" = children.info$left.child, "right.child" = children.info$right.child)
  },

  #' @description
  #' Retrieve a cached value or compute and cache it.
  #' @param key Character. Cache key.
  #' @param compute_func Function. Function to compute the value if not cached.
  #' @return Value from cache or computed.
  get_cached_or_compute = function(key, compute_func) {
    if (is.null(self$cache[[key]])) {
      self$cache[[key]] = compute_func()
    }
    self$cache[[key]]
  }
))
