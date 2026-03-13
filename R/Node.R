#' Node: Tree Node for Effect-based Decision Trees (R6 class)
#'
#' Represents a single node in an effect-based decision tree, storing split information,
#' effect statistics, and child nodes.
#'
#' @field id Integer. \cr
#'   Node identifier within its depth level.
#' @field depth Integer. \cr
#'   Depth of the node (root starts at 1).
#' @field subset_idx Integer vector. \cr
#'   Row indices of data that fall into this node.
#' @field objective_value_j Numeric vector. \cr
#'   Objective values for each feature in this node.
#' @field objective_value Numeric. \cr
#'   Total objective value for this node.
#' @field objective_value_parent Numeric. \cr
#'   Parent node's objective value.
#' @field grid Named list. \cr
#'   Grid values for each feature in this node.
#' @field id_parent Integer or NULL. \cr
#'   Parent node id.
#' @field child_type Character. \cr
#'   Split direction ("<=", ">", "==", "!=").
#' @field split_feature Character. \cr
#'   Feature used for splitting this node.
#' @field split_feature_parent Character. \cr
#'   Parent node's split feature.
#' @field split_value Numeric or factor. \cr
#'   Threshold or level used for splitting.
#' @field split_value_parent Numeric or factor. \cr
#'   Parent node's split value.
#' @field children List. \cr
#'   Contains left and right child nodes (or NULL for terminal nodes).
#' @field stop_criterion_met Logical. \cr
#'   Whether the minimal node size or improvement threshold has been reached.
#' @field improvement_met Logical. \cr
#'   Whether the improvement threshold was not met.
#' @field intImp_j Numeric vector. \cr
#'   Interaction importance for each feature.
#' @field intImp Numeric. \cr
#'   Overall interaction importance for this node.
#' @field intImp_parent Numeric. \cr
#'   Parent node's interaction importance.
#' @field strategy Strategy object. \cr
#'   Used for effect-specific operations.
#'
#' @details
#' This class is used internally by gadgetTree and strategy objects to represent
#' and manage nodes in effect-based decision trees. Each node stores split information,
#' effect statistics, and references to its children.
#'
#' @examples
#' # Example: Creating a Node (typically done internally)
#' # node <- Node$new(id = 1, depth = 1, subset_idx = 1:100, grid = list(feature1 = 1:10))
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric assert_character
#'
#' @keywords internal
Node = R6::R6Class("Node", public = list(
  id = NULL,
  depth = NULL,
  subset_idx = NULL,
  objective_value_j = NULL,
  objective_value = NULL,
  objective_value_parent = NULL,
  grid = NULL,
  id_parent = NULL,
  child_type = NULL,
  split_feature = NULL,
  split_feature_parent = NULL,
  split_value = NULL,
  split_value_parent = NULL,
  children = NULL,
  stop_criterion_met = NULL,
  improvement_met = NULL,
  intImp_j = NULL,
  intImp = NULL,
  intImp_parent = NULL,
  strategy = NULL,

  #' @description
  #' Create a node from id, depth, subset indices, and grid. Assigns fields and sets \code{stop_criterion_met = FALSE}.
  #' @param id Integer. Node identifier.
  #' @param depth Integer or NULL. Node depth (root is 1).
  #' @param subset_idx Integer vector. Row indices of data in this node.
  #' @param grid List. Grid values for each feature.
  #' @param id_parent Integer or NULL. Parent node id.
  #' @param child_type Character or NULL. Split direction.
  #' @param objective_value_parent Numeric or NULL. Parent node's objective value.
  #' @param objective_value_j Numeric vector or NULL. Objective values for each feature.
  #' @param objective_value Numeric or NULL. Total objective value.
  #' @param improvement_met Logical. Whether improvement threshold was not met.
  #' @param intImp Numeric or NULL. Interaction importance.
  #' @param intImp_j Numeric vector or NULL. Interaction importance for each feature.
  #' @param strategy Object or NULL. Strategy object for effect-specific logic.
  initialize = function(id, depth = NULL, subset_idx, grid, id_parent = NULL,
    child_type = NULL, objective_value_parent = NULL, objective_value_j = NULL,
    objective_value = NULL, improvement_met = FALSE, intImp = NULL, intImp_j = NULL, strategy = NULL) {

    assert_numeric(id, len = 1)
    assert_numeric(depth, len = 1, null.ok = TRUE)
    assert_numeric(subset_idx, min.len = 1)
    assert_numeric(id_parent, len = 1, null.ok = TRUE)
    assert_character(child_type, null.ok = TRUE)

    self$id = id
    self$depth = depth
    self$subset_idx = subset_idx
    self$id_parent = id_parent
    self$child_type = child_type
    self$intImp = intImp
    self$intImp_j = intImp_j
    self$objective_value_parent = objective_value_parent
    self$objective_value = objective_value
    self$objective_value_j = objective_value_j
    self$grid = grid
    self$stop_criterion_met = FALSE
    self$improvement_met = improvement_met
    self$strategy = strategy
  },

  #' @description
  #' Given Z, Y, root objectives, and tree params: checks stopping criteria; finds best split; creates and applies children; recurses into child nodes. 
  #' Returns NULL if no valid split.
  #' @param Z Data frame. Split feature set.
  #' @param Y List. Effect list.
  #' @param objective_value_root_j Numeric vector. Root node's objective values for each feature.
  #' @param objective_value_root Numeric. Root node's total objective value.
  #' @param min_node_size Integer. Minimum node size.
  #' @param n_quantiles Integer or NULL. Number of quantiles for candidate split points.
  #' @param impr_par Numeric. Improvement threshold parameter.
  #' @param depth Integer. Current node depth.
  #' @param max_depth Integer. Maximum allowed tree depth.
  #' @return NULL
  split_node = function(Z, Y, objective_value_root_j, objective_value_root, min_node_size, n_quantiles, impr_par, depth, max_depth) {
    t0 = proc.time()
    # 1. Stopping criteria
    if (objective_value_root < 1e-10 || depth >= max_depth || length(self$subset_idx) < min_node_size || isTRUE(self$improvement_met)) {
      self$stop_criterion_met = TRUE
      # Recursion exit: stop splitting at this node
      return(NULL)
    }
    # 2. Find the best split
    # Find best split with strategy-specific logic
    split_info = tryCatch({
      if (inherits(self$strategy, "aleStrategy")) {
        Y_curr = self$strategy$node_transform(Y, idx = self$subset_idx, split_feature = self$split_feature_parent)
        self$find_best_split(Z, Y_curr, min_node_size, n_quantiles)
      } else if (inherits(self$strategy, "pdStrategy")) {
        Y_curr = self$strategy$node_transform(Y = Y, grid = self$grid, idx = self$subset_idx)
        self$find_best_split(Z, Y_curr, min_node_size, n_quantiles)
      }
    }, error = function(e) {
      message("find_best_split error: ", e$message)
      return(NULL)
    })
    if (is.null(split_info)) {
      self$stop_criterion_met = TRUE
      return(NULL)
    }
    # 3. Create left and right child nodes
    children_info = tryCatch({
      self$create_children(Z, Y, split_info, objective_value_root_j, objective_value_root, impr_par)
    }, error = function(e) {
      message("create_children error: ", e$message)
      return(NULL)
    })
    if (is.null(children_info)) {
      self$stop_criterion_met = TRUE
      return(NULL)
    }
    # 4. Apply the split
    self$apply_split(split_info, children_info)
    t1 = proc.time()
    elapsed = as.numeric((t1 - t0)[3])
    # 5. Record split time
    if (!is.null(self$strategy) && !is.null(self$strategy$tree_ref)) {
      self$strategy$tree_ref$split_benchmark[[length(self$strategy$tree_ref$split_benchmark) + 1]] =
        list(node_id = self$id, depth = self$depth, time = elapsed)
    }
    # 6. Recursively split left and right child nodes
    if (!is.null(self$children$left_child)) {
      self$children$left_child$split_node(
        Z, Y,
        objective_value_root_j,
        objective_value_root,
        min_node_size,
        n_quantiles,
        impr_par,
        depth + 1,
        max_depth)
    }
    if (!is.null(self$children$right_child)) {
      self$children$right_child$split_node(
        Z, Y,
        objective_value_root_j,
        objective_value_root,
        min_node_size,
        n_quantiles,
        impr_par,
        depth + 1,
        max_depth)
    }
  },

  #' @description
  #' Given Z (subset by node indices), Y_curr, and params: calls \code{strategy$find_best_split} and returns list with 
  #' \code{split_feature}, \code{split_value}, \code{is_categorical} (and for aleStrategy: \code{left/right_objective_value_j}).
  #' @param Z Data frame. Split feature set.
  #' @param Y_curr List. Effect list for current node.
  #' @param min_node_size Integer. Minimum node size.
  #' @param n_quantiles Integer or NULL. Number of quantiles for candidate split points.
  #' @return List or NULL. Best split information or NULL if no valid split.
  find_best_split = function(Z, Y_curr, min_node_size, n_quantiles) {
    # Ensure Z subset is always a data.frame(data.table)
    Z_subset = Z[self$subset_idx, ]
    if (!is.data.frame(Z_subset)) {
      Z_subset = data.frame(Z_subset)
      colnames(Z_subset) = colnames(Z)
    }
    split_res = self$strategy$find_best_split(Z = Z_subset, Y = Y_curr,
      min_node_size = min_node_size, n_quantiles = n_quantiles)
    if (is.null(split_res$best_split) || length(split_res$best_split) == 0 || all(!split_res$best_split)) {
      return(NULL)
    }
    if (inherits(self$strategy, "aleStrategy")) {
      rows = which(split_res$best_split)
      left_objective_value_j = split_res$left_objective_value_j[rows]
      right_objective_value_j = split_res$right_objective_value_j[rows]
      return(list(
        split_feature = split_res$split_feature[split_res$best_split][1],
        split_value = split_res$split_point[split_res$best_split][1],
        is_categorical = split_res$is_categorical[split_res$best_split][1],
        left_objective_value_j = left_objective_value_j,
        right_objective_value_j = right_objective_value_j,
        left_objective_value = sum(left_objective_value_j, na.rm = TRUE),
        right_objective_value = sum(right_objective_value_j, na.rm = TRUE)
      ))
    }
    list(
      split_feature = split_res$split_feature[split_res$best_split][1],
      split_value = split_res$split_point[split_res$best_split][1],
      is_categorical = split_res$is_categorical[split_res$best_split][1]
    )
  },

  #' @description
  #' Given Z, Y, split_info, and root objectives: computes idx_left/right, child grids, objective values; checks improvement threshold; 
  #' creates left/right Node instances and sets parent info. 
  #' Returns list of \code{left_child}, \code{right_child}, \code{intImp}, \code{intImp_j} or NULL if improvement too small.
  #' @param Z Data frame. Split feature set.
  #' @param Y List. Effect list.
  #' @param split_info List. Information about the split.
  #' @param objective_value_root_j Numeric vector. Root node's objective values for each feature.
  #' @param objective_value_root Numeric. Root node's total objective value.
  #' @param impr_par Numeric. Improvement threshold parameter.
  #' @return List. Contains left and right child nodes and split statistics.
  create_children = function(Z, Y, split_info, objective_value_root_j, objective_value_root, impr_par) {
    split_feature = split_info$split_feature
    split_value = split_info$split_value
    is_categorical = split_info$is_categorical
    # Get indices for children
    z_sub = Z[[split_feature]][self$subset_idx]
    if (is_categorical) {
      idx_left = self$subset_idx[which(z_sub == split_value)]
      idx_right = self$subset_idx[which(z_sub != split_value)]
    } else {
      idx_left = self$subset_idx[which(z_sub <= as.numeric(split_value))]
      idx_right = self$subset_idx[which(z_sub > as.numeric(split_value))]
    }
    if (length(idx_left) == 0) idx_left = 0
    if (length(idx_right) == 0) idx_right = 0

    # Create grids for children (note: grid_info only effective for pdStrategy)
    grid_info = self$create_child_grids(split_feature, split_value, is_categorical)
    # Calculate objective values for children
    if (inherits(self$strategy, "pdStrategy")) {
      Y_curr_left = self$strategy$node_transform(Y = Y, grid = grid_info$grid_left, idx = idx_left)
      Y_curr_right = self$strategy$node_transform(Y = Y, grid = grid_info$grid_right, idx = idx_right)
      left_objective_value_j = self$strategy$heterogeneity(Y_curr_left)
      right_objective_value_j = self$strategy$heterogeneity(Y_curr_right)
      left_objective_value = sum(left_objective_value_j, na.rm = TRUE)
      right_objective_value = sum(right_objective_value_j, na.rm = TRUE)
      # Calculate interaction importance
      intImp_j = (self$objective_value_j - left_objective_value_j - right_objective_value_j) / objective_value_root_j
      intImp = (self$objective_value - left_objective_value - right_objective_value) / objective_value_root
    } else if (inherits(self$strategy, "aleStrategy")) {
      left_objective_value_j = split_info$left_objective_value_j
      right_objective_value_j = split_info$right_objective_value_j
      left_objective_value = split_info$left_objective_value
      right_objective_value = split_info$right_objective_value
      # Calculate interaction importance
      intImp_j = (self$objective_value_j - left_objective_value_j - right_objective_value_j) / objective_value_root_j
      intImp = (self$objective_value - left_objective_value - right_objective_value) / objective_value_root
    }

    # Threshold for root node: impr_par; for child node: intImp_parent * impr_par
    threshold = if (self$id == 1) impr_par else self$intImp_parent * impr_par
    # Check if improvement meets threshold
    if (intImp < threshold) {
      self$improvement_met = TRUE
      return(NULL) # Improvement not sufficient: stop splitting at this node
    }
    # Create child nodes
    left_child = Node$new(
      id = 2 * self$id, depth = self$depth + 1,
      subset_idx = idx_left, grid = grid_info$grid_left, id_parent = self$id,
      child_type = if (is.factor(Z[[split_feature]])) "==" else "<=",
      objective_value_parent = self$objective_value,
      objective_value = left_objective_value,
      objective_value_j = left_objective_value_j,
      intImp = NULL, intImp_j = NULL,
      improvement_met = self$improvement_met,
      strategy = self$strategy
    )
    right_child = Node$new(
      id = 2 * self$id + 1, depth = self$depth + 1,
      subset_idx = idx_right, grid = grid_info$grid_right, id_parent = self$id,
      child_type = if (is.factor(Z[[split_feature]])) "!=" else ">",
      objective_value_parent = self$objective_value,
      objective_value = right_objective_value,
      objective_value_j = right_objective_value_j,
      intImp = NULL, intImp_j = NULL,
      improvement_met = self$improvement_met,
      strategy = self$strategy
    )
    # Set parent info for children
    left_child$split_feature_parent = right_child$split_feature_parent = split_feature
    left_child$split_value_parent = right_child$split_value_parent = split_value
    left_child$intImp_parent = right_child$intImp_parent = intImp

    list(
      left_child = left_child,
      right_child = right_child,
      intImp = intImp,
      intImp_j = intImp_j
    )
  },

  #' @description
  #' Given split_feature, split_value, and is_categorical: partitions \code{self$grid[[split_feature]]} into left (<= or ==) and right (> or !=). 
  #' Returns list \code{grid_left}, \code{grid_right}.
  #' @param split_feature Character. Feature used for splitting.
  #' @param split_value Numeric or factor. Value used for splitting.
  #' @param is_categorical Logical. Whether the split feature is categorical.
  #' @return List. List with grid_left and grid_right.
  create_child_grids = function(split_feature, split_value, is_categorical) {
    grid_left = self$grid
    grid_right = self$grid
    if (split_feature %in% names(self$grid)) {
      if (is_categorical) {
        grid_left_idx = grid_left[[split_feature]] == split_value
        grid_right_idx = grid_right[[split_feature]] != split_value
      } else {
        grid_left_idx = as.numeric(grid_left[[split_feature]]) <= as.numeric(split_value)
        grid_right_idx = as.numeric(grid_right[[split_feature]]) > as.numeric(split_value)
      }
      grid_left[[split_feature]] = grid_left[[split_feature]][grid_left_idx]
      grid_right[[split_feature]] = grid_right[[split_feature]][grid_right_idx]
    }
    list(grid_left = grid_left, grid_right = grid_right)
  },

  #' @description
  #' Given split_info and children_info: sets \code{split_feature}, \code{split_value}, \code{intImp}, \code{intImp_j}, \code{children}.
  #' @param split_info List. Information about the split.
  #' @param children_info List. Information about the children.
  #' @return NULL
  apply_split = function(split_info, children_info) {
    self$split_feature = split_info$split_feature
    self$split_value = if (split_info$is_categorical) split_info$split_value else as.numeric(split_info$split_value)
    self$intImp = children_info$intImp
    self$intImp_j = children_info$intImp_j
    self$children = list("left_child" = children_info$left_child, "right_child" = children_info$right_child)
  }
))
