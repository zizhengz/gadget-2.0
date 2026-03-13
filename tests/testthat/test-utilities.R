test_that("convert_tree_to_list returns depth-based list", {
  skip_if_not(exists("convert_tree_to_list"), "convert_tree_to_list not available")
  # Build a minimal root node (no children)
  grid = list(x = 1:5)
  root = Node$new(
    id = 1, depth = 1, subset_idx = 1:10, grid = grid,
    id_parent = NULL, child_type = NULL
  )
  tree_list = convert_tree_to_list(root, max_depth = 2)
  expect_true(is.list(tree_list))
  expect_true(length(tree_list) >= 1)
  expect_true(is.list(tree_list[[1]]))
  expect_equal(length(tree_list[[1]]), 1)
  expect_equal(tree_list[[1]][[1]]$id, 1)
})

test_that("extract_split_info works with depth-based tree list", {
  # Tree: list of depths, each depth is list of nodes (same structure as convert_tree_to_list output)
  node1 = list(
    id = 1, depth = 1, subset_idx = 1:20,
    split_feature = "x", split_value = 0.5,
    objective_value = 1.5, intImp = NA, intImp_j = NULL,
    split_feature_parent = NA_character_, split_value_parent = NA,
    objective_value_parent = NA, intImp_parent = NA,
    improvement_met = FALSE, stop_criterion_met = FALSE,
    children = list(left_child = "dummy", right_child = "dummy")
  )
  node2 = list(
    id = 2, depth = 2, subset_idx = 1:10,
    split_feature = NA_character_, split_value = NA,
    objective_value = 0.5, intImp = 0.2, intImp_j = c(x = 0.2),
    split_feature_parent = "x", split_value_parent = 0.5,
    objective_value_parent = 1.5, intImp_parent = NA,
    improvement_met = FALSE, stop_criterion_met = TRUE,
    children = NULL
  )
  node3 = list(
    id = 3, depth = 2, subset_idx = 11:20,
    split_feature = NA_character_, split_value = NA,
    objective_value = 0.6, intImp = 0.3, intImp_j = c(x = 0.3),
    split_feature_parent = "x", split_value_parent = 0.5,
    objective_value_parent = 1.5, intImp_parent = NA,
    improvement_met = FALSE, stop_criterion_met = TRUE,
    children = NULL
  )
  tree = list(list(node1), list(node2, node3))
  result = extract_split_info(tree)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("depth", "id", "split_feature", "n_obs") %in% names(result)))
})

test_that("find_parent_by_id finds node by id in flat node list", {
  node1 = list(id = 1, depth = 1)
  node2 = list(id = 2, id_parent = 1, depth = 2)
  node3 = list(id = 3, id_parent = 1, depth = 2)
  node_list = list(node1, node2, node3)
  expect_equal(find_parent_by_id(node_list, 2)$id, 2)
  expect_equal(find_parent_by_id(node_list, 1)$id, 1)
  expect_null(find_parent_by_id(node_list, 99))
})

test_that("node_heterogeneity returns non-negative numeric vector", {
  Y = list(
    matrix(rnorm(20), ncol = 2),
    matrix(rnorm(20), ncol = 2)
  )
  result = node_heterogeneity(Y)
  expect_true(is.numeric(result))
  expect_length(result, 2)
  expect_true(all(!is.na(result)))
  expect_true(all(result >= 0))
})

test_that("order_categorical_levels returns factor with ordered levels", {
  data = data.frame(
    cat = factor(rep(letters[1:3], each = 5)),
    x = rnorm(15),
    y = rnorm(15)
  )
  x_cat = droplevels(data$cat)
  result = order_categorical_levels(x_cat, data, feature = "cat", target_feature_name = "y", order_method = "raw")
  expect_true(is.factor(result))
  expect_equal(length(result), length(x_cat))
  expect_true(all(levels(result) %in% levels(x_cat)))
  result_mds = order_categorical_levels(x_cat, data, feature = "cat", target_feature_name = "y", order_method = "mds")
  expect_true(is.factor(result_mds))
  expect_equal(length(levels(result_mds)), nlevels(x_cat))
})

test_that("mean_center_ice returns Y and grid from effect with results", {
  set.seed(1)
  effect = list(results = data.frame(
    x = rep(1:3, each = 2),
    .value = rnorm(6),
    .type = "ice",
    .id = rep(1:2, 3)
  ))
  result = mean_center_ice(effect, feature_set = NULL, mean_center = TRUE)
  expect_true(is.list(result))
  expect_true("Y" %in% names(result))
  expect_true("grid" %in% names(result))
  expect_true(is.list(result$Y))
  expect_true(is.list(result$grid))
})
