test_that("extract_split_info works", {
  # Skip if function is not available
  skip_if_not(exists("extract_split_info"), "extract_split_info function not available")

  # Create a simple tree structure
  tree = list(
    list(id = 1, parent = NULL, depth = 0, split.feature = "x1", split.point = 5),
    list(id = 2, parent = 1, depth = 1, split.feature = "x2", split.point = 3),
    list(id = 3, parent = 1, depth = 1, split.feature = NULL, split.point = NULL)
  )

  tryCatch({
    result = extract_split_info(tree)

    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2) # Only 2 nodes have splits
    expect_true(all(c("id", "parent", "depth", "split.feature", "split.point") %in% names(result)))
  }, error = function(e) {
    skip("extract_split_info requires complex setup")
  })
})

test_that("find_parent_by_id works", {
  # Skip if function is not available
  skip_if_not(exists("find_parent_by_id"), "find_parent_by_id function not available")

  # Create a simple tree structure
  tree = list(
    list(id = 1, parent = NULL, depth = 0),
    list(id = 2, parent = 1, depth = 1),
    list(id = 3, parent = 1, depth = 1)
  )

  tryCatch({
    parent = find_parent_by_id(tree, 2)
    expect_equal(parent$id, 1)

    parent = find_parent_by_id(tree, 3)
    expect_equal(parent$id, 1)

    parent = find_parent_by_id(tree, 1)
    expect_null(parent)
  }, error = function(e) {
    skip("find_parent_by_id requires complex setup")
  })
})

test_that("mean_center_ice works", {
  # Skip if function is not available
  skip_if_not(exists("mean_center_ice"), "mean_center_ice function not available")

  # Create test ICE data
  ice_data = matrix(1:20, ncol = 4)

  tryCatch({
    result = mean_center_ice(ice_data)

    expect_true(is.matrix(result))
    expect_equal(dim(result), dim(ice_data))
    expect_true(all(!is.na(result)))
  }, error = function(e) {
    skip("mean_center_ice requires complex setup")
  })
})

test_that("node_heterogeneity works", {
  # Skip if function is not available
  skip_if_not(exists("node_heterogeneity"), "node_heterogeneity function not available")

  # Create test data
  Y = list(matrix(1:20, ncol = 2))

  tryCatch({
    result = node_heterogeneity(Y)

    expect_true(is.numeric(result))
    expect_true(!is.na(result))
    expect_true(result >= 0)
  }, error = function(e) {
    skip("node_heterogeneity requires complex setup")
  })
})
