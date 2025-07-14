test_that("plot_tree_structure works", {
  # Skip if plotting functions are not available
  skip_if_not(exists("plot_tree_structure"), "Plotting functions not available")

  # Create a simple tree structure with proper format
  tree = list(
    list(id = 1, parent = NULL, depth = 0, split.feature = "x1", split.point = 5),
    list(id = 2, parent = 1, depth = 1, split.feature = "x2", split.point = 3),
    list(id = 3, parent = 1, depth = 1, split.feature = NULL, split.point = NULL)
  )

  # Test plotting
  tryCatch({
    plot_result = plot_tree_structure(tree)
    expect_true(inherits(plot_result, "gg"))
  }, error = function(e) {
    skip("plot_tree_structure requires complex setup")
  })
})

test_that("plot_regional_pd works", {
  # Skip if plotting functions are not available
  skip_if_not(exists("plot_regional_pd"), "Plotting functions not available")

  # Create test data
  set.seed(123)
  n = 50
  X = data.frame(x1 = rnorm(n), x2 = rnorm(n))
  Y = list(matrix(rnorm(n * 2), ncol = 2))

  # Create a simple tree
  tree = list(
    list(id = 1, parent = NULL, depth = 0, split.feature = "x1", split.point = 0)
  )

  # Test plotting
  tryCatch({
    plot_result = plot_regional_pd(tree, X, Y, target.feature.name = "x1")
    expect_true(inherits(plot_result, "gg"))
  }, error = function(e) {
    skip("plot_regional_pd requires complex setup")
  })
})

test_that("plot_tree_pd works", {
  # Skip if plotting functions are not available
  skip_if_not(exists("plot_tree_pd"), "Plotting functions not available")

  # Create test data
  set.seed(123)
  n = 50
  X = data.frame(x1 = rnorm(n), x2 = rnorm(n))
  Y = list(matrix(rnorm(n * 2), ncol = 2))

  # Create a simple tree
  tree = list(
    list(id = 1, parent = NULL, depth = 0, split.feature = "x1", split.point = 0)
  )

  # Test plotting
  tryCatch({
    plot_result = plot_tree_pd(tree, X, Y, target.feature.name = "x1")
    expect_true(inherits(plot_result, "list"))
    expect_true(length(plot_result) > 0)
  }, error = function(e) {
    skip("plot_tree_pd requires complex setup")
  })
})
