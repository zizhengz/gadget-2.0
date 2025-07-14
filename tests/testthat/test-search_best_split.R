test_that("search_best_split_cpp works with numeric data", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  # Test with simple numeric data
  test_data = data.frame(x = 1:10, y = 1:10)
  Y = list(matrix(1:20, ncol = 2))

  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 2)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 6)
  expect_true(all(c("split.feature", "is.categorical", "split.point",
    "split.objective", "split.runtime", "best.split") %in% names(result)))
  expect_true(all(result$is.categorical == FALSE))
  expect_true(all(!is.na(result$split.point)))
  expect_true(all(!is.infinite(result$split.objective)))
})

test_that("search_best_split_cpp works with mixed data types", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  # Test with numeric and categorical data
  test_data = data.frame(x = 1:10, y = factor(letters[1:10]))
  Y = list(matrix(1:20, ncol = 2))

  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 2)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(result$is.categorical[1] == FALSE) # x should be numeric
  expect_true(result$is.categorical[2] == TRUE) # y should be categorical
})

test_that("search_best_split_cpp handles minimum node size constraint", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  test_data = data.frame(x = 1:5) # Small dataset
  Y = list(matrix(1:10, ncol = 2))

  # With min_node_size = 3, should still find splits
  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 3)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)

  # With min_node_size = 5, should not find valid splits
  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 5)
  expect_true(is.data.frame(result))
  expect_true(all(is.infinite(result$split.objective)))
})

test_that("search_best_split_cpp handles multiple Y matrices", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  test_data = data.frame(x = 1:10)
  Y = list(matrix(1:20, ncol = 2), matrix(21:40, ncol = 2))

  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 2)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true(!is.infinite(result$split.objective[1]))
})

test_that("search_best_split_cpp handles edge cases", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  # Test with single column
  test_data = data.frame(x = 1:10)
  Y = list(matrix(1:20, ncol = 2))

  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 2)
  expect_equal(nrow(result), 1)

  # Test with all identical values
  test_data = data.frame(x = rep(5, 10))
  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 2)
  expect_true(is.infinite(result$split.objective[1]))
})

test_that("search_best_split_cpp performance test", {
  # Load C++ dynamic library for testing
  tryCatch({
    dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gadget/libs/gadget.so")
  }, error = function(e) {
    skip("C++ dynamic library not available")
  })

  # Create larger dataset for performance test
  set.seed(123)
  n = 500
  test_data = data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = factor(sample(letters[1:3], n, replace = TRUE))
  )
  Y = list(matrix(rnorm(n * 2), ncol = 2))

  # Test performance
  start_time = Sys.time()
  result = search_best_split_cpp(Z = test_data, Y = Y, min_node_size = 10)
  end_time = Sys.time()

  runtime = as.numeric(end_time - start_time)

  # Performance expectations
  expect_true(runtime < 0.1) # Should complete in less than 0.1 seconds
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3) # 3 features
  expect_true(all(c("split.feature", "is.categorical", "split.point",
    "split.objective", "split.runtime", "best.split") %in% names(result)))
})
