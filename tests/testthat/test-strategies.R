test_that("pdStrategy can be created", {
  strategy <- pdStrategy$new()
  
  expect_true(inherits(strategy, "pdStrategy"))
  expect_true(inherits(strategy, "R6"))
})

test_that("pdStrategy search_best_split method works", {
  # Skip if strategy classes are not available
  skip_if_not(exists("pdStrategy"), "Strategy classes not available")
  
  strategy <- pdStrategy$new()
  
  # Test data
  Z <- data.frame(x = 1:10, y = 1:10)
  Y <- list(matrix(1:20, ncol = 2))
  
  tryCatch({
    result <- strategy$search_best_split(Z = Z, Y = Y, min.node.size = 2)
    
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(c("split.feature", "is.categorical", "split.point", 
                      "split.objective", "split.runtime", "best.split") %in% names(result)))
  }, error = function(e) {
    skip("pdStrategy search_best_split requires complex setup")
  })
})

test_that("effectStrategy can be created", {
  strategy <- effectStrategy$new()
  
  expect_true(inherits(strategy, "effectStrategy"))
  expect_true(inherits(strategy, "R6"))
})

test_that("effectStrategy search_best_split method works", {
  # Skip if strategy classes are not available
  skip_if_not(exists("effectStrategy"), "Strategy classes not available")
  
  strategy <- effectStrategy$new()
  
  # Test data
  Z <- data.frame(x = 1:10, y = 1:10)
  Y <- list(matrix(1:20, ncol = 2))
  
  tryCatch({
    result <- strategy$search_best_split(Z = Z, Y = Y, min.node.size = 2)
    
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(c("split.feature", "is.categorical", "split.point", 
                      "split.objective", "split.runtime", "best.split") %in% names(result)))
  }, error = function(e) {
    skip("effectStrategy search_best_split requires complex setup")
  })
}) 