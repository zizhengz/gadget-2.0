test_that("pdStrategy can be created", {
  strategy = pdStrategy$new()
  expect_true(inherits(strategy, "pdStrategy"))
  expect_true(inherits(strategy, "effectStrategy"))
  expect_equal(strategy$name, "pd")
})

test_that("pdStrategy find_best_split returns expected structure", {
  tryCatch({
    search_best_split_cpp(Z = data.frame(x = 1:5), Y = list(matrix(1:10, ncol = 2)), min_node_size = 2)
  }, error = function(e) {
    if (grepl("not available for .Call", conditionMessage(e), fixed = TRUE))
      skip("C++ symbols not loaded (install package with compile)")
  })
  set.seed(1)
  n = 30
  Z = data.frame(x = runif(n), y = runif(n))
  Y = list(
    matrix(rnorm(n * 2), ncol = 2),
    matrix(rnorm(n * 2), ncol = 2)
  )
  strategy = pdStrategy$new()
  res = strategy$find_best_split(Z = Z, Y = Y, min.node.size = 5, n.quantiles = NULL)
  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 1)
  expect_true(all(c("split.feature", "is.categorical", "split.point",
    "split.objective", "split.runtime", "best.split") %in% names(res)))
})

test_that("pdStrategy heterogeneity returns numeric vector", {
  Y = list(matrix(rnorm(20), ncol = 2), matrix(rnorm(20), ncol = 2))
  strategy = pdStrategy$new()
  h = strategy$heterogeneity(Y)
  expect_true(is.numeric(h))
  expect_length(h, 2)
  expect_true(all(!is.na(h)))
  expect_true(all(h >= 0))
})

test_that("aleStrategy can be created", {
  strategy = aleStrategy$new()
  expect_true(inherits(strategy, "aleStrategy"))
  expect_true(inherits(strategy, "effectStrategy"))
  expect_equal(strategy$name, "ale")
})

test_that("aleStrategy heterogeneity returns numeric for ALE-like list", {
  tryCatch({
    dt = data.table::data.table(row.id = 1:5, interval.index = rep(1L, 5), dL = 0, int_n = 5L, int_s1 = 0, int_s2 = 0)
    calculate_ale_heterogeneity_list_cpp(list(x = dt))
  }, error = function(e) {
    if (grepl("not available for .Call", conditionMessage(e), fixed = TRUE))
      skip("ALE C++ symbols not loaded (install package with compile)")
  })
  n = 20
  dt = data.table::data.table(
    row.id = seq_len(n),
    interval.index = rep(1:4, length.out = n),
    dL = rnorm(n),
    int_n = 5L, int_s1 = 0, int_s2 = 1
  )
  Y = list(f1 = dt)
  strategy = aleStrategy$new()
  h = strategy$heterogeneity(Y)
  expect_true(is.numeric(h))
  expect_length(h, 1)
  expect_true(!is.na(h))
  expect_true(h >= 0)
})

test_that("effectStrategy can be created (abstract)", {
  strategy = effectStrategy$new()
  expect_true(inherits(strategy, "effectStrategy"))
  expect_true(inherits(strategy, "R6"))
})

test_that("effectStrategy find_best_split throws (not implemented)", {
  strategy = effectStrategy$new()
  expect_error(
    strategy$find_best_split(Z = data.frame(x = 1:5), Y = list(matrix(1:10, ncol = 2)),
      min.node.size = 1, n.quantiles = NULL),
    "Not implemented"
  )
})
