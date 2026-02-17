skip_ale_cpp_if_unavailable = function() {
  n = 5
  dt = data.table::data.table(row.id = seq_len(n), interval.index = rep(1L, n), dL = 0, int_n = n, int_s1 = 0, int_s2 = 0)
  tryCatch({
    calculate_ale_heterogeneity_list_cpp(list(x = dt))
  }, error = function(e) {
    if (grepl("not available for .Call", conditionMessage(e), fixed = TRUE))
      skip("ALE C++ symbols not loaded (install package with compile)")
  })
}

test_that("calculate_ale returns named list of data.tables", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  set.seed(1)
  n = 40
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  result = calculate_ale(
    model = learner,
    data = data,
    feature.set = c("x1", "x2"),
    target.feature.name = "y",
    n.intervals = 5
  )
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("x1", "x2"))
  for (nm in names(result)) {
    expect_true(data.table::is.data.table(result[[nm]]))
    expect_true(all(c("row.id", "dL", "interval.index") %in% names(result[[nm]])))
  }
})

test_that("calculate_ale_heterogeneity_cpp returns numeric", {
  skip_ale_cpp_if_unavailable()
  n = 20
  dt = data.table::data.table(
    row.id = seq_len(n),
    interval.index = rep(1:4, length.out = n),
    dL = rnorm(n),
    int_n = 5L, int_s1 = 0, int_s2 = 1
  )
  result = calculate_ale_heterogeneity_single_cpp(dt$dL, dt$interval.index)
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_true(!is.na(result))
  expect_true(result >= 0)
})

test_that("calculate_ale_heterogeneity_list_cpp works with list of ALE data", {
  skip_ale_cpp_if_unavailable()
  n = 15
  dt1 = data.table::data.table(
    row.id = seq_len(n), interval.index = rep(1:3, length.out = n),
    dL = rnorm(n), int_n = 5L, int_s1 = 0, int_s2 = 1
  )
  dt2 = data.table::data.table(
    row.id = seq_len(n), interval.index = rep(1:5, length.out = n),
    dL = rnorm(n), int_n = 3L, int_s1 = 0, int_s2 = 1
  )
  Y = list(f1 = dt1, f2 = dt2)
  result = calculate_ale_heterogeneity_list_cpp(Y)
  # C++ returns a named list of scalars, not a numeric vector
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_true(all(vapply(result, is.numeric, logical(1))))
  expect_true(all(!is.na(unlist(result))))
  expect_true(all(unlist(result) >= 0))
})

test_that("ALE tree fit stores root and effect_root", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_ale_cpp_if_unavailable()
  set.seed(2)
  n = 50
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 1, min.node.size = 15)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  expect_true(!is.null(tree$root))
  strat = tree$strategy
  expect_true(!is.null(strat$effect_root))
  expect_true(is.list(strat$effect_root))
})

test_that("prepare_split_data_ale returns Z and Y", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  set.seed(3)
  n = 30
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  result = prepare_split_data_ale(
    model = learner,
    data = data,
    target.feature.name = "y",
    n.intervals = 5,
    split.feature = c("x1", "x2")
  )
  expect_true("Z" %in% names(result))
  expect_true("Y" %in% names(result))
  expect_true(data.table::is.data.table(result$Z))
  expect_true(is.list(result$Y))
})
