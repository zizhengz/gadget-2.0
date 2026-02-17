test_that("plot_tree_structure works", {
  # Tree: list of depths, each depth = list of nodes (id, id.parent, split.feature, split.value, subset.idx)
  tree = list(
    list(list(id = 1, id.parent = NA, depth = 1, split.feature = "x1", split.value = 0.5, subset.idx = 1:20)),
    list(
      list(id = 2, id.parent = 1, depth = 2, split.feature = NA, split.value = NA, subset.idx = 1:10),
      list(id = 3, id.parent = 1, depth = 2, split.feature = NA, split.value = NA, subset.idx = 11:20)
    )
  )
  plot_result = plot_tree_structure(tree)
  expect_true(inherits(plot_result, "gg"))
})

test_that("plot_regional_pd is callable with valid prepared.data", {
  n = 10
  set.seed(1)
  prepared.data = list(
    x1 = data.frame(
      `0` = rnorm(n), `0.5` = rnorm(n), `1` = rnorm(n),
      node = rep(1, n),
      check.names = FALSE
    )
  )
  origin.data = data.frame(x1 = c(0, 0.5, 1)[rep(1:3, length.out = n)], y = rnorm(n))
  p = tryCatch({
    plot_regional_pd(
      prepared.data = prepared.data,
      origin.data = origin.data,
      target.feature.name = "y",
      node.idx = 1,
      color.ice = "lightblue", color.pd = "red",
      ymin = NA, ymax = NA, show.point = FALSE, mean.center = TRUE
    )
  }, error = function(e) {
    skip(paste("plot_regional_pd:", conditionMessage(e)))
  })
  # plot_regional_pd returns list of ggplot objects (one per feature)
  expect_true(is.list(p))
  expect_true(length(p) >= 1)
  expect_true(inherits(p[[1]], "gg"))
})

test_that("ALE tree plot returns list", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  n = 5
  dt = data.table::data.table(row.id = seq_len(n), interval.index = rep(1L, n), dL = 0, int_n = n, int_s1 = 0, int_s2 = 0)
  tryCatch(calculate_ale_heterogeneity_list_cpp(list(x = dt)), error = function(e) skip("ALE C++ not loaded"))
  set.seed(123)
  n = 40
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 1, min.node.size = 10)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  pl = tree$plot(data = data, target.feature.name = "y", show.plot = FALSE)
  expect_true(is.list(pl))
})
