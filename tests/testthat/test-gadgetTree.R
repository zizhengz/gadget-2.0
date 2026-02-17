test_that("gadgetTree can be created", {
  tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 4)

  expect_true(inherits(tree, "gadgetTree"))
  expect_equal(tree$n.split, 4)
  expect_true(inherits(tree$strategy, "pdStrategy"))
})

skip_ale_cpp_if_unavailable = function() {
  n = 5
  dt = data.table::data.table(row.id = seq_len(n), interval.index = rep(1L, n), dL = 0, int_n = n, int_s1 = 0, int_s2 = 0)
  tryCatch({
    calculate_ale_heterogeneity_list_cpp(list(x = dt))
  }, error = function(e) {
    if (grepl("not available for .Call", conditionMessage(e), fixed = TRUE)) {
      skip("ALE C++ symbols not loaded (install package with compile)")
    }
  })
}

test_that("gadgetTree fit with ALE strategy works", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_ale_cpp_if_unavailable()
  set.seed(123)
  n = 80
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 2, min.node.size = 20)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  expect_true(!is.null(tree$root))
  expect_true(inherits(tree$root, "R6"))
})

test_that("gadgetTree plot_tree_structure works after fit", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_ale_cpp_if_unavailable()
  set.seed(456)
  n = 60
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 1, min.node.size = 15)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  p = tree$plot_tree_structure()
  expect_true(inherits(p, "gg"))
})

test_that("gadgetTree extract_split_info returns data frame", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_ale_cpp_if_unavailable()
  set.seed(789)
  n = 60
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 2, min.node.size = 15)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  split_info = tree$extract_split_info()
  expect_true(is.data.frame(split_info))
  expect_true(nrow(split_info) >= 1)
  expect_true("depth" %in% names(split_info))
  expect_true("id" %in% names(split_info))
})

test_that("gadgetTree plot (ALE) returns list of plots", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_ale_cpp_if_unavailable()
  set.seed(101)
  n = 50
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n.split = 1, min.node.size = 15)
  tree$fit(model = learner, data = data, target.feature.name = "y", n.intervals = 5)
  plot_result = tree$plot(data = data, target.feature.name = "y", show.plot = FALSE)
  expect_true(is.list(plot_result))
  # Structure: plot_result[[depth_name]] = list(Node_id = patchwork, ...); get first actual plot
  if (length(plot_result) > 0 && length(plot_result[[1]]) > 0) {
    first_plot = plot_result[[1]][[1]]
    expect_true(inherits(first_plot, "gg") || inherits(first_plot, "patchwork"))
  }
})
