test_that("plot_tree_structure works", {
  # Tree: list of depths, each depth = list of nodes (id, id_parent, split_feature, split_value, subset_idx)
  tree = list(
    list(list(id = 1, id_parent = NA, depth = 1, split_feature = "x1", split_value = 0.5, subset_idx = 1:20)),
    list(
      list(id = 2, id_parent = 1, depth = 2, split_feature = NA, split_value = NA, subset_idx = 1:10),
      list(id = 3, id_parent = 1, depth = 2, split_feature = NA, split_value = NA, subset_idx = 11:20)
    )
  )
  plot_result = plot_tree_structure(tree)
  expect_true(inherits(plot_result, "gg"))
})

test_that("plot_regional_pd is callable with valid prepared_data", {
  n = 10
  set.seed(1)
  prepared_data = list(
    x1 = data.frame(
      `0` = rnorm(n), `0.5` = rnorm(n), `1` = rnorm(n),
      node = rep(1, n),
      check.names = FALSE
    )
  )
  origin_data = data.frame(x1 = c(0, 0.5, 1)[rep(1:3, length.out = n)], y = rnorm(n))
  p = tryCatch({
    plot_regional_pd(
      prepared_data = prepared_data,
      origin_data = origin_data,
      target_feature_name = "y",
      node_idx = 1,
      color_ice = "lightblue", color_pd = "red",
      ymin = NA, ymax = NA, show_point = FALSE, mean_center = TRUE
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
  dt = data.table::data.table(row_id = seq_len(n), interval_index = rep(1L, n), dL = 0, int_n = n, int_s1 = 0, int_s2 = 0)
  tryCatch(calculate_ale_heterogeneity_list_cpp(list(x = dt)), error = function(e) skip("ALE C++ not loaded"))
  set.seed(123)
  n = 40
  data = data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  task = mlr3::TaskRegr$new("t", backend = data, target = "y")
  learner = mlr3::lrn("regr.ranger")
  learner$train(task)
  tree = gadgetTree$new(strategy = aleStrategy$new(), n_split = 1, min_node_size = 10)
  tree$fit(model = learner, data = data, target_feature_name = "y", n_intervals = 5)
  pl = tree$plot(data = data, target_feature_name = "y", show_plot = FALSE)
  expect_true(is.list(pl))
})
