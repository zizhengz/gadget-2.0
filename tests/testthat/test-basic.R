test_that("package can be loaded", {
  library(gadget)
  expect_true(TRUE)
})

test_that("basic functionality works", {
  library(gadget)

  # Test that main classes can be created
  expect_true(inherits(pdStrategy$new(), "pdStrategy"))
  expect_true(inherits(effectStrategy$new(), "effectStrategy"))
  expect_true(inherits(gadgetTree$new(strategy = pdStrategy$new()), "gadgetTree"))

  # Test that C++ functions are available
  expect_true(exists("search_best_split_cpp"))
  expect_true(exists("search_best_split_point_cpp"))
})

test_that("package exports are available", {
  library(gadget)

  # Check that main functions are exported
  expected_functions = c(
    "gadgetTree", "pdStrategy", "aleStrategy", "effectStrategy",
    "search_best_split_cpp", "search_best_split_point_cpp",
    "plot_tree_structure", "plot_regional_pd", "plot_tree_pd",
    "calculate_ale", "convert_tree_to_list", "extract_split_info",
    "node_heterogeneity"
  )

  for (func in expected_functions) {
    expect_true(exists(func), info = paste("Function", func, "should be available"))
  }
})
