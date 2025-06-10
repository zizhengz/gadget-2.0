library(testthat)
library(data.table)
library(checkmate)

## -----------------------------------------------------------
## helpers used in multiple tests ----------------------------
## -----------------------------------------------------------
dummy_objective <- function(y, x, ...) {
  # simplistic loss: total variance of all entries
  sum((unlist(y) - mean(unlist(y)))^2, na.rm = TRUE)
}

make_dummy_Y <- function(n_row = 20, n_col = 5) {
  data.table(matrix(rnorm(n_row * n_col), nrow = n_row))
}

## -----------------------------------------------------------
## 1 generate_split_candidates -------------------------------
## -----------------------------------------------------------
test_that("generate_split_candidates respects min.node.size & uniqueness", {
  set.seed(1)
  x <- sort(rnorm(100))
  cand <- generate_split_candidates(xval = x,
                                    n.quantiles = 9,
                                    min.node.size = 10)

  expect_type(cand, "double")
  # ≥ 10 observations around each cand. point
  expect_true(all(cand > x[10] & cand < x[91]))
  # no duplication
  expect_equal(length(cand), length(unique(cand)))
})

## -----------------------------------------------------------
## 2 adjust_split_point --------------------------------------
## -----------------------------------------------------------
test_that("adjust_split_point nudges points inside open intervals", {
  x <- c(1, 2, 2, 4, 6, 7)
  s <- c(2, 4)
  s_adj <- adjust_split_point(s, x)

  # Adjusted to no longer be located on the original observation point
  expect_false(any(s_adj %in% x))
  # Same length and sorted
  expect_equal(length(s_adj), length(s))
  expect_true(is.unsorted(s_adj) == FALSE)
})

## -----------------------------------------------------------
## 3 get_closest_point ---------------------------------------
## -----------------------------------------------------------
test_that("get_closest_point snaps to nearest admissible value", {
  set.seed(123)
  x <- sort(sample(1:100, 50))
  pts <- c(15.3, 76.8)
  res <- get_closest_point(pts, x, min.node.size = 5)

  # res must be in x
  expect_true(all(res %in% x))
  # ≥ 5 observations around each return point
  idx <- match(res, x)
  expect_true(all(idx > 5 & idx < (length(x) - 5)))
})

## -----------------------------------------------------------
## 4 perform_split -------------------------------------------
## -----------------------------------------------------------
test_that("perform_split returns finite loss or Inf when min size violated", {
  set.seed(42)
  xval <- data.frame(feat = rnorm(30))
  y <- list(make_dummy_Y(30, 3))
  grid <- list(feat = 1:3)

  # valid split → finite loss
  good <- perform_split(split.points  = median(xval$feat),
                        xval          = xval,
                        y             = y,
                        min.node.size = 5,
                        grid          = grid,
                        objective     = dummy_objective)
  expect_true(is.finite(good))

  # violate min.node.size → Inf
  bad <- perform_split(split.points  = median(xval$feat),
                       xval          = xval,
                       y             = y,
                       min.node.size = 31,
                       grid          = grid,
                       objective     = dummy_objective)
  expect_identical(bad, Inf)
})

## -----------------------------------------------------------
## 5 find_best_binary_split -------------------------------------------
## -----------------------------------------------------------
test_that("find_best_binary_split returns Inf when no legal candidate", {
  xval <- data.frame(x = 1:15)           # too few sample
  y    <- list(make_dummy_Y(15, 3))
  grid <- list(x = 1:3)

  res <- find_best_binary_split(
           xval, y,
           min.node.size = 10,           # 0 candidate
           grid = grid,
           objective = dummy_objective,
           n.quantiles = 5
         )
  expect_true(is.infinite(res$objective.value))
  expect_true(is.na(res$split.points))
})

## -----------------------------------------------------------
## 6 compute_data_for_ice_splitting --------------------------
## -----------------------------------------------------------
test_that("compute_data_for_ice_splitting returns X, Y, grid of correct shape", {

  ice_long <- data.table(
    .id      = rep(1:3, each = 3),
    .type    = "ice",
    .feature = "x1",
    .borders = rep(1:3, times = 3),
    .value   = rnorm(9)
  )
  effect_stub <- list(
    features = "x1",
    results  = list(x1 = ice_long)
  )
  class(effect_stub) <- "FeatureEffect"

  testdata <- data.frame(x1 = rnorm(3), x2 = rnorm(3))
  out <- compute_data_for_ice_splitting(effect_stub, testdata, Z = c("x1", "x2"))

  expect_named(out, c("X", "Y", "grid"))
  expect_s3_class(out$X, "data.table")
  expect_type(out$Y, "list")
  expect_equal(length(out$Y), 1)
  expect_equal(names(out$Y), "x1")
  expect_type(out$grid, "list")
})

