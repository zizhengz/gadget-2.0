# Example
library(data.table)
library(ranger)
library(iml)
library(tidyverse)

set.seed(1)

# Simulate Data
n = 5000
x1 = round(runif(n, -1, 1), 1)
x2 = round(runif(n, -1, 1), 3)
x3 = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))

# noisy vars
x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
x6 = rnorm(n, mean = 1, sd = 5)
x7 = round(rnorm(n, mean = 10, sd = 10), 2)
x8 = round(rnorm(n, mean = 100, sd = 15), 4)
x9 = round(rnorm(n, mean = 1000, sd = 20), 7)
x10 = rnorm(n, mean = 10000, sd = 25)

# target
y = 0.2*x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > 0, I(8*x2),0) + 2*x8
eps = rnorm(n, 0, 0.1*sd(y))
y = y + eps

dat = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,y)
#dat = cbind(dat, dat, dat, dat, dat, dat, dat, dat, dat, dat)
#dat = cbind(dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, y)
#colnames(dat) = c(paste0("x", 1:1000), "y")
X = dat[, setdiff(colnames(dat), "y")]

# Fit model and compute ICE for x2
mod = ranger(y ~ ., data = dat, num.trees = 100)
pred = function(model, newdata) predict(model, newdata)$predictions
model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = "x2")

# Center ICE curves
eff = as.data.table(effect$results)
eff = as.data.frame(eff[, .value := (.value - mean(.value)), by = c(".type", ".id")])

# Get ICE values and arrange them in a horizontal matrix
Yice = spread(eff, x2, .value)
Yice = Yice[, setdiff(colnames(Yice), c(".type", ".id"))]
Yice = as.matrix(Yice)

str(X) # contains our feature values
str(Yice) # contains ICE values for each grid point

#sp = best_split(X, Yice)
#sp


# library(checkmate)
# split = build_tree(effect, data = dat, target.feature.name = "y", n.split = 1)
# split[[2]][[1]]$objective.value + split[[2]][[2]]$objective.value
# split

library(Rcpp)
library(devtools)
load_all()
#split = build_tree(effect, data = dat, target.feature.name = "y", n.split = 1)
prepared.data = prepare_split_data_pd(effect = effect, data = dat, target.feature.name = "y")
Z = prepared.data$Z
Y = prepared.data$Y
grid = prepared.data$grid

#
sourceCpp("search_best_split.cpp", rebuild = TRUE)

# check output
search_best_split_point(dat$x2, Y, min.node.size = 1)
search_best_split_point_cpp(dat$x2, Y, min_node_size = 1)

# check output
search_best_split(Z, Y, min.node.size = 1, n.quantiles = NULL)
search_best_split_cpp(Z, Y, min_node_size = 1, n_quantiles = NULL)
# best_split(X, Yice)

library(microbenchmark)
# speed test
microbenchmark(
  search_best_split_point(dat$x2, Y, min.node.size = 1),
  search_best_split_point_cpp(dat$x2, Y, min_node_size = 1),
  times = 50
)

# speed test
microbenchmark(
  search_best_split(Z, Y, min.node.size = 1, n.quantiles = NULL),
  search_best_split_cpp(Z, Y, min_node_size = 1, n_quantiles = NULL),
  times = 50
)
