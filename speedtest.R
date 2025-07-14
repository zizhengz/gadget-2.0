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
X = dat[, setdiff(colnames(dat), "y")]

# Fit model and compute ICE for x2
mod = ranger(y ~ ., data = dat, num.trees = 100)
pred = function(model, newdata) predict(model, newdata)$predictions
model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
effect = FeatureEffect$new(model, method = "ice", grid.size = 20, feature = "x2")

tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 5)
tree$fit(effect, dat, target.feature.name = "y")
dt = data.table::rbindlist(tree$split_benchmark)
print(dt)
boxplot(time ~ depth, data = dt, main = "Distribution of split time per layer")

tree$plot_tree_structure()
tree$extract_split_info()
tree$plot(effect, dat, target.feature.name = "y",
          show.plot = TRUE, show.point = FALSE, mean.center = FALSE,
          depth = 6,
          node.id = c(48,49),
          features = c("hr","workingday")
          )
