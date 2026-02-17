library(gadget)
library(iml)
library(ranger)
library(mlr3)
library(mlr3learners)
#library(pryr)
library(bench)

set.seed(1)
options(future.globals.maxSize = 4 * 1024 * 1024^2) # 4GB
library(future)
plan(sequential)

datagen_p5 = function(n, seed = 1) {
  set.seed(seed)
  x1 = round(runif(n, -1, 1), 1)
  x2 = round(runif(n, -1, 1), 3)
  x3 = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
  x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
  x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  dat = data.frame(x1, x2, x3, x4, x5)
  y = 0.2 * x1 - 8 * x2 + ifelse(x3 == 0, 16 * x2, 0) + ifelse(x1 > 0, 8 * x2, 0)
  eps = rnorm(n, 0, 0.1 * sd(y))
  y = y + eps
  dat$y = y
  X = dat[, setdiff(colnames(dat), "y")]
  mod = ranger(y ~ ., data = dat, num.trees = 100)
  pred = function(model, newdata) predict(model, newdata)$predictions
  model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
  eff = FeatureEffects$new(model, method = "ice", grid.size = 20)
  list(dat = dat, eff = eff)
}

datagen_p5 = function(n, seed = 1) {
  set.seed(seed)
  x1 = round(runif(n, -1, 1), 1)
  x2 = round(runif(n, -1, 1), 3)
  x3 = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
  x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
  x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  dat = data.frame(x1, x2, x3, x4, x5)
  y = 0.2 * x1 - 8 * x2 + ifelse(x3 == 0, 16 * x2, 0) + ifelse(x1 > 0, 8 * x2, 0)
  eps = rnorm(n, 0, 0.1 * sd(y))
  y = y + eps
  dat$y = y
  X = dat[, setdiff(colnames(dat), "y")]
  mod = ranger(y ~ ., data = dat, num.trees = 100)
  pred = function(model, newdata) predict(model, newdata)$predictions
  model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
  eff = FeatureEffects$new(model, method = "ice", grid.size = 20)
  list(dat = dat, eff = eff)
}

datagen_p10 = function(n, seed = 1) {
  set.seed(seed)
  x1 = round(runif(n, -1, 1), 1)
  x2 = round(runif(n, -1, 1), 3)
  x3 = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
  x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
  x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  x6 = rnorm(n, mean = 1, sd = 5)
  x7 = round(rnorm(n, mean = 10, sd = 10), 2)
  x8 = round(rnorm(n, mean = 100, sd = 15), 4)
  x9 = round(rnorm(n, mean = 1000, sd = 20), 7)
  x10 = rnorm(n, mean = 10000, sd = 25)
  dat = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  y = 0.2 * x1 - 8 * x2 + ifelse(x3 == 0, 16 * x2, 0) + ifelse(x1 > 0, 8 * x2, 0) + 2 * x8
  eps = rnorm(n, 0, 0.1 * sd(y))
  y = y + eps
  dat$y = y
  X = dat[, setdiff(colnames(dat), "y")]
  mod = ranger(y ~ ., data = dat, num.trees = 100)
  pred = function(model, newdata) predict(model, newdata)$predictions
  model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
  eff = FeatureEffects$new(model, method = "ice", grid.size = 20)
  list(dat = dat, eff = eff)
}

datagen_p20 = function(n, seed = 1) {
  set.seed(seed)
  x1 = round(runif(n, -1, 1), 1)
  x2 = round(runif(n, -1, 1), 3)
  x3 = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
  x4 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.7, 0.3))
  x5 = sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  x6 = rnorm(n, mean = 1, sd = 5)
  x7 = round(rnorm(n, mean = 10, sd = 10), 2)
  x8 = round(rnorm(n, mean = 100, sd = 15), 4)
  x9 = round(rnorm(n, mean = 1000, sd = 20), 7)
  x10 = rnorm(n, mean = 10000, sd = 25)
  noise = replicate(10, rnorm(n), simplify = FALSE)
  names(noise) = paste0("noise", 1:10)
  dat = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, noise)
  y = 0.2 * x1 - 8 * x2 + ifelse(x3 == 0, 16 * x2, 0) + ifelse(x1 > 0, 8 * x2, 0) + 2 * x8
  eps = rnorm(n, 0, 0.1 * sd(y))
  y = y + eps
  dat$y = y
  X = dat[, setdiff(colnames(dat), "y")]
  mod = ranger(y ~ ., data = dat, num.trees = 100)
  pred = function(model, newdata) predict(model, newdata)$predictions
  model = Predictor$new(mod, data = X, y = dat$y, predict.function = pred)
  eff = FeatureEffects$new(model, method = "ice", grid.size = 20)
  list(dat = dat, eff = eff)
}

n_list = c(1000, 5000, 10000)
p_list = c(5, 10, 20)

library(bench)
bench_results = list()
for (n in n_list) {
  for (p in p_list) {
    cat(sprintf("Running: n = %d, p = %d\n", n, p))
    if (p == 5) {
      sim = datagen_p5(n)
    } else if (p == 10) {
      sim = datagen_p10(n)
    } else if (p == 20) {
      sim = datagen_p20(n)
    }
    res = bench::mark(
      fit = {
        tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 10)
        tree$fit(sim$eff, sim$dat, target.feature.name = "y")
      },
      iterations = 5
    )
    res$n = n
    res$p = p
    bench_results[[paste0("n", n, "_p", p)]] = res
  }
}

bench_all = do.call(rbind, bench_results)
n_vec = rep(bench_all$n, each = 5)
p_vec = rep(bench_all$p, each = 5)
time_vec = unlist(bench_all$time)
time_ms = as.numeric(time_vec) * 1000
bench_long = data.frame(
  n = n_vec,
  p = p_vec,
  time_ms = time_ms
)


library(ggplot2)
ggplot(bench_long, aes(x = factor(n), y = time_ms, color = factor(p), group = p)) +
  geom_boxplot(aes(group = interaction(n, p))) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "Sample Size (n)", y = "Fit Time (ms)", color = "Feature Number (p)",
       title = "gadgetTree$fit Benchmark: Varying n and p") +
  theme_minimal()

