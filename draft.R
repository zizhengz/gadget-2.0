library(ISLR2)
library(mlr3)
library(mlr3learners)
library(iml)
library(data.table)
library(gadget)

#### Bike data ####
data(Bikeshare)
bike = data.table(Bikeshare[sample(1:8645, 1000), ])

bike.X = bike[, .(day, hr, temp, windspeed, weekday, workingday, hum, season,mnth, holiday,registered, weathersit, atemp, casual)]
bike.y = bike$bikers
train = cbind(bike.X, "target" = bike.y)
bike.data = as.data.frame(train)

set.seed(123)
bike.task = TaskRegr$new(id = "bike", backend = bike.data, target = "target")
bike.learner = lrn("regr.ranger")
bike.learner$train(bike.task)

bike.X = bike.task$data(cols = bike.task$feature_names)
bike.y = bike.task$data(cols = bike.task$target_names)[[1]]

bike.predictor = Predictor$new(model = bike.learner, data = bike.X, y = bike.y)

effect_all = FeatureEffects$new(bike.predictor, method = "ice",
  grid.size = 20)

tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 10)
tree$fit(effect_all, bike.data, target.feature.name = "target")
tree$plot_tree_structure()
tree$extract_split_info()
tree$plot(effect_all, bike.data, target.feature.name = "target",
  show.plot = TRUE, show.point = FALSE, mean.center = FALSE,
  depth = 6,
  node.id = c(58, 59),
  features = c("hr", "workingday")
)
dt = data.table::rbindlist(tree$split_benchmark)
print(dt)
boxplot(time ~ depth, data = dt, main = "Distribution of split time per layer")

result = search_best_split_cpp(Z = bike.data, Y = effect_all$Y, min_node_size = 2)
print(result)


#### Synthetic data ####
set.seed(123)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
y = ifelse(x3 > 0, 3 * x1, -3 * x1) + x3 + rnorm(n, sd = 0.3)
syn.data = data.frame(x1, x2, x3, y)

syn.task = TaskRegr$new("xor", backend = syn.data, target = "y")
syn.learner = lrn("regr.ranger")
syn.learner$train(syn.task)
syn.predictor = Predictor$new(syn.learner, data = syn.data[, c("x1", "x2", "x3")], y = syn.data$y)
syn.effect = FeatureEffects$new(syn.predictor, grid.size = 20, method = "ice")

syn.tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 4)
syn.tree$fit(syn.effect, syn.data, target.feature.name = "y", split.feature = NULL)
syn.tree$plot_tree_structure()
syn.tree$extract_split_info()
syn.tree$plot(syn.effect, syn.data, target.feature.name = "y")
