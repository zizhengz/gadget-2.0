library(ISLR2)
library(mlr)
library(mlr3)
library(mlr3learners)
library(mlr3verse)
library(mlr3tuning)
library(mlr3measures)
library(iml)
library(data.table)
library(gadget)

#### Bike data processing ####
data(Bikeshare)
bike = data.table(Bikeshare[sample(1:8645,1000),])

bike.X = bike[, .(day, hr, temp, windspeed, weekday, workingday, hum, season,mnth, holiday,registered, weathersit, atemp, casual)]
bike.y = bike$bikers
train1 = cbind(bike.X, "cnt" = bike.y)
bike.data = as.data.frame(train1)

set.seed(123)
Bike.task = TaskRegr$new(id = "bike", backend = bike.data, target = "cnt")
Bike.learner = lrn("regr.ranger")
Bike.learner$train(Bike.task)

bike.X = Bike.task$data(cols = Bike.task$feature_names)
bike.y = Bike.task$data(cols = Bike.task$target_names)[[1]]

Bike.predictor = Predictor$new(model = Bike.learner, data = bike.X, y = bike.y)

effect_all = FeatureEffects$new(Bike.predictor, method = "ice",
  grid.size = 20)

tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 5)
tree$fit(effect_all, bike.data, target.feature.name = "cnt")
tree$plot_tree_structure()
tree$extract_split_info()
tree$plot(effect_all, bike.data, target.feature.name = "cnt",
          show.plot = TRUE, show.point = FALSE, mean.center = FALSE,
          depth = 6,
          node.id = c(58,59),
          features = c("hr","workingday")
          )
dt = data.table::rbindlist(tree$split_benchmark)
print(dt)
boxplot(time ~ depth, data = dt, main = "Distribution of split time per layer")

result <- search_best_split_cpp(Z = bike.data, Y = effect_all$Y, min_node_size = 2)
   print(result)


#### Synthetic data processing ####
n = 500
set.seed(123)
create_xor = function(n, seed) {
  x2 = runif(n, -1, 1)
  x3 = runif(n, -1, 1)
  x1 = runif(n, -1, 1)
  y = ifelse(x3 > 0, 3 * x1, -3 * x1) + x3 + rnorm(n, sd = 0.3)
  data.frame(x1, x2, x3, y)
}
syn.data = create_xor(n, seed)
syn.X = syn.data[, setdiff(names(syn.data), "y")]
syn.features = colnames(syn.X)
set.seed(123)
syn.task = makeRegrTask(data = syn.data, target = "y")
ps = makeParamSet(
  makeDiscreteParam("decay", values = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5)),
  makeDiscreteParam("size", values = c(3, 5, 10, 20, 30))
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 5L)
res = tuneParams(makeLearner("regr.nnet", maxit = 1000), task = syn.task, resampling = rdesc,
  par.set = ps, control = ctrl, measures = list(mlr::mse, mlr::mae, mlr::rsq))
set.seed(123)
syn.lrn = makeLearner("regr.nnet", maxit = 1000, size = res$x$size, decay = res$x$decay, trace = F)
syn.model = mlr::train(task = syn.task, learner = syn.lrn)
syn.predictor = Predictor$new(syn.model, data = syn.data[which(names(syn.data) != "y")], y = syn.data$y)
syn.effect = FeatureEffects$new(syn.predictor, grid.size = 20, method = "ice")


syn.tree = gadgetTree$new(strategy = pdStrategy$new(), n.split = 4)
syn.tree$fit(syn.effect, syn.data, target.feature.name = "y", split.feature = NULL)
syn.tree$plot_tree_structure()
syn.tree$extract_split_info()
syn.tree$plot(syn.effect, syn.data, target.feature.name = "y", depth = 2, node.id = 2)


test_data <- data.frame(x = 1:10, y = 1:10)
test_result <- search_best_split_cpp(Z = test_data, Y = list(matrix(1:20, ncol=2)), min_node_size = 2)
print(test_result)
