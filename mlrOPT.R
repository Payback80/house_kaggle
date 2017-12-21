library(mlr)
lrn = makeLearner("regr.xgboost", eval_metric = "rmse", predict.type = "response")

ps = makeParamSet(
  makeIntegerParam("nrounds", lower = 50, upper = 1000, default = 100),
  makeNumericParam("eta", lower = 0.01, upper = 1, default = 0.01), 
                
  makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6)
)

library(mlrMBO)
library(parallelMap)
task = makeRegrTask(data = train_2, target = "SalePrice")
mbo.ctrl = makeMBOControl(save.on.disk.at = c(0, 5, 10, 20, 50, 75, 85, 95))
mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 100)
surrogate.lrn = makeLearner("regr.km", predict.type = "se")
ctrl = mlr:::makeTuneControlMBO(learner = surrogate.lrn, mbo.control = mbo.ctrl)

parallelStartMulticore(cpus = 10L)
res.mbo = tuneParams(lrn, task, cv10, par.set = ps, control = ctrl, 
                     show.info = TRUE, measures = rmse)
parallelStop()