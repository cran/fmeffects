params <-
list(EVAL = FALSE)

## -----------------------------------------------------------------------------
library(fmeffects)
data(bikes, package = "fmeffects")
str(bikes)

## ---- message=FALSE-----------------------------------------------------------
set.seed(123)
library(mlr3verse)
library(ranger)
task = as_task_regr(x = bikes, target = "count")
forest = lrn("regr.ranger")$train(task)

## -----------------------------------------------------------------------------
effects = fme(model = forest,
               data = bikes,
               features = list(temp = 1),
               ep.method = "envelope")

## ---- message=FALSE-----------------------------------------------------------
plot(effects)

## -----------------------------------------------------------------------------
effects$ame

## -----------------------------------------------------------------------------
head(effects$results)

## ---- message=FALSE-----------------------------------------------------------
effects2 = fme(model = forest,
               data = bikes,
               features = list(temp = -3, humidity = -0.1),
               ep.method = "envelope")

## ---- message=FALSE-----------------------------------------------------------
plot(effects2)

## -----------------------------------------------------------------------------
effects2$ame

## -----------------------------------------------------------------------------
sd(effects2$results$fme)

## ---- results='hide'----------------------------------------------------------
effects3 = fme(model = forest,
               data = bikes[1:200,],
               feature = list(temp = 1),
               ep.method = "envelope",
               compute.nlm = TRUE)

## -----------------------------------------------------------------------------
effects3$anlm

## ---- message = FALSE, fig.asp = 0.4------------------------------------------
plot(effects3, with.nlm = TRUE)

## ---- results='hide'----------------------------------------------------------
effects4 = fme(model = forest,
               data = bikes[1:200,],
               features = list(temp = -3, humidity = -0.1),
               ep.method = "envelope",
               compute.nlm = TRUE)

## ---- message = FALSE, fig.asp = 0.4------------------------------------------
plot(effects4, bins = 25, with.nlm = TRUE)

## -----------------------------------------------------------------------------
effects5 = fme(model = forest,
              data = bikes,
              features = list(weather = "rain"))
summary(effects5)

## ---- warning=FALSE-----------------------------------------------------------
plot(effects5)

## -----------------------------------------------------------------------------
fme(model = forest,
    data = bikes,
    features = list(weather = "clear", workingday = "no"))$ame

## ---- warning=FALSE-----------------------------------------------------------
overview = ame(model = forest, data = bikes)
overview$results

## ---- warning=FALSE-----------------------------------------------------------
overview = ame(model = forest,
               data = bikes,
               features = list(weather = c("rain", "clear"), humidity = 0.1),
               ep.method = "envelope")
overview$results

## -----------------------------------------------------------------------------
subspaces = came(effects = effects2, number.partitions = 3)
summary(subspaces)

## -----------------------------------------------------------------------------
plot(subspaces)

