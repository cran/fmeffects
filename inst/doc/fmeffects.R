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
task = as_task_regr(x = bikes, id = "bikes", target = "count")
forest = lrn("regr.ranger")$train(task)

## -----------------------------------------------------------------------------
effects = fme(model = forest,
               data = bikes,
               target = "count",
               feature = "temp",
               step.size = 1,
               ep.method = "envelope")

## ---- warning=FALSE-----------------------------------------------------------
plot(effects, jitter = c(0.2, 0))

## -----------------------------------------------------------------------------
effects$ame

## -----------------------------------------------------------------------------
head(effects$results)

## -----------------------------------------------------------------------------
effects2 = fme(model = forest,
               data = bikes,
               target = "count",
               feature = c("temp", "humidity"),
               step.size = c(-3, -0.1),
               ep.method = "envelope")

plot(effects2, jitter = c(0.1, 0.02))

## -----------------------------------------------------------------------------
effects2$ame

## -----------------------------------------------------------------------------
var(effects2$results$fme)

## -----------------------------------------------------------------------------
effects3 = fme(model = forest,
              data = bikes,
              target = "count",
              feature = "weather",
              step.size = "rain")
summary(effects3)

## ---- warning=FALSE-----------------------------------------------------------
plot(effects3)

## ---- warning=FALSE-----------------------------------------------------------
overview = ame(model = forest,
           data = bikes,
           target = "count")
overview$results

## ---- warning=FALSE-----------------------------------------------------------
overview = ame(model = forest,
               data = bikes,
               target = "count",
               features = c(weather = c("rain", "clear"), temp = -1, humidity = 0.1),
               ep.method = "envelope")
overview$results

## -----------------------------------------------------------------------------
subspaces = came(effects = effects2, number.partitions = 3)
summary(subspaces)

## -----------------------------------------------------------------------------
plot(subspaces)

