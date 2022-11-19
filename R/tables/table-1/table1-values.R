library(tidyverse)
library(caret)
library(caretEnsemble)
library(showtext)
library(ggokabeito)
library(viridis)
library(xgboost)

set.seed(42)

model_predictions <- function(model,
                              my_data = testTransformed)
  {
  modelpred<-predict(model, my_data)
  x <- data.frame(obs = my_data$cor_logit,
                  pred = modelpred,
                  res = modelpred-my_data$cor_logit)
}


z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] |> dplyr::select(u:z, u1:z1, cor_logit)
testing <- z1[-inTraining,] |> dplyr::select(u:z, u1:z1, cor_logit)
training1 <- z1[ inTraining,]
testing1 <- z1[-inTraining,]

x = training |> dplyr::select(u:z, u1:z1)
y = training$cor_logit
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

svm1 <- readRDS("R/models/SVR/svr-C20-sigma012-3_60mins")
rf1 <- readRDS("R/models/Random-Forest/rf-minnode-5-mtry-6-5_79min")
gbm1 <- readRDS("R/models/GBM/gbm-s-01-05-id1-5-10-nm2-5-10-nt100-300-500-2_95hours")
glmnet1 <- readRDS("R/models/GLMnet/glmnet-0_2min")
lm1 <- readRDS("R/models/linear/linear-0_3min")

svm1values <- model_predictions(model = svm1)
rf1values <- model_predictions(rf1)
gbm1values <- model_predictions(gbm1)
glmnet1values <- model_predictions(glmnet1)
lm1values <- model_predictions(lm1)
yardstick::metrics(data = svm1values, truth = obs, estimate = pred)
yardstick::metrics(data = rf1values, truth = obs, estimate = pred)
yardstick::metrics(data = gbm1values, truth = obs, estimate = pred)
yardstick::metrics(data = glmnet1values, truth = obs, estimate = pred)
yardstick::metrics(data = lm1values, truth = obs, estimate = pred)


hydroGOF::NSE(svm1values$pred, svm1values$obs)
hydroGOF::NSE(rf1values$pred, rf1values$obs)
hydroGOF::NSE(gbm1values$pred, gbm1values$obs)
hydroGOF::NSE(glmnet1values$pred, glmnet1values$obs)
hydroGOF::NSE(lm1values$pred, lm1values$obs)
