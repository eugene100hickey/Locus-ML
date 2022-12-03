library(tidyverse)
library(caret)

set.seed(42)

z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1))


tr <- trainControl(method = "repeatedcv",
                   number = 20,
                   repeats = 10)

## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% select(u:z, u1:z1, cor_logit)
x = training %>% select(u:z, u1:z1)
y = training$cor_logit
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

tg <- data.frame(mtry = 6,
                 splitrule = "variance",
                 min.node.size = 5)


(t1 <- Sys.time())
rf1 <- train(cor_logit~., data = trainTransformed,
             method = "ranger", trControl=tr, tuneGrid = tg)
(t2 <- Sys.time())
t2-t1

saveRDS(rf1, "R/models/Random-Forest/rf-minnode-5-mtry-6-5_79min")

rf1$results

rf1pred<-predict(rf1$finalModel, testTransformed)$predictions
rf1values<-data.frame(obs=testTransformed$cor_logit,
                      pred=rf1pred,
                      res=rf1pred-testTransformed$cor_logit)
yardstick::metrics(data = rf1values, truth = obs, estimate = pred)
defaultSummary(rf1values)

prediction2 <- predict(rf1$finalModel,
                       data = predict(rf1$preProcess,
                                      testTransformed))$prediction
