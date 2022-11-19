library(tidyverse)
library(caret)

set.seed(42)

z1 <- read_csv("data/clean/train-test-3556.csv") |> 
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1))

tr <- trainControl(method = "repeatedcv",
                   number = 20,
                   repeats = 10)
tr1 <- trainControl(method = "boot632",number =1000)


## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% select(u:z, u1:z1, cor_logit)
x = training %>% select(u:z, u1:z1)
y = training$cor_logit
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

## GLMnet
(t1 <- Sys.time())
glmnet1 <- train(cor_logit~., data=trainTransformed,
             method = "glmnet",trControl=tr)
(t2 <- Sys.time())
t2-t1
saveRDS(glmnet1, "models/by-model/glmnet-new/glmnet-0_2min")
summary(glmnet1)
fitted <- predict(glmnet1)
glmpred1 <- predict(glmnet1, testTransformed)
lmvalues1 <- data.frame(obs = testTransformed$cor_logit, 
                      pred = glmpred1,
                      res = glmpred1 - testTransformed$cor_logit)

defaultSummary(lmvalues1)
