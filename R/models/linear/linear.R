library(tidyverse)
library(caret)

set.seed(42)

z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

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


## LINEAR
(t1 <- Sys.time())
lm1 <- train(cor_logit~., data=trainTransformed,
             method = "lm",trControl=tr)
(t2 <- Sys.time())
t2-t1
saveRDS(lm1, "R/models/linear/linear-0_3min")
summary(lm1)
fitted <- predict(lm1)
lmpred1 <- predict(lm1, testTransformed)
lmvalues1 <- data.frame(obs = testTransformed$cor_logit,
                      pred = lmpred1,
                      res = lmpred1 - testTransformed$cor_logit)

defaultSummary(lmvalues1)
# Calculate MAE
mean(abs(lmvalues1$pred-lmvalues1$obs))

theme_set(theme_bw())

ggplot(lm1values, aes(obs,pred, colour=res)) +
  geom_point(alpha=0.9) +
  geom_smooth(se=FALSE,colour="red",
              linetype="dashed", size=0.5)+
  geom_abline(slope=1, linetype="dashed")
