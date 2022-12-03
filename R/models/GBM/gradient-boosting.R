library(tidyverse)
library(caret)
library(caretEnsemble)
library(showtext)
library(ggokabeito)
library(viridis)
library(xgboost)

set.seed(42)

font_add("Fuzzy Bubbles", regular = "fonts/ABeeZee-Regular.ttf")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 16, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 16),
          axis.title = element_text(face = "bold", size = 20),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}

z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1))


tr <- trainControl(method = "repeatedcv",
                   number = 20,
                   repeats = 10)
# tr1 <- trainControl(method = "boot632",number =1000)


## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% select(u:z, u1:z1, cor_logit)
x = training %>% select(u:z, u1:z1)
y = training$cor_logit
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)


##  Gradient Boosting

tg <- expand.grid(shrinkage = 0.1,
                  interaction.depth = c(10),
                  n.minobsinnode = c(10),
                  n.trees = c(1000))
tg <- expand.grid(shrinkage = seq(0.1, 0.5, by = 0.2),
                  interaction.depth = c(1, 5, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500))
(t1 <- Sys.time())
gbm1<- train(cor_logit~., data = trainTransformed,
             method = "gbm", trControl = tr, tuneGrid =tg, verbose = FALSE)
(t2 <- Sys.time())
t2-t1
gbm1

plot(gbm1)
gbm1pred<-predict(gbm1, testTransformed)
gbm1values<-data.frame(obs=testTransformed$cor_logit,
                       pred=gbm1pred,
                       res=gbm1pred-testTransformed$cor_logit)
defaultSummary(gbm1values)
theme_set(theme_bw())
gbm1values |>
  ggplot(aes(res)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 50) +
  geom_density(lwd = 0.5, colour = 4,
               fill = 4, alpha = 0.25) +
  theme_clean() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
ggplot(gbm1values, aes(obs,
                       pred,
                       colour=res)) +
  geom_point(alpha=0.9) +
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+
  geom_abline(slope=1, linetype="dashed")
qqplot(gbm1values$pred, gbm1values$obs)

