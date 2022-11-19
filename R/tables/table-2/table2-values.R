library(tidyverse)
library(caret)

set.seed(42)

model_predictions <- function(model,
                              my_data = testTransformed,
                              index,
                              back_up = testing1) {
  modelpred<-predict(model, my_data)
  x <- data.frame(obs = my_data$cor_logit[index],
                  pred = modelpred[index],
                  res = modelpred-my_data$cor_logit[index],
                  specobj = back_up$specobjid[index],
                  cor = back_up$cor[index],
                  cor_logit = back_up$cor_logit[index],
                  pred_cor_logit = revPredict(preproc = preProcValues, modelpred)$cor_logit[index])
  x |> mutate(pred_cor = rev_logit(pred_cor_logit))
}

revPredict <- function(preproc = preProcValues, data, digits=0) {
  dummy <- rep(1, length(data))
  data.frame(u=dummy,g=dummy, r=dummy, i=dummy, z=dummy,
             u1=dummy, g1=dummy, r1=dummy, i1=dummy, z1=dummy,
             cor_logit = data) %>%
    select(one_of(preproc$mean %>% names)) %>%
    map2_df(preproc$std, ., function(sig, dat) dat * sig) %>%
    map2_df(preproc$mean, ., function(mu, dat) dat + mu)
}
rev_logit <- function(x){
  (exp(x)-1)/(exp(x)+1)
}


z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

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

# A vs B  correlation fit
index <- 2211
model_predictions(model = svm1, my_data = trainTransformed, back_up = training1, index = index)[1,]
model_predictions(model = rf1, my_data = trainTransformed, back_up = training1, index = index)[1,]
model_predictions(model = gbm1, my_data = trainTransformed, back_up = training1, index = index)[1,]
model_predictions(model = glmnet1, my_data = trainTransformed, back_up = training1, index = index)[1,]
model_predictions(model = lm1, my_data = trainTransformed, back_up = training1, index = index)[1,]

# C vs D  correlation fit
index <- 331
model_predictions(model = svm1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = rf1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = gbm1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = glmnet1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = lm1, my_data = testTransformed, back_up = testing1, index = index)[1,]


# E vs F  correlation fit
index <- 415
model_predictions(model = svm1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = rf1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = gbm1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = glmnet1, my_data = testTransformed, back_up = testing1, index = index)[1,]
model_predictions(model = lm1, my_data = testTransformed, back_up = testing1, index = index)[1,]
