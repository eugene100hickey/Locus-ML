library(tidyverse)
library(caret)
library(showtext)
library(ggokabeito)
library(viridis)
library(patchwork)
# library(xgboost)

set.seed(42)

font_add("Fuzzy Bubbles", regular = "fonts/ABeeZee-Regular.ttf")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 36, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 40),
          axis.title = element_text(face = "bold", size =40),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}

z1 <- read_csv("data/train-test-3556.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

validate <- read_csv("data/validate-525.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

validate_extra <- read_csv("data/validate-extra-898.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))

tr <- trainControl(method = "repeatedcv",
                   number = 20,
                   repeats = 10)
# tr1 <- trainControl(method = "boot632",number =1000)


## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] |> select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] |> select(u:z, u1:z1, cor_logit)
training_cleaned <- z1[ inTraining,] |>
  filter(subclass != "CV", subclass1 != "CV") |>
  select(u:z, u1:z1, cor_logit)
validating <- validate |> select(u:z, u1:z1, cor_logit)
validating_extra <- validate_extra |> select(u:z, u1:z1, cor_logit)
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)
training_cleanedTransformed <- predict(preProcValues, training_cleaned)
validTransformed <- predict(preProcValues, validating)
valid_extraTransformed <- predict(preProcValues, validating_extra)

## SVR

tuneGrid <- expand.grid(
  C = 20,
  sigma = 0.12
)
(t1 <- Sys.time())
svm <- train(cor_logit~., data = trainingTransformed,
              method = "svmRadial",
              tuneLength=14,
              trControl=tr,
              tuneGrid = tuneGrid)
(t2 <- Sys.time())
t2-t1
svm1 <- readRDS("models/by-model/svr-new/svr-C20-sigma012-3_60mins")
svm1

# SVR trained on training set with no CV's

tuneGrid <- expand.grid(
  C = 20,
  sigma = 0.12
)
(t1 <- Sys.time())
svm_no_cv <- train(cor_logit~., data = training_cleanedTransformed,
             method = "svmRadial",
             tuneLength=14,
             trControl=tr,
             tuneGrid = tuneGrid)
(t2 <- Sys.time())
t2-t1
saveRDS(svm_no_cv, "models/by-model/svr-new/svr-clean-C20-sigma012-noCV-4_7min")
svm_no_cv

# fit on the test set
svm1pred<-predict(svm1, testTransformed)
svm1values<-data.frame(obs=testTransformed$cor_logit,
                       pred=svm1pred,
                       res=svm1pred-testTransformed$cor_logit)
yardstick::metrics(data = svm1values, truth = obs, estimate = pred)
defaultSummary(svm1values)

# fit on the validation set
svm1pred<-predict(svm1, validTransformed)
svm1values<-data.frame(obs=validTransformed$cor_logit,
                       pred=svm1pred,
                       res=svm1pred-validTransformed$cor_logit)
yardstick::metrics(data = svm1values, truth = obs, estimate = pred)
defaultSummary(svm1values)

# fit on the training set with no CV's
svr1pred_no_cv <- predict(svm_no_cv, valid_extraTransformed)
svr1values_no_cv <- data.frame(obs=valid_extraTransformed$cor_logit,
                       pred=svr1pred_no_cv,
                       res=svr1pred_no_cv-valid_extraTransformed$cor_logit)
yardstick::metrics(data = svr1values_no_cv, truth = obs, estimate = pred)
defaultSummary(svr1values_no_cv)


theme_set(theme_bw())
ggplot(svm1values, aes(obs,
                       pred,
                       colour=res)) +
  geom_point(alpha=0.9) +
  geom_smooth(se=FALSE, colour="red", linetype="dashed", size=0.5)+
  geom_abline(slope=1, linetype="dashed")

svm1values |>
  ggplot(aes(res)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 50) +
  geom_density(lwd = 0.5, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Residuals from Validation Set of SVR Model Fit") +
  geom_point(aes(x=max(svm1values$res), y=0),
             inherit.aes = FALSE,
             size=4, shape=1, color="red", stroke = 2) +
  theme_clean() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 24))

plot_obs_pred <- svm1values |>
  ggplot(aes(pred, obs)) +
  geom_point(colour = "#82a1b3", size = 1) +
  xlab("Predicted Logit Correlation Value") +
  ylab("Observed Logit Correlation Value") +
  geom_abline(slope = 1, linetype = "dashed") +
  theme_clean()

(plot_obs_pred)  +
  plot_annotation(tag_levels = list(c('A')),
                  tag_prefix = "(",
                  tag_suffix = ")")

plot_res_pred <- svm1values |>
  ggplot(aes(pred, res)) +
  geom_point(colour = "#82a1b3", size = 1) +
  xlab("Predicted Logit Correlation Value") +
  ylab("Residuals") +
  theme_clean()

(plot_res_pred)  +
  plot_annotation(tag_levels = list(c('B')),
                  tag_prefix = "(",
                  tag_suffix = ")")

qq_plot <- svm1values |>
  ggplot(aes(sample = res)) +
  stat_qq() +
  theme_clean()

(qq_plot)  +
  plot_annotation(tag_levels = list(c('C')),
                  tag_prefix = "(",
                  tag_suffix = ")")

qqplot(svm1values$pred, svm1values$obs)
abline(a = 0, b = 1, col = "red")

