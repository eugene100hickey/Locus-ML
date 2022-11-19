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

models_compare <- resamples(list(
  SVM = svm1,
  RF = rf1,
  GBM = gbm1,
  GLMnet = glmnet1,
  LINEAR = lm1
))


model.names <- c("SVM", "RF", "GBM",
                 "GLMnet",
                 "LINEAR")
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"), cex = 2)
bwplot(models_compare,
       scales=scales,
       layout = c(1, 2),
       par.strip.text = list(cex = 2),
       cex = 1)
