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

validate <- read_csv("data/validate-525.csv") |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1)) |>
  select(u:z, u1:z1, cor_logit)

preProcValues <- readRDS("R/models/preProcValues")
validTransformed <- predict(preProcValues, validate)


svm1 <- readRDS("R/models/SVR/svr-C20-sigma012-3_60mins")

model_predictions <- function(model, my_data = testTransformed) {
  modelpred<-predict(model, my_data)
  data.frame(obs = my_data$cor_logit,
             pred = modelpred,
             res = modelpred-my_data$cor_logit)
}
svm1values <- model_predictions(svm1, validTransformed)

theme_set(theme_bw())

svm1values |>
  mutate(extreme = res>2) |>
  ggplot(aes(res)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 50) +
  ggforce::geom_circle(aes(x0 = 3.42, y0 = 0.02, r = 0.08),
              inherit.aes = FALSE, colour = "red") +
  geom_density(lwd = 0.5, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x = "Residuals from Validation Set of SVR Model Fit") +
  theme_clean() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())


