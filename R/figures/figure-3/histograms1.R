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
          text = element_text(size = 16, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 24),
          axis.title = element_text(face = "bold", size = 28),
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

## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% dplyr::select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% dplyr::select(u:z, u1:z1, cor_logit)
training_hist <- z1[ inTraining,]
x = training %>% dplyr::select(u:z, u1:z1)
y = training$cor_logit
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

hist1 <- training_hist |>
  ggplot(aes(cor)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "white",
                 colour = 1) +
  # coord_cartesian(xlim = c(0.8, 1.0)) +
  geom_density(lwd = 0.5, colour = 4,
               fill = 4, alpha = 0.25) +
  xlim(0.5, 1) +
  labs(x = "Pearson Correlation", y = "Density") +
  theme_clean() +
  theme(axis.text.y = element_blank())

hist_logit <- trainTransformed |>
  ggplot(aes(cor_logit)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "white",
                 colour = 1) +
  geom_density(lwd = 0.5, colour = 4,
               fill = 4, alpha = 0.25, bw = 0.5) +
  # xlim(0.5, 1) +
  labs(x = "Logit Correlation", y = "Density") +
  theme_clean() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

hist1 +
  hist_logit +
  plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ") ")
