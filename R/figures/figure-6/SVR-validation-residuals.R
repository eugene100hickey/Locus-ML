library(tidyverse)
library(caret)
library(patchwork)
library(showtext)
library(ggokabeito)
library(viridis)
library(ggforce)

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

validate <- read_csv("data/validate-525.csv") |>
  mutate(objid = as.character(objid),
         objid1 = as.character(objid1),
         specobjid = as.character(specobjid),
         specobjid1 = as.character(specobjid1)) |>
  select(u:z, u1:z1, cor_logit)

preProcValues <- readRDS("R/models/preProcValues")

validateTransformed <- predict(preProcValues, validate)

svm1 <- readRDS("R/models/SVR/svr-C20-sigma012-3_60mins")
svm1

svm1valpred<-predict(svm1, validateTransformed)
svm1values<-data.frame(obs=validateTransformed$cor_logit,
                       pred=svm1valpred,
                       res=svm1valpred-validateTransformed$cor_logit)
yardstick::metrics(data = svm1values, truth = obs, estimate = pred)

index <- 315
plot1 <- ggplot(svm1values, aes(y=obs,
                       x=pred,
                       colour=abs(res))) +
  geom_point(alpha=0.9, show.legend = F, size = 0.8) +
#  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+
  geom_abline(slope=1, linetype="dashed") +
  ggforce::geom_circle(aes(x0 = svm1values$pred[index],
                           y0 = svm1values$obs[index],
                           r = 0.08),
                       inherit.aes = FALSE, colour = "red") +
  scale_color_gradient(low = "#7BA0B4", high = "#0A2D46") +
  labs(y = "Observered Logit Correlation Value",
       x = "Predicted Logit Correlation Value") +
  theme_clean() +
  theme(axis.title = element_text(size = 36),
        axis.text = element_text(size = 36))

plot2 <- ggplot(svm1values, aes(y = res,
                       x = pred)) +
  geom_point(alpha=0.9, show.legend = F, col = "#44728C", size = 0.8) +
  labs(y = "Residuals",
       x = "Predicted Logit Correlation Value") +
  theme_clean() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

plot3 <- svm1values %>%
  ggplot(aes(sample = res)) +
  geom_qq(size = 0.5) +
  scale_color_viridis_d() +
  theme_clean()


layout <- "
AAAA#CCCC
BBBB#CCCC
"
plot1 + plot2 + plot3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ") ")
