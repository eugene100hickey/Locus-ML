library(tidyverse)
library(glue)
library(showtext)
library(patchwork)
library(ggtext)

star_pairs <- read_csv("data/validate-525.csv") |>
  arrange(cor) |>
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1),
         objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))


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

get_spectrum <- function(index = 1,
                         is_match = TRUE,
                         wavelength_lower_limit = 3000,
                         wavelength_upper_limit = 10000){

  object = star_pairs[index,]

  if(is_match){
    plate <- object$plate
    mjd <- object$mjd
    fiber <- object$fiberid
  } else{
    plate <- object$plate1
    mjd <- object$mjd1
    fiber <- object$fiberid1
  }

  url_spect <- glue("http://dr12.sdss.org/csvSpectrum?plateid={plate}",
                    "&mjd={mjd}&fiber={fiber}&reduction2d=v5_7_0")
  spectrum <- read_csv(file = url_spect)
  spectrum |>
    filter(between(Wavelength, wavelength_lower_limit, wavelength_upper_limit)) %>%
    select(Wavelength, BestFit) |>
    head(-20)
}

spectrum_plot <- function(index = 1,
                          is_match = TRUE,
                          label_height = 0.5,
                          mag_label_height = 0.1) {
  object <- star_pairs[index,]
  specobjid <- ifelse(is_match, object$specobjid1, object$specobjid)
  u <- ifelse(is_match, object$u1, object$u)
  g <- ifelse(is_match, object$g1, object$g)
  r <- ifelse(is_match, object$r1, object$r)
  i <- ifelse(is_match, object$i1, object$i)
  z <- ifelse(is_match, object$z1, object$z)
  spectrum <- get_spectrum(index, is_match)
  spectrum |>
    ggplot(aes(Wavelength/10, BestFit)) +
    geom_line(colour = "gray70") +
    geom_line(data = spectrum |> filter(between(Wavelength, 5500, 7000)),
              aes(Wavelength/10, BestFit),
              colour = "black",
              size = 1.5) +
    scale_x_continuous(breaks = seq(400, 1000, by = 100)) +
    labs(x = "Wavelength (nm)",
         y = "Flux (erg/cm²/s Å)") +
    annotate("text",
             x = 800,
             y = max(spectrum$BestFit) * label_height,
             family = "Fuzzy Bubbles",
             size = 8,
             label = glue("specobjid\n{as.character(specobjid)}")) +
    annotate("text",
             x = 620,
             y = min(spectrum$BestFit) * 0.9 + max(spectrum$BestFit) * mag_label_height,
             family = "Fuzzy Bubbles",
             size = 10,
             label = glue("u = {round(u, 2)}, g = {round(g, 2)}, r = {round(r, 2)}, i = {round(i, 2)}, z = {round(z, 2)}")) +
    coord_cartesian(xlim = c(320, 1000)) +
    theme_clean() +
    theme(axis.title.y = element_text(size = 20))
}

plot_CV <- spectrum_plot(index = 11,
                         is_match = F,
                         label_height = 0.7,
                         mag_label_height = -0.05) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot_CV_match <- spectrum_plot(index = 11,
                               is_match = T,
                               label_height = 0.8,
                               mag_label_height = -0.03) +
  ylim(0, 40)

(plot_CV / plot_CV_match)  +
  plot_annotation(tag_levels = list(c('A', 'B')),
                  tag_prefix = "(",
                  tag_suffix = ")")
