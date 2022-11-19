library(tidyverse)
library(glue)
library(RCurl)
library(gtools)
library(progress)


# SQL that downloads some info on the chosen target from SDSS.
# ObjID from SDSS specifies the target
# set search parameters
column_names <- c("ra", "dec", "u", "g", "r", "i", "z", "objid", "specobjid",
                  "class", "subclass","survey", "plate","mjd", "fiberid",
                  "ra1", "dec1", "u1", "g1", "r1", "i1", "z1", "objid1", "specobjid1",
                  "class1", "subclass1", "survey1", "plate1", "mjd1","fiberid1",
                  "cor", "number_of_matches")

delta <- 0.3
bands_min <- 15
bands_max <- 20


get_spectrum <- function(object, wavelength_lower_limit = 5500, wavelength_upper_limit = 7000){
  plate <- object$plate
  mjd <- object$mjd
  fiber <- object$fiberid
  url_spect <- glue("http://dr12.sdss.org/csvSpectrum?plateid={plate}",
                    "&mjd={mjd}&fiber={fiber}&reduction2d=v5_7_0")
  spectrum <- read_csv(file = url_spect, show_col_types = FALSE)
  spectrum %>%
    filter(between(Wavelength, wavelength_lower_limit, wavelength_upper_limit)) %>%
    select(Wavelength, BestFit)
}

# master_target_SqlQuery <- glue("SELECT p.ra, p.dec, ",
#                                "p.u, p.g, p.r, p.i, p.z, p.objid, ",
#                                "s.specobjid, s.class, s.subclass, s.survey, ",
#                                "s.plate, s.mjd, s.fiberid ",
#                                "FROM photoObj AS p ",
#                                "JOIN SpecObj AS s ON s.bestobjid = p.objid ",
#                                "WHERE p.g BETWEEN {bands_min} AND {bands_max} ",
#                                "AND p.r BETWEEN {bands_min} AND {bands_max} ",
#                                "AND p.i BETWEEN {bands_min} AND {bands_max} ",
#                                "AND s.class = 'STAR' ",
#                                "AND p.clean = 1 AND (p.calibStatus_r & 1) != 0",
#                                "AND s.survey != 'eboss'",
#                                "AND p.psfmagerr_u < 0.05",
#                                "AND p.psfmagerr_g < 0.05",
#                                "AND p.psfmagerr_r < 0.05",
#                                "AND p.psfmagerr_i < 0.05",
#                                "AND p.psfmagerr_z < 0.05" )
#
# # downloads target data
# # dataframe target has necessary info
# master_target_SqlQuery <- str_squish(master_target_SqlQuery)
# urlBase <- "http://skyserver.sdss.org/dr15/SkyserverWS/SearchTools/SqlSearch?"
# X <- getForm(urlBase, cmd = master_target_SqlQuery, format = "csv")
# master_targets <- read.table(text = X, header = TRUE, sep = ",", dec = ".", comment.char = "#") %>%
#   mutate(objid = as.character(objid),
#          specobjid = as.character(specobjid)) |>
#   filter(!duplicated(objid), !duplicated(specobjid))

master_targets <- read_csv("data/stars-with-spectra-200949.csv")

get_correlation <- function(index = 8){
  pb$tick()
  delta_gr_mag <- master_targets$g[index] - master_targets$r[index]
  delta_ri_mag <- master_targets$r[index] - master_targets$i[index]
  plate <- master_targets$plate[index]

  match1 <- z |>
    filter(between((g-r), delta_gr_mag, (delta_gr_mag+delta)),
           between((r-i), delta_ri_mag, (delta_ri_mag+delta)))
  match <- match1[sample(1:nrow(match1), size = 1),]
  names(match) <- glue::glue("{names(match)}1")
  spect1 <- get_spectrum(master_targets[index,])
  spect2 <- get_spectrum(match)
  cor_value <- cor(spect1$BestFit, spect2$BestFit)
  bind_cols(star_1=match, star_2=master_targets[index,], cor = cor_value, n = nrow(match1))
}
z <- master_targets
# Wed Apr 22 18:41:31 2020 ------------------------------
pb <- progress_bar$new(total = 1000,
                       format = "[:bar] :percent finish in :eta")
pb$tick(0)

set.seed(42)
indices <- sample(1:nrow(z), 5500)
master_targets <- z[indices,]
get_correlation_safely <- safely(get_correlation)
(t1 <- Sys.time())
z1 <- map(1:nrow(master_targets), ~get_correlation_safely(.x)) %>%
  map_df("result") %>%
  compact()
(t2 <- Sys.time())
t2-t1
names(z1) <- column_names
z1 <- z1 %>%
  mutate(cor_logit = logit(cor, min = -1, max = 1)) %>%
  mutate(objid = as.character(objid),
         specobjid = as.character(specobjid),
         objid1 = as.character(objid1),
         specobjid1 = as.character(specobjid1))


sum(duplicated(z1$objid))
sum(duplicated(z1$objid1))
objids <- c(z1$objid, z1$objid1)
specobjids <- c(z1$specobjid, z1$specobjid1)
z2 <- z1 |>
  filter(!duplicated(objid), !duplicated(objid1)) |>
  filter(!(objid1 %in% objid), !(objid %in% objid1)) |>
  filter(!(specobjid1 %in% specobjid), !(specobjid %in% specobjid1))

write_csv("data/my-data-file.csv")
