library(tidyverse)
library(SPADAR)
data(uhecrauger2014)

z <- bind_rows(
  read_csv("data/train-test-3556.csv"),
  read_csv("data/validate-525.csv"),
  read_csv("data/validate-extra-898.csv")
)

ps <- 0.3
my_mainGrid="galactic"
my_projname = "mollweide"
my_dataCoordSys = "equatorial"



createAllSkyScatterPlotChart(uhecrauger2014$RA, uhecrauger2014$DEC,
                             mainGrid = my_mainGrid,
                             projname = my_projname,
                             dataCoordSys = my_dataCoordSys,
                             pointcol="white", pch=0,
                             pointsize= 0,
                             label.cex = 0,
                             eclDraw=F, eqDraw = F, galDraq=T,
                             eqLty=2, galLty=1,
                             galCol="black", eqCol=rgb(1,0,0,0.5),
                             main = "")

overplotScatterPlotInAllSkyGridChart(z$ra, z$dec,
                                     mainGrid = my_mainGrid,
                                     projname = my_projname,
                                     dataCoordSys = my_dataCoordSys,
                                     pointcol="darkblue", pch=19,
                                     pointsize= ps,
                                     eclDraw=F, eqDraw = F, galDraq=T,
                                     eqLty=2, galLty=1,
                                     galCol="black", eqCol=rgb(1,0,0,0.5))
overplotScatterPlotInAllSkyGridChart(z$ra1, z$dec1,
                                     mainGrid = my_mainGrid,
                                     projname = my_projname,
                                     dataCoordSys = my_dataCoordSys,
                                     pointcol="lightseagreen", pch=19,
                                     pointsize= ps,
                                     eclDraw=F, eqDraw = F, galDraq=T,
                                     eqLty=2, galLty=1,
                                     galCol="black", eqCol=rgb(1,0,0,0.5))

# z |> ggplot(aes(number_of_matches)) + geom_histogram(bins = 50)
