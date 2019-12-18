
install.packages(c("maps", "mapproj"))

estados  <- readRDS("clases/mapas/counties.rds")
source("clases/mapas/helpers.R")

library(maps)
library(mapproj)

var <- estados$white
color <- "darkgreen"
legend.title <- "% Blancos"
min <- 0 # primera entrada del slider (rango)
max <- 100 # segunda entrada del slider (rango)


var <- estados$black
color <- "black"
legend.title <- "% Negros"
min <- 0 # primera entrada del slider (rango)
max <- 100 # segunda entrada del slider (rango)

var <- estados$hispanic
color <- "darkorange"
legend.title <- "% Latinos"
min <- 0 # primera entrada del slider (rango)
max <- 100 # segunda entrada del slider (rango)

var <- estados$white
color <- "darkviolet"
legend.title <- "% Asiaticos"
min <- 0 # primera entrada del slider (rango)
max <- 50# segunda entrada del slider (rango)

percent_map(var, color, legend.title, min, max)
