library(ggplot2)
library(lubridate)
library(dplyr)

region_per_diff <- read.csv("src/shiny-dashboard/stwFluid/regional_per_diff.csv")

region_per_diff$National = sprintf(region_per_diff$National, fmt = "%#.3f")
