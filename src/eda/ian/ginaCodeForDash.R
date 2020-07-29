library(ggplot2)
library(lubridate)
library(dplyr)

x <- read.csv("src/shiny-dashboard/stwFluid/statebinsData.csv")

x$per_diff = sprintf(x$per_diff, fmt = "%#.2f")

