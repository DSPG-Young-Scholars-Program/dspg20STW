library(ggplot2)
library(lubridate)
library(dplyr)

prof <- read.csv("src/shiny-dashboard/stwFluid/prof.csv")

x <- head(prof)

x$completeness = x$completeness * 100


