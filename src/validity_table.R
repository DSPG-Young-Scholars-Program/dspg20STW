
data<- data.frame(Variable =c("id","jobdate", "state","soc", "socname", "lat", "long", "minedu", "maxedu"), 
           `Validity` = c("the observation is unique, i.e. does not appear more than once",
                          "the observation is in the format YYYY-MM-DD and contained within the range of January 1st for December 31st of the specified year",
                          "the observation is one of the 50 states, the District of Columbia, or a U.S. territory",
                          "the observation has seven character",
                          "the observation is not a numeric string",
                          "the observation is greater than zero",
                          "the observation is less than zero",
                          "the observation is either 0, 12, 14, 16, 18, or 21", 
                          "the observation is either 0, 12, 14, 16, 18, or 21"))
View(data)
write.csv(data, "src/shiny-dashboard/stwFluid/validity_table.csv", row.names = F)



# make region map

library(statebins)
states <- data.frame(state.name, state.region)
states <- rbind(states, c("District of Columbia", "South"))
levels(states$state.region)[levels(states$state.region)=="North Central"] <- "Midwest"

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


statebins(state_data = states, state_col = "state.name", value_col = "state.region", ggplot2_scale_function = scale_fill_manual,
          font_size = 5, 
          round = TRUE,
          values = c("South" = cbPalette[6], "Midwest" = cbPalette[4],"Northeast" = cbPalette[7], 
                     "West" = cbPalette[2])) +
  theme_statebins()+
  theme(plot.margin = margin(0,0,0,0), 
        legend.position = "none") 

