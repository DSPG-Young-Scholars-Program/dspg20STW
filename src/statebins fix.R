#renderPlot height and width
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


data <- read.csv("src/shiny-dashboard/stwFluid/state_year.csv")
viz_data <- data %>% filter(year == 2019) 

viz_data$value <- ifelse(viz_data$per_change > -70, cbPalette[3], cbPalette[7])

ifelse(per_change <= 0 & per_change > -20, "[0, -20)",
       ifelse(per_change <= -20 & per_change > -40, "[-20, 40)", 
              ifelse(per_change <= -40 & per_change > -60, "[-40, 60)", 
                     ifelse(per_change <= -60 & per_change > -80, "[-60, 80)", 
                            ifelse(per_change <= -80 & per_change > -100, "[-80, -100)", NA)))))





mutate(viz_data, value=  ifelse(per_change <= 0 & per_change > -20, "[0, -20)",
                                ifelse(per_change <= -20 & per_change > -40, "[-20, -40)", 
                                       ifelse(per_change <= -40 & per_change > -60, "[-40, -60)", 
                                              ifelse(per_change <= -60 & per_change > -80, "[-60, -80)", 
                                                     ifelse(per_change <= -80 & per_change > -100, "[-80, -100)", NA)))))) %>%
  statebins(ggplot2_scale_function = scale_fill_manual,
            round = TRUE,
            values = c("[0, -20)" = cbPalette[6], "[-20, -40)" = cbPalette[3],"[-40, -60)" = cbPalette[1], 
                       "[-60, -80)" = cbPalette[2], "[-80, -100)"= cbPalette[7] )) +
  theme_statebins()+
  theme(plot.margin = margin(0,0,0,0),
        legend.position = c(.35, .9),
        legend.justification = c("right", "top"),
        legend.direction =  "horizontal") + 
  labs(fill = "Percent Difference", title = "2019 BGT/JOLTS Percent Difference by State")



