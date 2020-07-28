library(ggplot2)
library(lubridate)

data <- read.csv("src/shiny-dashboard/stwFluid/per_diff_state.csv")
data$time <- as_date(parse_date_time(data$date, "ym"))

ggplot(subset(data, State %in% c("Utah"))) + 
  geom_line(aes(x=time, y=per_diff),color="#E57200")  + 
  theme_minimal() +
   scale_y_continuous(limits = c(-25, 200)) +
    labs(title = "Percent Difference", x = "", y = "") 



