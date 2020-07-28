library(tidyr)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)

###########--------------- Comparison of Total Jobs by Year ---------------########### 

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))


compare_years <- function(years){
  
  total <- data.frame(
    year = years, 
    variable = c(rep("jolts", length(years)), rep ("bgt", length(years))) , 
    value = numeric(length = 2 * length(years)))
  
  jolts <- read.table("data/ncses_stw/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  
  for(y in years){
    
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT id) FROM bgt_job.jolts_comparison_", y, " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  sep = ""))
    

    total[total$variable == "bgt" & total$year == y, "value"] <- tbl[, "count"]
    
    tbl2 <- jolts %>% 
      filter(series_id == "JTU00000000JOL" & year == y) %>%
      select(series_id, year, value) %>%
      mutate(value = value * 1000) %>%
      group_by(year) %>% 
      summarize(JobOpenings = sum(value))
    
    total[total$year == y & total$variable == "jolts", "value"] <- tbl2[tbl2$year == y, 'JobOpenings']
    
  }
  total_wide <<- spread(total, variable, value)
  total <<- total
}

compare_years(2010:2019)



###########--------------- Comparison of Total Jobs by Year and Region ---------------########### 

  
compare_years_region <- function(years){
  
  # empty data frame to fill in 
  total <- data.frame(
    region = rep(rep(c("South", "Northeast", "West", "Midwest"),10), each = 2), 
    year = rep(rep(years, each = 4), each = 2), 
    variable = rep(c("jolts", "bgt"), 40), 
    value = numeric(length = 80))
  
  # region abbr to region name look up table
  lookup <- data.frame(region = c("NE", "SO", "WE", "MW"), name = c("Northeast", "South", "West", "Midwest"))
  
  # open jolts
  jolts <- read.table("data/ncses_stw/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  # state to region look up table
  states <- data.frame(state.name, state.region)
  states <- rbind(states, c("District of Columbia", "South"))
  levels(states$state.region)[levels(states$state.region)=="North Central"] <- "Midwest"
  
  
  for(y in years){

    # jolts
    
    region <- jolts %>% 
      filter(grepl(pattern = "JTU.+\\D{2}JOL", x = series_id) & year == y) %>%
      mutate(region = lookup$name[match(substr(series_id, start= 10, stop = 11), lookup$region)] ) %>%
      select(series_id, year, value,region)%>%
      select(year, region, value) %>%
      group_by(year, region) %>%
      summarise(value = sum(value) * 1000)
    
    total[total$year == y & total$variable == "jolts", "value"] <- region$value[match(total[total$year == y & total$variable == "jolts", "region"], region$region)]
    
    # burning glass
    
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT id), state 
                    FROM bgt_job.jolts_comparison_", y, 
                        " WHERE state 
                      IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY state",  sep = ""))
    
    tbl <- merge(tbl, states, by.x = "state", by.y = "state.name")
    
    tbl <- tbl %>%
      select(state.region, count) %>%
      group_by(state.region) %>%
      summarise(value = sum(count)) %>%
      rename(region = state.region)
    
    total[total$year == y & total$variable == "bgt", "value"] <- tbl$value[match(total[total$year == y & total$variable == "bgt", "region"], tbl$region)]

    
  }
  
  total_region <<- total
  total_wide_region <<- spread(total, variable, value)
}

compare_years_region(2010:2019)


###########--------------- PLOTS ---------------########### 

# segment time chart:  National count comparison of BGT and JOLTS by year
ggplot(total_wide, aes(x= year, xend = year, y = bgt, yend = jolts)) + 
  geom_segment(color = "grey60") + 
  geom_point(y = total_wide$bgt, color = "#E57200", size = 3)+
  geom_point(y = total_wide$jolts, color = "#232D4B", size = 3) +
  scale_x_continuous(breaks = 2010:2019, 
                     limits =c(2010,2020)) + 
  scale_y_continuous(breaks = seq(0, 90000000, by = 10000000), 
                     labels = scales::comma,
                     name ="Number of Job Openings/Ads",
                     limits = c(0, 90000000),
                     expand = c(0, 0))+
  theme_minimal() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        plot.title = element_text(hjust = .5, size = 20), 
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(), 
        strip.text.x = element_text(face = "bold",size = 12))+
  labs(y = '', 
       x = "", 
       title = "BGT Job Ads vs JOLTS Job Openings by Year",
       subtitle= "Blue dots show JOLTS job openings estimates, \nand orange dots show BGT job-ads estimates.")


# line chart: Regional count comparison of BGT and JOLTS by year
ggplot(total_region, aes(x = year, y = value, group=region, color = region)) +
  geom_line() + 
  theme_minimal() + 
  scale_x_continuous(breaks = 2010:2019, 
                     limits =c(2010,2019)) + 
  scale_y_continuous(breaks = seq(0, 40000000, by = 10000000), 
                     labels = c( "0", paste(seq(10, 40, by = 10), "million")), 
                     limits = c(0, 40000000),
                     expand = c(0, 0)) +
  facet_grid(~variable)

# Segment time chart: Regional count comparison of BGT and JOLTS by year
ggplot(total_wide_region, aes(x = year, xend = year, y = bgt, yend = jolts)) + 
  geom_segment(color = "grey60") +
  #geom_text(aes(x = year+0.32, y = bgt+ ((jolts-bgt)/2), label = paste(round(per_diff, 2), "%", sep = ""))) +
  geom_point(y = total_wide_region$bgt, color = "#E57200", size = 3)+
  geom_point(y = total_wide_region$jolts, color = "#232D4B", size = 3) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 30000000, 5000000)) +  
  scale_x_continuous(breaks = c(2010:2019)) + 
  scale_color_manual(values=c("#E57200", "#232D4B")) +
  facet_wrap(~region) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5, size = 20), 
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(), 
        legend.position = "none", 
        strip.text.x = element_text(face = "bold",size = 12)) +
  labs(title = "BGT Job Ads vs JOLTS Job Openings by Region and Year",
       y = "Number of Job Openings/Ads",
       subtitle = "Blue dots represent JOLTS Job Openings Estimates, \nand orange dots represent BGT Job Ads.") 

# bar chart: BGT vs JOLTS, by year and region
var.names <- as_labeller(c("bgt" = "BGT", "jolts" = "JOLTS"))
ggplot(total_region, aes(x=year, y=value, fill=region)) +
  geom_bar(stat="identity") +  
  scale_y_continuous(labels = scales::comma, name ="Number of Job Openings/Ads", seq(0, 90000000, by = 10000000)) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "BGT Job Ads vs JOLTS Job Openings by Region and Year", 
       x = ""
  ) +
  scale_fill_manual(values = c("#E57200","#232D4B","#009FDF","#FDDA24")) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        plot.title = element_text(hjust = .5, size = 20), 
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(), 
        strip.text.x = element_text(face = "bold",size = 12))+
  facet_wrap(~variable, labeller=var.names)



#------------------------------------------PERCENT DIFFERNCE-----------------------------------------#

total_wide$per_diff <- (abs(total_wide$jolts - total_wide$bgt)/((total_wide$jolts + total_wide$bgt)/2))*100

total_wide_region$per_diff <- (abs(total_wide_region$jolts - total_wide_region$bgt)/((total_wide_region$jolts + total_wide_region$bgt)/2))*100

# line graph: national percent difference over time
ggplot(total_wide, aes(x = year, y = per_diff)) +
  geom_line() + 
  scale_y_continuous(limits = c(0, 120)) +
  scale_x_continuous(breaks= 2010:2019) +
  theme_minimal()

total_wide$region <- "National"
total_wide_region <- rbind(total_wide, total_wide_region)

# line graph: regional percent differnce over time

ggplot(total_wide_region, aes(x = year, y = per_diff, group = region, color = region)) +
  geom_line() + 
  scale_x_continuous(breaks= 2010:2019) +
  scale_y_continuous(limits = c(50, 120)) +
  theme_minimal()

# a simple table to see percent difference by year and region
region_per_diff <-total_wide_region %>% select(year, per_diff, region) %>% spread(key = region, value = per_diff) %>% select("Year" = year, National, Northeast, Midwest, South, West)
region_per_diff

# will need this for shiny dashboard
#write.csv(total_wide, "src/shiny-dashboard/stwFluid/jobsByYear.csv", row.names = F)
#write.csv(total_wide_region, "src/shiny-dashboard/stwFluid/total_wide_region.csv", row.names = F)
#write.csv(region_per_diff, "src/shiny-dashboard/stwFluid/regional_per_diff.csv", row.names = F)




