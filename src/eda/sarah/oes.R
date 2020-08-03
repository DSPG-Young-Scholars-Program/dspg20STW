#download.file("https://www.bls.gov/oes/special.requests/oesm19nat.zip", destfile = "oes19nat.zip")
#unzip("oes19nat.zip")
library(readxl)
data <- read_xlsx("data/ncses_stw/original/oes/oesm19nat/national_M2019_dl.xlsx")

library(dplyr)
maj <-data %>% filter(o_group == "major"|o_group == "total") %>%
  select(occ_code, tot_emp)
maj$per_oes <- (maj$tot_emp/as.numeric(maj[maj$occ_code == "00-0000", "tot_emp"]))* 100
maj <- maj[-1,]
maj$occ_code <- substr(maj$occ_code, 1, 2)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

bgt <- data.frame()
for(year in 2019){
  library(dplyr)
  library(tidyr)
  
  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, SUBSTRING(soc from 1 for 2) AS occ, COUNT(DISTINCT(id)) AS bgt
                      FROM bgt_job.jolts_comparison_", year, 
                      " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                      " GROUP BY  year, occ", sep = ""))
  
  bgt <- rbind(bgt, tbl)
}

bgt$per <- (bgt$bgt/sum(bgt$bgt))* 100


table <- merge(maj, bgt, by.x= "occ_code", by.y = "occ")


library(ggplot2)
library(ggrepel)
ggplot(table, aes(x = per, y = per_oes, label = occ_code)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:14, limits = c(0, 14)) +
  scale_y_continuous(breaks = 0:14, limits = c(0, 14))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment Major SOC Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates")



