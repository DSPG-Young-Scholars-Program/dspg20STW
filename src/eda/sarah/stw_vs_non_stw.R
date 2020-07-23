library(dplyr)
library(tidyr)
library(statebins)
library(ggplot2)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

bach <- data.frame()
for(year in 2010:2019){

      tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT A.id) as bgt, 
                          EXTRACT(YEAR FROM A.jobdate) AS year,
                          A.state, A.minedu, CASE WHEN B.cip IS NOT NULL THEN TRUE ELSE FALSE END hascip
                        FROM bgt_job.jolts_comparison_", year, " A LEFT JOIN bgt_job.cip B ON A.id = B.id",
                        " WHERE A.state 
                          IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY A.state, A.minedu, year, hascip",  sep = ""))
    

    tbl$hasbach <- ifelse(is.na(tbl$minedu)==TRUE & tbl$hascip == TRUE, TRUE, 
                          ifelse(is.na(tbl$minedu)==TRUE & tbl$hascip == FALSE, FALSE, 
                                 ifelse(tbl$minedu >14, TRUE, FALSE)))
    
    
   tbl <-  tbl %>% select(bgt, year, state, hasbach) %>% group_by(year, state, hasbach) %>% summarize(bgt = sum(bgt))
    tbl<- tbl %>% spread( key = hasbach, value = bgt) %>% rename(bach = "TRUE", nobach = "FALSE")  
    
    tbl <- tbl %>% mutate(total = nobach + bach, nobach = nobach/total, bach = bach/total) %>% select(!total)
    
    bach <- rbind(tbl, bach)

}




occ <- data.frame()

xwalk <- read.csv("src/edu_knowledge_rothwell/rothwell.csv")

for(year in 2010:2019){

    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT EXTRACT(YEAR FROM A.jobdate) AS year, A.state, B.onet, COUNT(DISTINCT(A.id)) AS bgt
                      FROM bgt_job.jolts_comparison_", year, " A LEFT JOIN bgt_job.main B ON A.id = B.id",
                        " WHERE A.state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY  year, B.onet, A.state", sep = ""))
    
    tbl_original <- tbl
   
    
    tbl<- merge(tbl, xwalk[,c("O.NET.SOC.Code","rothwell_STW")], by.x = "onet", by.y = "O.NET.SOC.Code", all.x = T)
    
    tbl$occ <- substr(tbl$onet, start = 1, stop = 2)
    tbl <- tbl %>% select(!onet)%>% filter(rothwell_STW == 1)  %>% group_by(year, state, occ) %>% summarize(bgt = sum(bgt)) 
    
    tbl <-  tbl  %>% spread(key = occ, value = bgt)
    
    tbl[is.na(tbl)] <- 0
    tbl <- as.data.frame(tbl)

    tbl <- tbl %>% mutate(round(tbl[, -c(1:2)]/rowSums(x = tbl[, -c(1:2)]), 4))
    
    occ <- rbind(occ, tbl)
}

final_data <- merge(bach[, c("state", "year", "nobach")], occ,  by = c("state", "year"))

final_data$nobach <- round(final_data$nobach, 4)

#write.csv(final_data, "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/stw_edu.csv", row.names = F)

final_data <- read.csv("data/ncses_stw/stw_edu.csv")

y = 2019

data <- final_data %>% 
  filter(year == y) %>%
  arrange(desc(nobach))



statebins(data[data$year == y, ], state_col = "state", value_col = "nobach", palette = "Blues", direction = 1, round = TRUE, name = "Percent of Job Ads") + theme_statebins() +
  labs(title = "Percent of BGT Job Ads That Do Not Require a College Degree by State")


