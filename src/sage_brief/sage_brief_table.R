
library(dplyr)
library(tidyr)
xwalk <- read.csv("/sfs/qumulo/qhome/sm9dv/dspg20STW/src/edu_knowledge_rothwell/rothwell.csv")

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))



bgt <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = paste("SELECT COUNT(DISTINCT(A.id)), A.state, A.minedu, B.onet
  FROM bgt_job.jolts_comparison_2019 A
  LEFT JOIN bgt_job.main B
  ON A.id = B.id
  WHERE A.state IN ",  paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                    " GROUP BY A.state, A.minedu, B.onet", sep = ""))

bgt <- bgt %>% spread(key = minedu, value = count)


states <- data.frame(state.name, state.region)
states <- rbind(states, c("District of Columbia", "South"))
levels(states$state.region)[levels(states$state.region)=="North Central"] <- "Midwest"


bgt <- merge(bgt, states, by.x = "state", by.y = "state.name")

bgt_cip <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = paste("SELECT COUNT(DISTINCT(A.id)) AS NAcips, A.state,  B.onet
  FROM bgt_job.jolts_comparison_2019 A
  LEFT JOIN bgt_job.main B
  ON A.id = B.id
  LEFT JOIN bgt_job.cip C
  ON A.id = C.id
  WHERE C.cip IS NOT NULL AND A.minedu IS NULL AND A.state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  
                    " GROUP BY A.state, B.onet",
                    sep = ""))

bgt <- merge(bgt, bgt_cip, by = c("state", "onet"), all.x = T)

bgt$nacips <- ifelse(is.na(bgt$nacips)==T, 0,  bgt$nacips)
bgt$`0` <- ifelse(is.na(bgt$`0`)==T, 0,  bgt$`0`)
bgt$`12` <- ifelse(is.na(bgt$`12`)==T, 0,  bgt$`12`)
bgt$`14` <- ifelse(is.na(bgt$`14`)==T, 0,  bgt$`14`)
bgt$`16` <- ifelse(is.na(bgt$`16`)==T, 0,  bgt$`16`)
bgt$`18` <- ifelse(is.na(bgt$`18`)==T, 0,  bgt$`18`)
bgt$`21` <- ifelse(is.na(bgt$`21`)==T, 0,  bgt$`21`)
bgt$`<NA>` <- ifelse(is.na(bgt$`<NA>`)==T, 0,  bgt$`<NA>`)


bgt$N <- rowSums(bgt[, c("0", "12", "14","16","18", "21", "<NA>")])



bgt <- bgt %>%
  select(-state) %>%
  group_by(state.region, onet) %>%
  summarise("N" = sum(N),
            "minedu_0" = sum(`0`),
            "minedu_12" = sum(`12`),
            "minedu_14" = sum(`14`),
            "minedu_16" = sum(`16`),
            "minedu_18" = sum(`18`),
            "minedu_21" = sum(`21`),
            "minedu_<NA>" = sum(`<NA>`),
            "nacips" = sum(nacips))


bgt <- merge(bgt, xwalk[,c("X2010.SOC.Code", "O.NET.SOC.Code", "Title","Percent.No.Bachelors", "rothwell_STW" )], by.x = "onet", by.y = "O.NET.SOC.Code", all.x = T)

stw <- bgt %>% filter(rothwell_STW == 1)

#write.csv(stw, "src/sage_brief/stw_edu_region.csv", row.names = F)


