#download.file("https://www.bls.gov/oes/special.requests/oesm19nat.zip", destfile = "oes19nat.zip")
#unzip("oes19nat.zip")

# download all oes files 2010-2019

#for (i in 10:11){

#download.file(paste("https://www.bls.gov/oes/special.requests/oesm", i, "nat.zip", sep = ""), destfile = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "nat.zip", sep = ""))
#unzip(paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "nat.zip", sep= ""), exdir = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "nat/", sep = ""))

#}

#for (i in 12:19){
  
 # download.file(paste("https://www.bls.gov/oes/special.requests/oesm", i, "nat.zip", sep = ""), destfile = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "nat.zip", sep = ""))
#  unzip(paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "nat.zip", sep= ""), exdir = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/")
  
#}


### note: on national estimates: These estimates are calculated with data collected from employers in all industry 
### sectors in metropolitan and nonmetropolitan areas in every state and the District of Columbia.
### some job ads have NA for SOC code

#---------------------- NATIONAL COMPARISONS MAJOR OCCUPATIONS -------------------------------------#
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggrepel)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))


oes_maj <- data.frame()
for(y in 2010:2019){
  if(y %in% (2010:2013)){
    data <- read_xls(paste("data/ncses_stw/original/oes/oesm", substr(y, 3,4), "nat/national_M", y, "_dl.xls", sep = ""))
    
  } else{
    data <- read_xlsx(paste("data/ncses_stw/original/oes/oesm", substr(y, 3,4), "nat/national_M", y, "_dl.xlsx", sep = ""))
    
  }
  
  names(data) <- tolower(names(data))
  
  if("group" %in% colnames(data)){
    colnames(data)[colnames(data) == "group"] <- "o_group"
  } else if ("occ_group" %in% colnames(data)){
    colnames(data)[colnames(data) == "occ_group"] <- "o_group"
  }
    
  
  grand_total <- as.numeric(data[data$occ_code == "00-0000", "tot_emp"])
  
  oes <-data %>% filter(o_group == "major") %>%
    select(occ_code, tot_emp) %>%
    mutate(per_oes = (tot_emp/grand_total) * 100, 
           maj_occ_code = substr(occ_code, 1, 2), 
           year = y) %>%
    select(!occ_code)
  
  oes_maj <- rbind(oes_maj, oes)

}


### note: 55- SOCs not included
bgt_maj <- data.frame()
for(y in 2010:2019){
  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, SUBSTRING(soc from 1 for 2) AS maj_occ_code, soc AS occ_code, COUNT(DISTINCT(id)) AS bgt
                      FROM bgt_job.jolts_comparison_", y, 
                      " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                      " GROUP BY  year, maj_occ_code, occ_code", sep = ""))
  
  tbl <- tbl %>% select(year, maj_occ_code, bgt) %>% 
    group_by(year, maj_occ_code) %>% 
    summarize(bgt = sum(bgt)) %>%
    mutate(per_bgt = (bgt/sum(bgt)) * 100)
  
  bgt_maj <- rbind(bgt_maj, tbl)
}




national_maj <- merge(bgt_maj, oes_maj, by = c("maj_occ_code", "year"), all = T)

ggplot(national_maj[national_maj$year == 2014, ], aes(x = per_bgt, y = per_oes, label = maj_occ_code)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:17, limits = c(0, 17)) +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment \nby Major Occupation Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates") +
  coord_fixed(ratio = 1)

#write.csv(national_maj, "/sfs/qumulo/qhome/sm9dv/dspg20STW/src/shiny-dashboard/stwFluid/oes_national_maj.csv", row.names = F)

#---------------------- STATE COMPARISONS MAJOR OCCUPATIONS -------------------------------------#

# download files

#for (i in 14:19){
#  download.file(paste("https://www.bls.gov/oes/special.requests/oesm", i,"st.zip", sep = ""), destfile = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "st.zip", sep = ""))
#  unzip(paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "st.zip", sep= ""), exdir = "/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/")
#}

#for (i in 10:13){
#  download.file(paste("https://www.bls.gov/oes/special.requests/oesm", i, "st.zip", sep = ""), destfile = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "st.zip", sep = ""))
#  unzip(paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "st.zip", sep= ""), exdir = paste("/sfs/qumulo/qhome/sm9dv/dspg20STW/data/ncses_stw/original/oes/oesm", i, "st/", sep = ""))
#}

# ** converted to NA: indicates that an employment estimate is not available

oes_maj_st <- data.frame()
for(y in 2010:2019){

  if(y %in% (2010:2013)){
    data <- read_xls(paste("data/ncses_stw/original/oes/oesm", substr(y, 3,4), "st/state_M", y, "_dl.xls", sep = ""), na = "**")
    
  } else{
    data <- read_xlsx(paste("data/ncses_stw/original/oes/oesm", substr(y, 3,4), "st/state_M", y, "_dl.xlsx", sep = ""), na = "**")
    
  }
  
  
  names(data) <- tolower(names(data))
  
  if("group" %in% colnames(data)){
    colnames(data)[colnames(data) == "group"] <- "o_group"
  } else if ("occ_group" %in% colnames(data)){
    colnames(data)[colnames(data) == "occ_group"] <- "o_group"
  }
  
  if("state" %in% colnames(data)){
    colnames(data)[colnames(data) == "state"] <- "area_title"
  }
  
  
  grand_total <- data %>% filter(o_group == "total") %>%
    select(area_title, tot_emp)
  
  oes <-data %>% filter(o_group == "major") %>%
    select(area_title, occ_code, tot_emp) %>%
    mutate(per_oes = (as.numeric(tot_emp)/as.numeric(grand_total$tot_emp[match(area_title, grand_total$area_title)]))*100, 
           maj_occ_code = substr(occ_code, 1, 2), 
           year = y) %>%
    select(!occ_code)
  
  oes_maj_st <- rbind(oes_maj_st, oes)
  
}

### note: 55- SOCs not included
### should we use compare Guam, virgin islands etc
bgt_maj_st <- data.frame()
for(y in 2010:2019){

  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, state, SUBSTRING(soc from 1 for 2) AS maj_occ_code, soc AS occ_code, COUNT(DISTINCT(id)) AS bgt
                      FROM bgt_job.jolts_comparison_", y, 
                      " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                      " GROUP BY  year, state, maj_occ_code, occ_code", sep = ""))
  
  tbl <- tbl %>% select(year, state, maj_occ_code, bgt) %>% 
    group_by(year, state, maj_occ_code) %>% 
    summarize(bgt = sum(bgt)) %>%
    mutate(per_bgt = (bgt/sum(bgt)) * 100)
  
  bgt_maj_st <- rbind(bgt_maj_st, tbl)
}

colnames(oes_maj_st)[colnames(oes_maj_st) == "area_title"] <- "state"

state_maj <- merge(bgt_maj_st, oes_maj_st, by = c("maj_occ_code", "year", "state"), all = T)

state_maj <- read.csv("/sfs/qumulo/qhome/sm9dv/dspg20STW/src/shiny-dashboard/stwFluid/oes_state_maj.csv")
state <- data.frame(abb = c(state.abb, "DC"), state = c(state.name, "District of Columbia"), region = c(as.character(state.region), "South"))
state$region[state$region=="North Central"] <- "Midwest"

state_maj$abb <- state$abb[match(state_maj$state, state$state)]
state_maj$region <- state$region[match(state_maj$state, state$state)]

state_maj <- state_maj %>% filter(state %in% c(state.name, "District of Columbia"))

#write.csv(state_maj, "/sfs/qumulo/qhome/sm9dv/dspg20STW/src/shiny-dashboard/stwFluid/oes_state_maj.csv", row.names = F)

ggplot(state_maj[state_maj$year == 2019 & state_maj$state == "Alabama", ], aes(x = per_bgt, y = per_oes, label = maj_occ_code)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:35, limits = c(0, 35)) +
  scale_y_continuous(breaks = 0:35, limits = c(0, 35))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment \nby Major Occupation Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates") +
  coord_fixed(ratio = 1)





ggplot(state_maj[state_maj$year == 2019 & state_maj$maj_occ_code == 51, ], aes(x = per_bgt, y = per_oes, label = abb, color = region)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:35, limits = c(0, 35)) +
  scale_y_continuous(breaks = 0:35, limits = c(0, 35))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment \nby Major Occupation Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates") +
  coord_fixed(ratio = 1)


region <- state_maj %>% select(year, maj_occ_code, bgt, tot_emp, region) %>%
  filter(is.na(region) == F & !(maj_occ_code == 55)) %>%
  group_by(year, maj_occ_code, region) %>%
  summarize(bgt = sum(bgt, na.rm = T), tot_emp = sum(tot_emp,na.rm = T))%>%
  group_by(year, region) %>%
  mutate(per_bgt = (bgt/sum(bgt)) * 100, 
         per_oes = (tot_emp/sum(tot_emp)) * 100)




ggplot(region[region$year == 2019 & region$region == "South", ], aes(x = per_bgt, y = per_oes, label = maj_occ_code)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:18, limits = c(0, 18)) +
  scale_y_continuous(breaks = 0:18, limits = c(0, 18))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment \nby Major Occupation Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates") +
  coord_fixed(ratio = 1)

ggplot(region[region$year == 2014 & region$maj_occ_code == 47, ], aes(x = per_bgt, y = per_oes, label = region)) +
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(breaks = 0:18, limits = c(0, 18)) +
  scale_y_continuous(breaks = 0:18, limits = c(0, 18))+
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  labs(x = "Percent of BGT Ads", y = "Percent of OES Total Employed", 
       title = "Comparison of BGT Job Ads and OES Employment \nby Major Occupation Codes", caption = "BGT: 2019 Job Ads; OES: May 2019 National Estimates") +
  coord_fixed(ratio = 1)

# national by Soc Code



