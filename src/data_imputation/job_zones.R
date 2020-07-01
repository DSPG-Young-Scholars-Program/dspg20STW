library(readxl)
library(dplyr)
#download.file("https://www.bls.gov/emp/classifications-crosswalks/nem-onet-to-soc-crosswalk.xlsx",
 # destfile = "data/original/nem-onet-to-soc-crosswalk.xlsx")

# soc-onet crosswalk
xwalk <- read_excel("data/original/nem-onet-to-soc-crosswalk.xlsx", skip = 4)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

zones <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM onet.job_zones")

# retrieves onet code
bgt <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT A.id, A.minedu, A.soc, B.onet
  FROM bgt_job.jolts_comparison_2010 A
  JOIN bgt_job.main B
  ON A.id = B.id")

bgt$job_zone<- zones$job_zone[match(bgt$onet, zones$onetsoc_code)]

nonMatch <- bgt%>%filter(is.na(onet) == FALSE & is.na(job_zone)==TRUE) %>% select(onet,soc) %>% unique()

# share of bgt jobs that match to job zone and have minedu
nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE))/nrow(bgt)


# share of bgtjobs that have minedu but not job zone
nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==FALSE))/nrow(bgt)


# share of bggt jobs that have job zone but not minedu
nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==TRUE))/nrow(bgt)


# double NAS
nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==TRUE))/nrow(bgt)


tbl <- bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE) %>% 
  select(job_zone, minedu, id) %>%
  group_by(job_zone, minedu) %>%
  summarise(n = n())

  

