job_zones <- function(years){
  
  library(dplyr)
  library(tidyr)
  
  conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                                 dbname = "sdad",
                                 host = "postgis1",
                                 port = 5432,
                                 user = Sys.getenv(x = "DB_USR"),
                                 password = Sys.getenv(x = "DB_PWD"))
  
  zones <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = "SELECT * FROM onet.job_zones")
  
  share <- data.frame("Year" = years, 
    "Job Zone and Minedu" = rep(0, length(years)),
    "Minedu Only" = rep(0, length(years)),
    "Job Zone Only" = rep(0, length(years)),
    "Neither Job Zone nor Minedu" = rep(0, length(years)))
  
  counts <- data.frame("job_zone" = character(0), 
                       "minedu" = numeric(0), 
                       "Count.of.IDs" = numeric(0), 
                       "year" = numeric(0))
  
  for (y in years){
    

    # retrieves onet code
    bgt <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT A.id, A.minedu, A.soc, B.onet
      FROM bgt_job.jolts_comparison_", y, " A
      JOIN bgt_job.main B
      ON A.id = B.id", sep = ""))
    
    bgt$job_zone<- zones$job_zone[match(bgt$onet, zones$onetsoc_code)]

    nonMatch <- bgt%>%filter(is.na(onet) == FALSE & is.na(job_zone)==TRUE) %>% select(onet,soc) %>% unique()

    share[share$Year == y, "Job.Zone.and.Minedu"] <-nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE))/nrow(bgt)
    share[share$Year == y, "Minedu.Only"] <-nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==FALSE))/nrow(bgt)
    share[share$Year == y, "Job.Zone.Only"] <-nrow(bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==TRUE))/nrow(bgt)
    share[share$Year == y, "Neither.Job.Zone.nor.Minedu"] <-nrow(bgt %>% filter(is.na(job_zone)==TRUE & is.na(minedu)==TRUE))/nrow(bgt)



  tbl <- bgt %>% filter(is.na(job_zone)==FALSE & is.na(minedu)==FALSE) %>% 
    select(job_zone, minedu, id) %>%
    group_by(job_zone, minedu) %>%
    summarise(Count.of.IDs = n()) %>%
    mutate(year = y)

  counts <- rbind(counts, tbl)

  }
  
counts <<- counts
counts_wide <<- spread(counts, year, Count.of.IDs)
share <<- share
}  



job_zones(2010:2019)
