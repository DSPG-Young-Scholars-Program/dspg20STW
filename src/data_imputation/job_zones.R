library(readxl)
#download.file("https://www.bls.gov/emp/classifications-crosswalks/nem-onet-to-soc-crosswalk.xlsx",
 #             destfile = "data/original/nem-onet-to-soc-crosswalk.xlsx")
xwalk <- read_excel("data/original/nem-onet-to-soc-crosswalk.xlsx", skip = 4)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM onet.job_zones")

  
bgt <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT id, soc, minedu, maxedu FROM bgt_job.jolts_comparison_2010",   sep = ""))

bgt$job_zones <- tbl$job_zone[match(bgt$soc, substr(tbl$onetsoc_code, start = 1, stop = 7))]



# share of non missing socs that match to a job zone
length(bgt[is.na(bgt$soc)== FALSE & is.na(bgt$job_zones) == FALSE, "id"])/length(bgt[is.na(bgt$soc )== FALSE, "id"])

# failed matches
fail <- unique(bgt[is.na(bgt$soc) == FALSE & is.na(bgt$job_zones) == TRUE, "soc"])

# are failed in SOC code cross walk?
table(fail %in% xwalk$`SOC Code`)

# are SOC code cross walk in bgt data


xsoc <- unique(xwalk$`SOC Code`)

table(xsoc %in% substr(tbl$onetsoc_code, start = 1, stop = 7))

xsoc_fail <- xsoc[!(xsoc %in% substr(tbl$onetsoc_code, start = 1, stop = 7))]

fail[!(fail %in% xsoc_fail)]

  
