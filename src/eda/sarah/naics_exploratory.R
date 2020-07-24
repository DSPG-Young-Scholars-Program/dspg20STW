library(dplyr)
data <- read.table("data/ncses_stw/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)

# to acquire industry tables
table <- read.table("data/ncses_stw/original/jt.industry.txt", 
                    sep ="\t", 
                    header = T, 
                    colClasses = c("industry_code" = "character"))

industry <- data %>% filter(grepl(pattern = "JTU.+\\d{2}JOL", x = series_id))%>%
              mutate(value = value * 1000, 
                     period = substr(period, start = 2, stop = 3), 
                     industry_code = substr(series_id, start = 4, stop = 9), 
                     industry_name = table$industry_text[match(industry_code, table$industry_code)])


conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

y = 2019
tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = paste("SELECT COUNT(DISTINCT A.id), B.naics
                    FROM bgt_job.jolts_comparison_2019 A
                    LEFT JOIN bgt_job.job_main B", 
                    " WHERE A.state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  sep = ""))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = paste("SELECT *
                    FROM bgt_job.job_main"))


