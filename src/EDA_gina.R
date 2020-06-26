conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

df <- data.frame(year = 2010:2019, job_ads = numeric(length = length(2010:2019)))


for (y in 2010:2019){
  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn,
    statement = paste("SELECT id FROM bgt_job.jolts_comparison_", y, " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), 
                                                                                                                     type="sh"), collapse=", "), ")", sep = ""),  sep = ""))
  
  df[df$year == y, "job_ads"] <- sum(!is.na(unique(tbl$id)))
  
}

library(ggplot2)
ggplot(data=df, aes(x=year, y=job_ads)) + geom_line()
