
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM onet.job_zones")

