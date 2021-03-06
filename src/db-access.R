#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory


conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

#---------------- Example Table -----------------------#

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010 LIMIT 30;")
