# job ad | soc | onet | job zone| minedu |cip |S Major| PER HS

library(readxl)
library(dplyr)
xwalk <- read_xlsx("data/ONETSOC_SOC_OCC_NCSES_JUL2020.xlsx")

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

bgt <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT DISTINCT A.id, A.soc, A.minedu, B.onet, 
  CASE WHEN C.cip IS NULL THEN FALSE ELSE TRUE END AS hascip,
  CASE WHEN D.stdmajor IS NULL THEN FALSE ELSE TRUE END AS hasstdmajor
      
  FROM bgt_job.jolts_comparison_2019 A
      LEFT JOIN bgt_job.main B ON A.id = B.id 
      LEFT JOIN bgt_job.cip C ON A.id = C.id 
      LEFT JOIN bgt_job.maj D ON A.id = D.id 
  ")

minedu_na <- bgt %>% filter(is.na(minedu) == TRUE)

sum(minedu_na$hasstdmajor)/nrow(minedu_na)
sum(minedu_na$hascip)/nrow(minedu_na)






