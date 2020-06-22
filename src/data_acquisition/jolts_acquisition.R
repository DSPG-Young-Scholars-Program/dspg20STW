#----------------------- State Estimates----------------------------#
#TOTAL NONFARM, February 2001-September 2019
#COMPOSITE SYNTHETIC ESTIMATES (February 2001-December 2018)
#EXTENDED COMPOSITE SYNTHETIC ESTIMATES (January 2019-September 2019)

library(readxl)
download.file(url = "https://www.bls.gov/jlt/jlt_statedata_q3_2019.xlsx", destfile = "data/original/jlt_statedata_q3_2019.xlsx")

data <- read_xlsx("data/original/jlt_statedata_q3_2019.xlsx", skip = 4)
View(data)


#---------------------- national tables-----------------------------------------------#

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.data.2.JobOpenings", 
              destfile = "data/original/jt.data.2.JobOpenings.txt")

data <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)

library(tidyr)
library(dplyr)


# to acquire region tables, U = not seasonally adjusted, L = in thousands, JO = job openings, only available at total non farm level
data %>% filter(grepl(pattern = "JTU.+\\D{2}JOL|JTU00000000JOL", x = series_id))


# to acquire industry tables
data %>% filter(grepl(pattern = "JTU.+\\d{2}JOL", x = series_id))


#-----------industry codes table-------------------------------#

#from: https://download.bls.gov/pub/time.series/jt/jt.industry

download.file("https://download.bls.gov/pub/time.series/jt/jt.industry", "data/original/jt.industry.txt")


table <- read.table("data/original/jt.industry.txt", 
                    sep ="\t", 
                    header = T, 
                    colClasses = c("industry_code" = "character"))








