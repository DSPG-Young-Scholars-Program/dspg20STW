library(tidyr)
library(dplyr)
library(readxl)


#----------------------- State Estimates----------------------------#
#TOTAL NONFARM, February 2001-September 2019
#COMPOSITE SYNTHETIC ESTIMATES (February 2001-December 2018)
#EXTENDED COMPOSITE SYNTHETIC ESTIMATES (January 2019-September 2019)

#download.file(url = "https://www.bls.gov/jlt/jlt_statedata_q3_2019.xlsx", destfile = "data/original/jlt_statedata_q3_2019.xlsx")

state <- read_xlsx("data/original/jlt_statedata_q3_2019.xlsx", skip = 4)

# to adjust from "level in thousands" to actual thousands

state <- state %>% mutate(`Job Openings` = `Job Openings` * 1000, 
                 Hires = Hires * 1000, 
                 Quits = Quits * 1000, 
                 `Layoffs & Discharges` = `Layoffs & Discharges` * 1000, 
                 `Total Separations` = `Total Separations` * 1000)


#---------------------- national tables-----------------------------------------------#

#download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.data.2.JobOpenings", destfile = "data/original/jt.data.2.JobOpenings.txt")

data <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)




# to acquire region tables, U = not seasonally adjusted, L = in thousands, JO = job openings, only available at total non farm level
region <- data %>% filter(grepl(pattern = "JTU.+\\D{2}JOL|JTU00000000JOL", x = series_id))


# to acquire industry tables
industry <- data %>% filter(grepl(pattern = "JTU.+\\d{2}JOL", x = series_id))


#-----------industry codes table-------------------------------#

#from: https://download.bls.gov/pub/time.series/jt/jt.industry

download.file("https://download.bls.gov/pub/time.series/jt/jt.industry", "data/original/jt.industry.txt")


table <- read.table("data/original/jt.industry.txt", 
                    sep ="\t", 
                    header = T, 
                    colClasses = c("industry_code" = "character"))


# get industry names for the industry table
industry$industry_name <- table$industry_text[match(substr(industry$series_id, start = 4, stop = 9), table$industry_code)]


# state by region
states <- data.frame(state.name, state.region)
states <- rbind(states, c("District of Columbia", "South"))


