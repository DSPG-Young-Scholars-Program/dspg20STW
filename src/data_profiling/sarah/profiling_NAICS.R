conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))


completeness <- function(x){
  sum(!is.na(x))/length(x)
}

uniqueness <- function(x){
  sum(!is.na(unique(x)))
}




for(year in 2010:2019){
    prof <- data.frame(variable = c("sector"), 
                      completeness = numeric(length = length(1)),
                      validity = numeric(length = length(1)), 
                      uniqueness = numeric(length = length(1)))

    
    tbl  <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT B.sector
  FROM bgt_job.jolts_comparison_", year, " A
  JOIN bgt_job.main B
  ON A.id = B.id
  WHERE A.state IN" , paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""), sep = ""))
    
    
    j = "sector"
    
    prof[prof$variable == j, "completeness"] <- completeness(tbl[, j])
    
    prof[prof$variable == j, "uniqueness"] <- uniqueness(tbl[, j])
    
    prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[, j])] %in% c("55",    "44-45", "62",    "72",    "23",    "54",    "31-33", "52",    "53",    "81",    "56",    "61",    "48-49",
                                           "51",    "92",    "22",    "42",    "11",    "21",    "71" )) + sum(is.na(tbl[, j])))/length(tbl[, j])
    
  
     
      
  assign(paste("sector", year, sep = ""), prof, envir = .GlobalEnv)  
}
