
data<- data.frame(Variable =c("id","jobdate", "state","soc", "socname", "lat", "long", "minedu", "maxedu"), 
           `Validity` = c("the observation is unique, i.e. does not appear more than once",
                          "the observation is in the format YYYY-MM-DD and contained within the range of January 1st for December 31st of the specified year",
                          "the observation is one of the 50 states, the District of Columbia, or a U.S. territory",
                          "the observation has seven character",
                          "the observation is not a numeric string",
                          "the observation is greater than zero",
                          "the observation is less than zero",
                          "the observation is either 0, 12, 14, 16, 18, or 21", 
                          "the observation is either 0, 12, 14, 16, 18, or 21"))
View(data)
write.csv(data, "src/shiny-dashboard/stwFluid/validity_table.csv", row.names = F)
