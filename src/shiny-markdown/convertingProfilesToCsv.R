prof2010 <<- data.frame(variable = col_names, 
                    completeness = c(1.0,1.0,1.0,.9760239,.9760239,.9728879,.9728879,.4418033,.1473610),
                    validity = c(1,1,1,1,1,1,1,1,1), 
                    uniqueness = c(11687110,361,57,829,829,26580,27906,5,5))

write.csv(prof2010,"src/shiny-markdown/2010.csv", row.names = FALSE) 

prof2011 <<- data.frame(variable = col_names, 
                        completeness = c(1.0,1.0,1.0,.9761293,.9761293,.9801127,.9801127,.4911761,.1622305),
                        validity = c(1,1,1,1,1,1,1,1,1), 
                        uniqueness = c(14184613,362,55,831,831,25109,26402,5,5))

write.csv(prof2011,"src/shiny-markdown/2011.csv", row.names = FALSE)

prof2012 <<- data.frame(variable = col_names, 
                        completeness = c(1.0,1.0,1.0,.9745240,.9745240,.9826769,.9826769,.4997426,.1670383),
                        validity = c(1,1,1,1,1,1,1,1,1), 
                        uniqueness = c(14090686,366,57,831,831,29499,31360,5,5))

write.csv(prof2012, "src/shiny-markdown/2012.csv", row.names = FALSE)