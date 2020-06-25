This paper is on data profiling BGT

    #sarah's %completeness function
    completeness <- function(x){
      (length(x) - sum(is.na(x))) / length(x) 
    }

    #My uniqueness function
    getUni <- function(col){
      return(sum(!is.na(unique(col))))  
    }

    #id
    #Assuming that a valid jobid is between 6 12 characters long
    validId <- function(col){
      return(sum(between(nchar(col),6,12) | is.na(col)) / length(col)) 
    }

    #Validate jobdate by looking at the year (maybe go back to)
    validDate <- function(column, yr){
      return(sum(str_detect(column, as.character(yr)) | is.na(column)) / length(column)) 
    }

    #Validate state
    #c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau")


    other_places = c("District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau", "Marshall Islands", "Federated States of Micronesia")
    validState <- function(col){
      return(sum(col%in%state.name | col%in%other_places) / length(col))  
    }

    #Validate soc
    #Looks like soc is two digits followed by a dash followed by 4 digits
    validSoc <- function(col){
      return(sum(str_detect(col, "\\d+-\\d\\d\\d\\d") | is.na(col)) / length(col))
    }


    #validate socname
    #For this variable I am just going to see if any occupation names have any digits in them. If they do, they won't be counted as valid
    validSocname <- function(col){
      return(sum(!str_detect(col, "[:digit:]") | is.na(col)) / length(col)) 
    }


    #Validate lat
    #Since the US is in lat range 0 to 90 (Northern Hemisphere) I looked at the number of ranges between 0 and 90
    validLat <- function(col){
      return(sum(col >= 0 & col <= 90 | is.na(col)) / length(col))
    }

    #prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[,j])] < 0 |( 133 < tbl[, j][!is.na(tbl[,j])] & tbl[, j][!is.na(tbl[,j])] < 172)) + sum(is.na(tbl[,j])))/length(tbl[,j])


    #Validate lon
    #Since the US is West of the prime meridian, valid longitude values will be in the negatives (or between 133 and 172)

    validLong <- function(col){
      return(sum(sign(col) == -1 | is.na(col) | between(col, 132, 173)) / length(col))
    }

    #validate minedu
    #Does 0 mean there are no education requirements? #add zero
    #12,14,16,18,and 21 were used to represent edu for all of the years. 2018 and 2019 had 0 as an entry, however, so I did not count these as valid
    valid <- c(0, 12, 14, 16, 18, 21)
    validMinEdu <- function(col){
      return(sum(col%in%valid | is.na(col)) / length(col))
    }

    #validate maxedu
    #12,14,16,18,and 21 were used to represent edu for all of the years. 2018 and 2019 had 0 as an entry, however, so I did not count these as valid
    valid <- c(0, 12, 14, 16, 18, 21)
    validMaxEdu <- function(col){
      return(sum(col%in%valid | is.na(col)) / length(col)) 
    }

    year <- 2010:2019 
    col_names = c("id","jobdate", "state", "soc", "socname", "lat", "lon", "minedu", "maxedu")

    for(i in year){
      #create df validity, completeness, and uniqueness
      prof <<- data.frame(variable = col_names, 
                          completeness = numeric(length = length(col_names)),
                          validity = numeric(length = length(col_names)), 
                          uniqueness = numeric(length = length(col_names)))
      
      
      for(col in col_names){
        tbl <- RPostgreSQL::dbGetQuery(
          conn = conn, 
          statement = paste("SELECT ", col, " FROM bgt_job.jolts_comparison_", i, sep = ""))
        
        prof[prof$variable == col, "completeness"] <- completeness(tbl[, col])
        prof[prof$variable == col, "uniqueness"] <- getUni(tbl[, col])
        
        #testing jobdate validity
        if(col == "jobdate"){
          prof[prof$variable == col, "validity"] <- validDate(tbl[, col], i)
        }
        
        #Testing for state
        if(col == "id"){
          prof[prof$variable == col, "validity"] <- validId(tbl[, col])
        }
        
        if(col == "state"){
          prof[prof$variable == col, "validity"] <- validState(tbl[, col])
        }
        
        if(col == "soc"){
          prof[prof$variable == col, "validity"] <- validSoc(tbl[, col]) 
        }
        
        if(col == "socname"){
          prof[prof$variable == col, "validity"] <- validSocname(tbl[, col]) 
        }
        
        if(col == "lat"){
          prof[prof$variable == col, "validity"] <- validLat(tbl[, col])
        }
        
        if(col == "lon"){
          prof[prof$variable == col, "validity"] <- validLong(tbl[, col])
        }
        
        if(col == "minedu"){
          prof[prof$variable == col, "validity"] <- validMinEdu(tbl[, col])
        }
        
        if(col == "maxedu"){
          prof[prof$variable == col, "validity"] <- validMaxEdu(tbl[, col])
        }
      } 
      assign(paste("prof", i, sep = ""), prof)
      
      
      
    }

2010

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">0.9999999</td>
<td align="right">11687110</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">361</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">57</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9760239</td>
<td align="right">1.0000000</td>
<td align="right">829</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9760239</td>
<td align="right">1.0000000</td>
<td align="right">829</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9728879</td>
<td align="right">1.0000000</td>
<td align="right">26580</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9728879</td>
<td align="right">1.0000000</td>
<td align="right">27906</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4418033</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1473610</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2011
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">0.9968022</td>
<td align="right">14184613</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">362</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">55</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9761293</td>
<td align="right">1.0000000</td>
<td align="right">831</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9761293</td>
<td align="right">1.0000000</td>
<td align="right">831</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9801127</td>
<td align="right">1.0000000</td>
<td align="right">25109</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9801127</td>
<td align="right">1.0000000</td>
<td align="right">26402</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4911761</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1622305</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2012
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">14090686</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">366</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">57</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9745240</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9745240</td>
<td align="right">1</td>
<td align="right">831</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9826769</td>
<td align="right">1</td>
<td align="right">29499</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9826769</td>
<td align="right">1</td>
<td align="right">31360</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4997426</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1670383</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2013
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">18072049</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9707383</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9707383</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9779461</td>
<td align="right">1</td>
<td align="right">32801</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9779461</td>
<td align="right">1</td>
<td align="right">34443</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4677514</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1565640</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2014
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">0.9999941</td>
<td align="right">19026414</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1.0000000</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9724461</td>
<td align="right">1.0000000</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9724461</td>
<td align="right">1.0000000</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9801636</td>
<td align="right">1.0000000</td>
<td align="right">34392</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9801636</td>
<td align="right">1.0000000</td>
<td align="right">36237</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.4848017</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1626419</td>
<td align="right">1.0000000</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2015
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">20828263</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">59</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9746747</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9746747</td>
<td align="right">1</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9918737</td>
<td align="right">1</td>
<td align="right">36649</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9918737</td>
<td align="right">1</td>
<td align="right">38749</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5145077</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1734663</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2016
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">23500839</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">366</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9736071</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9736071</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9926837</td>
<td align="right">1</td>
<td align="right">38235</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9926837</td>
<td align="right">1</td>
<td align="right">40430</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5001087</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1661879</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2017
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">22259806</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9742542</td>
<td align="right">1</td>
<td align="right">834</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9742542</td>
<td align="right">1</td>
<td align="right">834</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9918083</td>
<td align="right">1</td>
<td align="right">37936</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9918083</td>
<td align="right">1</td>
<td align="right">40190</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.5286905</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.1756633</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

2018
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">28769736</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">1.0000000</td>
<td align="right">1</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9716869</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9716869</td>
<td align="right">1</td>
<td align="right">832</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9820593</td>
<td align="right">1</td>
<td align="right">38558</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9820593</td>
<td align="right">1</td>
<td align="right">40857</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.8960728</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.8236399</td>
<td align="right">1</td>
<td align="right">6</td>
</tr>
</tbody>
</table>

2019
<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">completeness</th>
<th align="right">validity</th>
<th align="right">uniqueness</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">id</td>
<td align="right">1.0000000</td>
<td align="right">1.000000</td>
<td align="right">33452673</td>
</tr>
<tr class="even">
<td align="left">jobdate</td>
<td align="right">1.0000000</td>
<td align="right">1.000000</td>
<td align="right">365</td>
</tr>
<tr class="odd">
<td align="left">state</td>
<td align="right">0.9999690</td>
<td align="right">0.999969</td>
<td align="right">58</td>
</tr>
<tr class="even">
<td align="left">soc</td>
<td align="right">0.9711764</td>
<td align="right">1.000000</td>
<td align="right">833</td>
</tr>
<tr class="odd">
<td align="left">socname</td>
<td align="right">0.9711764</td>
<td align="right">1.000000</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="left">lat</td>
<td align="right">0.9910704</td>
<td align="right">1.000000</td>
<td align="right">38150</td>
</tr>
<tr class="odd">
<td align="left">lon</td>
<td align="right">0.9910704</td>
<td align="right">1.000000</td>
<td align="right">40377</td>
</tr>
<tr class="even">
<td align="left">minedu</td>
<td align="right">0.9994033</td>
<td align="right">1.000000</td>
<td align="right">6</td>
</tr>
<tr class="odd">
<td align="left">maxedu</td>
<td align="right">0.9999813</td>
<td align="right">1.000000</td>
<td align="right">6</td>
</tr>
</tbody>
</table>
